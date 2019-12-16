/**
 * npm i -g jscodeshift
 * jscodeshift -t transform.js elm.js
 * https://astexplorer.net/
 */
const glslx = require('glslx').compile;

module.exports = function (file, api, options) {
    const j = api.jscodeshift;
    const airity = new Map(); // Map(functionName, airity)
    const tree = j(file.source);

    // Build the initial airity map
    tree.find(j.VariableDeclarator).forEach(path => {
        if (
            path.node.id.type === "Identifier" &&
            path.node.init &&
            path.node.init.type === "CallExpression" &&
            path.node.init.callee.type === "Identifier" &&
            path.node.init.callee.name.match(/^F\d$/)
        ) {
            airity.set(
                path.node.id.name,
                Number(path.node.init.callee.name.substr(1))
            );
        }
    });

    // Add re-declarations of the existing functions
    tree.find(j.VariableDeclarator).forEach(path => {
        if (
            path.node.id.type === "Identifier" &&
            path.node.init &&
            path.node.init.type === "Identifier" &&
            airity.has(path.node.init.name)
        ) {
            airity.set(path.node.id.name, airity.get(path.node.init.name));
        }
    });

    //Optimize glsl string
    tree.find(j.Literal)
        .filter((path) => path.value && path.value.value && path.value.value.length > 100)
        .replaceWith(nodePath => {
            const { node } = nodePath;

            const newValue = glslx(node.value, { renaming: 'none', format: "json", });
            if (newValue.log === "") {
                node.value = JSON.parse(newValue.output).shaders[0].contents;
            }

            return node;
        });


    //===============================PREPACK MAGIC Start ===============================
    // Fix Prepack "FatalError PP0001: This operation is not yet supported on document at hidden (https://github.com/facebook/prepack/wiki/PP0001)"
    tree.find(j.FunctionDeclaration)
        .filter((path) => (path.node.id.name) === "_Browser_visibilityInfo")
        .replaceWith(({ node }) => {
            node.body = "{return { b4: 'hidden', b0: 'visibilitychange' }}";
            return node
        });

    //Add Prepack __optimize
    const entryPoint = tree.find(j.VariableDeclarator)
        .filter(path => path.node.id.name === "$author$project$Main$main")
        .at(0).get().node.init.name;

    const appParts = tree.find(j.VariableDeclarator)
        .filter(path => path.node.id.name === entryPoint)
        .at(0).get().node.init.arguments.map((a) => a.name);

    tree.find(j.ExpressionStatement)
        .filter(path => path.node.expression.callee && path.node.expression.callee.name === "_Platform_export")
        .replaceWith((path) => {
            // console.log(path.node.expression.arguments[0].properties[0].value.properties[0].value.loc);
            // path.insertBefore(`__optimize(${path.node.expression.arguments[0].properties[0].value.properties[0].value.callee.callee.name}($elm$json$Json$Decode$succeed(0)));`)
            path.insertBefore(`__optimize(${appParts[0]});`);
            path.insertBefore(`__optimize(${appParts[1]});`);
            path.insertBefore(`__optimize(${appParts[2]});`);
            path.insertBefore(`__optimize($author$project$Main$main($elm$json$Json$Decode$succeed(0)));`);


            return (path.node);
        });
    //===============================PREPACK MAGIC End ===============================


    // Transform the A1..n calls
    tree.find(j.CallExpression)
        .forEach(path => {
            if (
                path.node.callee.type === "Identifier" &&
                path.node.callee.name.match(/^A\d$/) &&
                path.node.arguments.length > 1 &&
                path.node.arguments[0].type === "Identifier" &&
                airity.get(path.node.arguments[0].name) ===
                path.node.arguments.length - 1 &&
                airity.get(path.node.arguments[0].name) ===
                Number(path.node.callee.name.substr(1))
            ) {
                path.node.callee = {
                    type: "MemberExpression",
                    object: {
                        type: "Identifier",
                        name: path.node.arguments[0].name
                    },
                    property: {
                        type: "Identifier",
                        name: "f"
                    }
                };
                path.node.arguments.shift();
            }
        });
    return tree.toSource();
};

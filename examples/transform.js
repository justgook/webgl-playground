/**
 * npm i -g jscodeshift
 * jscodeshift -t transform.js elm.js
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
    // Fix Prepack "FatalError PP0001: This operation is not yet supported on document at hidden (https://github.com/facebook/prepack/wiki/PP0001)"
    tree.find(j.FunctionDeclaration)
        .filter((path) => (path.node.id.name) === "_Browser_visibilityInfo")
        .replaceWith(nodePath => {
            const { node } = nodePath;

            node.body = "{return { b4: 'hidden', b0: 'visibilitychange' }}";
            return node
        });

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

const { exec } = require("child_process");
const core = require('@actions/core');


const cmd = (aaa) => exec(aaa, (err, stdout, stderr) => {
    if (err) {
        // node couldn't execute the command
        return -1;
    }

    // the *entire* stdout and stderr (buffered)
    console.log(`stdout: ${stdout}`);
    console.log(`stderr: ${stderr}`);
});

const inputFile = core.getInput('elm-version');

// cmd(`jscodeshift -t transform.js ${inputFile}`);
cmd(`uglifyjs ${inputFile} --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters" --output=${inputFile}`);
// cmd(`prepack ${inputFile} --inlineExpressions --out ${inputFile} --maxStackDepth  10000`);
// cmd(`uglifyjs ${inputFile} --compress 'keep_fargs=false,unsafe_comps,unsafe' --mangle --output=${inputFile}`);

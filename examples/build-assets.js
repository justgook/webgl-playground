const http = require("http");
const fs = require("fs");
const Pageres = require('pageres');
const port = 3000;

const root = "gh-pages";
const examples = ["Clock", "Polygon", "Polygon2", /*"Tree",*/ "Mouse", "Vectors", "Shmup", "Circles", "Font", "MSDF", "JumpGun", "Mario", "HexGrid", "Main"];
const server = http.createServer((req, res) => {
    // `${process.env.GAME}_bundle.js`;
    if (req.url === "/") {
        req.url = `/index.html`;
    }

    const path = __dirname + `/${root}` + req.url;
    fs.access(path, fs.F_OK, (err) => {
        if (err) {
            console.error(err);
            res.writeHead(404);
            res.end();
            process.exit(-1);
            return;
        }
        res.writeHead(200,
            path.endsWith(".mjs")
                ? { "Content-Type": "application/javascript; charset=utf-8" }
                : {}
        );
        res.end(fs.readFileSync(path));
    })

});

server.listen(port, () => {
    console.log("server started");
    screenshot(() => server.close());
});


function screenshot(done) {
    stepScreenshot(examples.reverse())
        .catch(() => process.exit(-1))
        .then(done);
}

function stepScreenshot(input) {
    const url = `http://localhost:${port}/`;
    const item = input.pop();
    console.log(`Screenshot for ${item} (${[...input].reverse().join(", ")})`);
    return new Pageres({ filename: item, timeout: 120 })
        .src(`${url}${item}.html`, ['800x600'])
        .dest(`${__dirname}/gh-pages`)
        .run()
        .catch((err) => {
            console.log(`Fail ${item} (${err})`);
            process.exit(-1)
        })
        .then(() => input.length
            ? stepScreenshot(input)
            : Promise.resolve()
        )

}


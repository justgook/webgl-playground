const http = require("http");
const fs = require("fs");
const port = 3000;

const root = "gh-pages";
const examples = [
    "Shmup",
    "Tree",
    "Clock",
    "Polygon",
    "Polygon2",
    "Mouse",
    "Vectors",
    "Circles",
    "Font",
    "MSDF",
    "JumpGun",
    "Mario",
    "HexGrid",
    "Main"
];
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

async function stepScreenshot(input) {
    const puppeteer = require('puppeteer');


    // 1. Launch the browser
    const browser = await puppeteer.launch();
    // 2. Open a new page
    const page = await browser.newPage();
    await stepStepSTEP(page, input);

    await browser.close();

}

async function stepStepSTEP(page, input) {
    const url = `http://localhost:${port}/`;
    const item = input.pop();
    console.log(`Screenshot for ${item} (${[...input].reverse().join(", ")})`);

    // 3. Navigate to URL
    await page.goto(`${url}${item}.html`);
    // 4. Take screenshot
    await page.screenshot({ omitBackground: true, path: `${__dirname}/gh-pages/${item}.png` });
    if (input.length) {
        await stepStepSTEP(page, input);
    }

}

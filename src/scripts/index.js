#!/usr/bin/env node

if (process.argv.length !== 4)
    throw new Error("USAGE: ./index.js CACHE_DIR OUTPUT_APP");

const [, , cacheDirectory, outputApp] = process.argv;

XMLHttpRequest = require("xhr2");

import * as fs from "fs";
import * as path from "path";

import {Elm} from "./Setup.elm";

const app = Elm.Setup.init();

function pathInCache(moduleName) {
    return path.join(cacheDirectory, moduleName + ".elm");
}

function createFile(filePath, data) {
    fs.writeFile(filePath, data, "utf8", err => {
        if (err) throw new Error(err);
        else console.log(`>> created ${filePath}`);
    });
}

app.ports.lookup.subscribe(msg => {
    fs.access(pathInCache(msg.moduleName), err => {
        if (err) app.ports.onMissing.send(msg.metadata);
    });
});

app.ports.put.subscribe(msg => {
    createFile(pathInCache(msg.moduleName), msg.code);
});

app.ports.writeOutput.subscribe(code => createFile(outputApp, code));

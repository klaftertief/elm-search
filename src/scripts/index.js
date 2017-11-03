#!/usr/bin/env node

if (process.argv.length !== 5)
    throw new Error('USAGE: ./index.js COMPILED_SETUP_APP CACHE_DIR OUTPUT_APP');

const [,, compiledSetupApp, cacheDirectory, outputApp] = process.argv;

XMLHttpRequest = require('xhr2');
const fs = require('fs');
const path = require('path');
const compiledElm = path.relative(__dirname, compiledSetupApp);
const app = require(compiledElm).Setup.worker();

function pathInCache(moduleName) {
    return path.join(cacheDirectory, moduleName + '.elm');
}

function createFile(filePath, data) {
    fs.writeFile(filePath, data, 'utf8', err => {
        if (err)
            throw new Error(err);
        else
            console.log(`>> created ${filePath}`);
    });
}

app.ports.lookup.subscribe(msg => {
    fs.access(pathInCache(msg.moduleName), (err) => {
        if (err)
            app.ports.onMissing.send(msg.identifier);
    });
});

app.ports.put.subscribe(msg => {
    createFile(pathInCache(msg.moduleName), msg.code);
});

app.ports.writeOutput.subscribe(code => createFile(outputApp, code));

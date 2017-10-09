#!/usr/bin/env node

if (process.argv.length !== 5)
    throw new Error('USAGE: ./index.js COMPILED_SETUP_APP CACHE_DIR OUTPUT_APP');

const [,, compiledSetupApp, cacheDirectory, outputApp] = process.argv;

XMLHttpRequest = require('xhr2');
const fs = require('fs');
const path = require('path');
const Elm = require(compiledSetupApp);
const app = Elm.Setup.worker();

function pathInCache(package) {
    const baseName = `${package.user}@${package.name}@${package.version}`;
    return path.join(cacheDirectory, baseName + '.json');
}

function checkForFile(package) {
    fs.readFile(pathInCache(package), 'utf8', (err, data) => {
        if (err)
            app.ports.inbox.send({ tag: 'CACHE_MISS', package: package });
        else
            app.ports.inbox.send({
                tag: 'CACHE_HIT',
                package: Object.assign({}, package, {
                    modules: JSON.parse(data),
                }),
            });
    });
}

function createFile(filePath, data) {
    fs.writeFile(filePath, data, 'utf8', err => {
        if (err)
            throw new Error(err);
        else
            console.log(`>> created ${filePath}`);
    });
}

app.ports.outbox.subscribe(action => {
    switch (action.tag) {
        case 'CHECK_CACHE':
            checkForFile(action.package);
            break;
        case 'CACHE':
            createFile(
                pathInCache(action.package),
                JSON.stringify(action.package.modules)
            );
            break;
        case 'DONE':
            createFile(outputApp, action.code);
            break;
    }
});

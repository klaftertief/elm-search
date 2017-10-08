#!/usr/bin/env node

if (process.argv.length !== 5)
    throw new Error('USAGE: ./index.js COMPILED_SETUP_APP CACHE_DIR MAIN_APP_OUTPUT_PATH');

const compiledSetupApp = process.argv[2];
const cacheDirectory = process.argv[3];
const mainAppOutputPath = process.argv[4];

XMLHttpRequest = require('xhr2');
const fs = require('fs');
const path = require('path');
const Elm = require(compiledSetupApp);
const app = Elm.Setup.worker();

function pathInCache(package) {
    const baseName = `${package.user}@${package.name}@${package.version}`
    return path.join(cacheDirectory, baseName + '.json');
}

function checkCache(package) {
    fs.readFile(pathInCache(package), (err, data) => {
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
    fs.writeFile(filePath, data, err => {
        if (err)
            throw new Error(err);
        else
            console.log(`>> created ${filePath}`);
    });
}

app.ports.outbox.subscribe(action => {
    switch (action.tag) {
        case 'CHECK_CACHE':
            checkCache(action.package);
            break;
        case 'CACHE':
            createFile(
                pathInCache(action.package),
                JSON.stringify(action.package.modules)
            );
            break;
        case 'DONE':
            createFile(mainAppOutputPath, action.code);
            break;
    }
});

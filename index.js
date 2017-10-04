#!/usr/bin/env node

if (process.argv.length !== 4)
    throw new Error('USAGE: ./index.js COMPILED_SETUP_APP MAIN_APP_OUTPUT_PATH');

var compiledSetupApp = process.argv[2];
var mainAppOutputPath = process.argv[3];

XMLHttpRequest = require('xhr2');
var Elm = require(compiledSetupApp);

Elm.Setup.worker().ports.done.subscribe(contents => {
    require('fs').writeFileSync(mainAppOutputPath, contents);
    console.log(`Created ${mainAppOutputPath}!`);
});

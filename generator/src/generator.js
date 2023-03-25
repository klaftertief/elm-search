XMLHttpRequest = require('xhr2');

import * as fs from "fs-extra";
import * as path from "path";

import {Elm} from './Main.elm';

let app = Elm.Main.init();

app.ports.log.subscribe(console.log);

// app.ports.lookupCache.subscribe((identifier) => {
//     fs.access(pathInCache(identifier), err => {
//         if (err) {
//             app.ports.cacheMiss.send(identifier)
//         } else {
//             app.ports.cacheHit.send(identifier)
//         }
//     });
// });

app.ports.writeRawCache.subscribe(msg => {
    let [author, name, version] = msg.identifier;
    let prefix = path.join('cache', 'raw', author, name, version)
    createFile(path.join(prefix, "README.md"), msg.rawPackage.readme);
    createFile(path.join(prefix, "elm.json"), msg.rawPackage.project);
    createFile(path.join(prefix, "docs.json"), msg.rawPackage.docs);
});

app.ports.writeCache.subscribe(msg => {
    createFile(pathInCache(msg.identifier), msg.code);
});

app.ports.writeIndex.subscribe(msg => {
    createFile(path.join('cache', "Index.elm"), msg.code);
});

function pathInCache([author, name, version]) {
    return path.join('cache', ["Package", author, name, version.split(".").join("_")].join("__").split("-").join("_") + ".elm");
}

function createFile(filePath, data) {
    return fs.outputFile(filePath, data, "utf8", err => {
        if (err) throw new Error(err);
    });
}

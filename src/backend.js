const fs = require("fs");
const path = require("path");
const Backend = require("./Backend.elm").Elm.Backend;
const express = require("express");
const search = Backend.init();
const cors = require("cors");
const server = express();
const api = express();

server.use(cors());

const port = 3333;
const maxDuration = 1000;
const interval = 5;

const cache = {};

const allPackages = require("../packages/search.json").filter(package =>
  package.startsWith("elm/")
);

allPackages.forEach(package => {
  search.ports.addPackage.send({
    package: require(pathInCache(path.join(package, "elm.json"))),
    readme: require(pathInCache(path.join(package, "readme.json"))).readme,
    docs: require(pathInCache(path.join(package, "docs.json")))
  });
});

search.ports.response.subscribe(function(msg) {
  cache[msg.query || msg.url] = msg;
});

api.get("/*", function(req, res) {
  const url = `${req.protocol}://${req.hostname}${req.url}`;

  search.ports.request.send(url);
  let duration = 0;

  function trySend() {
    if (cache[url]) {
      const response = cache[url];
      delete cache[url];
      res.send(response);
    } else if (duration >= maxDuration) {
      res.sendStatus(408);
    } else {
      duration += interval;
      setTimeout(trySend, interval);
    }
  }

  trySend();
});

server.use("/api", api);

server.listen(port, () =>
  console.log(`Example app listening on port ${port}!`)
);

// DOWNLOAD

// const Download = require("./Download.elm").Elm.Download;
// XMLHttpRequest = require("xhr2");

// const download = Download.init();

// function createFile(filePath, data) {
//   fs.writeFile(filePath, data, "utf8", err => {
//     if (err) throw new Error(err);
//     // else console.log(`>> created ${filePath}`);
//   });
// }

// download.ports.writeFile.subscribe(data =>
//   createFile(pathInCache(data.path), data.content)
// );

function pathInCache(filePath) {
  return path.join("../packages", filePath.split("/").join("__"));
}

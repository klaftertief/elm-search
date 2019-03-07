const fs = require("fs");
const path = require("path");
const Backend = require("./Backend.elm").Elm.Backend;
const express = require("express");
const search = Backend.init();
const cors = require("cors");
const server = express();

server.use(cors());

const port = 3333;
const maxDuration = 1000;
const interval = 5;

const cache = {};

const allPackages = require("../packages/search.json");
allPackages.forEach(package => {
  search.ports.addPackage.send({
    package: require(pathInCache(path.join(package, "elm.json"))),
    readme: require(pathInCache(path.join(package, "readme.json"))).readme,
    docs: require(pathInCache(path.join(package, "docs.json")))
  });
});

search.ports.result.subscribe(function(msg) {
  cache[msg.query] = msg;
});

server.get("/", function(req, res) {
  res.send("Hello elm-search!");
});

server.get("/search", function(req, res) {
  const query = req.query.q || "";
  let duration = 0;

  search.ports.search.send(query);

  function trySend() {
    if (cache[query]) {
      const result = cache[query];
      delete cache[query];
      res.send(result);
    } else if (duration >= maxDuration) {
      res.sendStatus(408);
    } else {
      duration += interval;
      setTimeout(trySend, interval);
    }
  }

  trySend();
});

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

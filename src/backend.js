const fs = require("fs");
const path = require("path");
const Backend = require("./Backend.elm").Elm.Backend;
const express = require("express");
const cors = require("cors");
const { Client } = require("pg");

const port = process.env.PORT || 3333;
const maxDuration = 1000;
const search = Backend.init();
const server = express();
server.use(cors());
const api = express();
const client = new Client({
  connectionString:
    process.env.DATABASE_URL || "postgresql://localhost/elm-search"
});
client.connect();

const query = {
  text:
    'SELECT "id", "name", "info", "readme", "docs" FROM "public"."packages"  WHERE ("name" ILIKE \'elm/%\') ORDER BY "id" ASC LIMIT 10000'
};

client.query(query, (err, res) => {
  if (err) {
    console.log(err.stack);
  } else {
    res.rows.forEach(package => {
      console.log("got row", package.name);
      search.ports.addPackage.send({
        package: package.info,
        readme: package.readme,
        docs: package.docs
      });
    });
  }
});

const cache = {};

search.ports.response.subscribe(function(msg) {
  cache[msg.url] = msg;
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
      setTimeout(trySend);
    }
  }

  trySend();
});

server.use("/api", api);

server.listen(port, () =>
  console.log(`elm-search app listening on port ${port}!`)
);

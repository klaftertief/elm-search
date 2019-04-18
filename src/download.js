const Download = require("./Download.elm").Elm.Download;
const { Client } = require("pg");
XMLHttpRequest = require("xhr2");

const client = new Client({
  database: "elm-search"
});
client.connect();

const download = Download.init();

function addPackage(name, data) {
  const text =
    "INSERT INTO packages(name, info, readme, docs) VALUES($1, $2, $3, $4) RETURNING name";
  const values = [name, data.info, data.readme, data.docs];

  client.query(text, values, (err, res) => {
    if (err) {
      console.log(err.stack);
    } else {
      console.log(res.rows[0]);
    }
  });
}

const cache = {};

download.ports.writeFile.subscribe(data => {
  console.log("got", data.name, data.type);
  let inCache = cache[data.name] || {};
  cache[data.name] = inCache;

  inCache[data.type] = data.value;
  console.log(Object.keys(inCache));

  if (inCache.info && inCache.readme && inCache.docs) {
    console.log("writing", data.name);
    addPackage(data.name, inCache);
  }
});

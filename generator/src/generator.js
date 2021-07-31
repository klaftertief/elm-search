const { Elm } = require("./Main.elm");

let app = Elm.Main.init();

app.ports.log.subscribe(console.log);

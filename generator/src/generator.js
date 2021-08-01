XMLHttpRequest = require('xhr2');

import {Elm} from './Main.elm';

let app = Elm.Main.init();

app.ports.log.subscribe(console.log);

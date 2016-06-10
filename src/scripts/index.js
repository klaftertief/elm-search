var Elm = require('./Cli.js');
var express = require('express');
var index = require('../../dist/0.17/index.json');

var app = Elm.Cli.worker({ index : index });
var server = express();

server.get('/', function (req, res) {
	app.ports.foo.send('Hello?');
	res.send('Hello World');
});

server.listen(9999);
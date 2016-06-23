var Elm = require('./Cli.js');
var express = require('express');
var index = require('../../dist/0.17/index.json');

var app = Elm.Cli.worker({ index : index });
var server = express();

var responses = {};
var interval = 5;
var maxDuration = 5000;

app.ports.response.subscribe(function(msg) {
	responses[msg.query] = msg;
});

server.get('/', function (req, res) {
	var query = req.query.q || "";
	var duration = 0;

	app.ports.request.send(query);

	function trySend() {
		if (responses[query]) {
			var result = responses[query];
			delete responses[query];
			res.send(result);
		} else if (duration >= maxDuration) {
			res.sendStatus(408);
		} else {
			duration += interval;
			setTimeout(trySend, interval);
		}
	};

	trySend();
});

server.listen(9999);
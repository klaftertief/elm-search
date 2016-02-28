var Elm = Elm || { Native: {} };
Elm.Native.Basics = {};
Elm.Native.Basics.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Basics = localRuntime.Native.Basics || {};
	if (localRuntime.Native.Basics.values)
	{
		return localRuntime.Native.Basics.values;
	}

	var Utils = Elm.Native.Utils.make(localRuntime);

	function div(a, b)
	{
		return (a / b) | 0;
	}
	function rem(a, b)
	{
		return a % b;
	}
	function mod(a, b)
	{
		if (b === 0)
		{
			throw new Error('Cannot perform mod 0. Division by zero error.');
		}
		var r = a % b;
		var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r + b) : -mod(-a, -b));

		return m === b ? 0 : m;
	}
	function logBase(base, n)
	{
		return Math.log(n) / Math.log(base);
	}
	function negate(n)
	{
		return -n;
	}
	function abs(n)
	{
		return n < 0 ? -n : n;
	}

	function min(a, b)
	{
		return Utils.cmp(a, b) < 0 ? a : b;
	}
	function max(a, b)
	{
		return Utils.cmp(a, b) > 0 ? a : b;
	}
	function clamp(lo, hi, n)
	{
		return Utils.cmp(n, lo) < 0 ? lo : Utils.cmp(n, hi) > 0 ? hi : n;
	}

	function xor(a, b)
	{
		return a !== b;
	}
	function not(b)
	{
		return !b;
	}
	function isInfinite(n)
	{
		return n === Infinity || n === -Infinity;
	}

	function truncate(n)
	{
		return n | 0;
	}

	function degrees(d)
	{
		return d * Math.PI / 180;
	}
	function turns(t)
	{
		return 2 * Math.PI * t;
	}
	function fromPolar(point)
	{
		var r = point._0;
		var t = point._1;
		return Utils.Tuple2(r * Math.cos(t), r * Math.sin(t));
	}
	function toPolar(point)
	{
		var x = point._0;
		var y = point._1;
		return Utils.Tuple2(Math.sqrt(x * x + y * y), Math.atan2(y, x));
	}

	return localRuntime.Native.Basics.values = {
		div: F2(div),
		rem: F2(rem),
		mod: F2(mod),

		pi: Math.PI,
		e: Math.E,
		cos: Math.cos,
		sin: Math.sin,
		tan: Math.tan,
		acos: Math.acos,
		asin: Math.asin,
		atan: Math.atan,
		atan2: F2(Math.atan2),

		degrees: degrees,
		turns: turns,
		fromPolar: fromPolar,
		toPolar: toPolar,

		sqrt: Math.sqrt,
		logBase: F2(logBase),
		negate: negate,
		abs: abs,
		min: F2(min),
		max: F2(max),
		clamp: F3(clamp),
		compare: Utils.compare,

		xor: F2(xor),
		not: not,

		truncate: truncate,
		ceiling: Math.ceil,
		floor: Math.floor,
		round: Math.round,
		toFloat: function(x) { return x; },
		isNaN: isNaN,
		isInfinite: isInfinite
	};
};

Elm.Native.Port = {};

Elm.Native.Port.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Port = localRuntime.Native.Port || {};
	if (localRuntime.Native.Port.values)
	{
		return localRuntime.Native.Port.values;
	}

	var NS;

	// INBOUND

	function inbound(name, type, converter)
	{
		if (!localRuntime.argsTracker[name])
		{
			throw new Error(
				'Port Error:\n' +
				'No argument was given for the port named \'' + name + '\' with type:\n\n' +
				'    ' + type.split('\n').join('\n        ') + '\n\n' +
				'You need to provide an initial value!\n\n' +
				'Find out more about ports here <http://elm-lang.org/learn/Ports.elm>'
			);
		}
		var arg = localRuntime.argsTracker[name];
		arg.used = true;

		return jsToElm(name, type, converter, arg.value);
	}


	function inboundSignal(name, type, converter)
	{
		var initialValue = inbound(name, type, converter);

		if (!NS)
		{
			NS = Elm.Native.Signal.make(localRuntime);
		}
		var signal = NS.input('inbound-port-' + name, initialValue);

		function send(jsValue)
		{
			var elmValue = jsToElm(name, type, converter, jsValue);
			setTimeout(function() {
				localRuntime.notify(signal.id, elmValue);
			}, 0);
		}

		localRuntime.ports[name] = { send: send };

		return signal;
	}


	function jsToElm(name, type, converter, value)
	{
		try
		{
			return converter(value);
		}
		catch(e)
		{
			throw new Error(
				'Port Error:\n' +
				'Regarding the port named \'' + name + '\' with type:\n\n' +
				'    ' + type.split('\n').join('\n        ') + '\n\n' +
				'You just sent the value:\n\n' +
				'    ' + JSON.stringify(value) + '\n\n' +
				'but it cannot be converted to the necessary type.\n' +
				e.message
			);
		}
	}


	// OUTBOUND

	function outbound(name, converter, elmValue)
	{
		localRuntime.ports[name] = converter(elmValue);
	}


	function outboundSignal(name, converter, signal)
	{
		var subscribers = [];

		function subscribe(handler)
		{
			subscribers.push(handler);
		}
		function unsubscribe(handler)
		{
			subscribers.pop(subscribers.indexOf(handler));
		}

		function notify(elmValue)
		{
			var jsValue = converter(elmValue);
			var len = subscribers.length;
			for (var i = 0; i < len; ++i)
			{
				subscribers[i](jsValue);
			}
		}

		if (!NS)
		{
			NS = Elm.Native.Signal.make(localRuntime);
		}
		NS.output('outbound-port-' + name, notify, signal);

		localRuntime.ports[name] = {
			subscribe: subscribe,
			unsubscribe: unsubscribe
		};

		return signal;
	}


	return localRuntime.Native.Port.values = {
		inbound: inbound,
		outbound: outbound,
		inboundSignal: inboundSignal,
		outboundSignal: outboundSignal
	};
};

if (!Elm.fullscreen) {
	(function() {
		'use strict';

		var Display = {
			FULLSCREEN: 0,
			COMPONENT: 1,
			NONE: 2
		};

		Elm.fullscreen = function(module, args)
		{
			var container = document.createElement('div');
			document.body.appendChild(container);
			return init(Display.FULLSCREEN, container, module, args || {});
		};

		Elm.embed = function(module, container, args)
		{
			var tag = container.tagName;
			if (tag !== 'DIV')
			{
				throw new Error('Elm.node must be given a DIV, not a ' + tag + '.');
			}
			return init(Display.COMPONENT, container, module, args || {});
		};

		Elm.worker = function(module, args)
		{
			return init(Display.NONE, {}, module, args || {});
		};

		function init(display, container, module, args, moduleToReplace)
		{
			// defining state needed for an instance of the Elm RTS
			var inputs = [];

			/* OFFSET
			 * Elm's time traveling debugger lets you pause time. This means
			 * "now" may be shifted a bit into the past. By wrapping Date.now()
			 * we can manage this.
			 */
			var timer = {
				programStart: Date.now(),
				now: function()
				{
					return Date.now();
				}
			};

			var updateInProgress = false;
			function notify(id, v)
			{
				if (updateInProgress)
				{
					throw new Error(
						'The notify function has been called synchronously!\n' +
						'This can lead to frames being dropped.\n' +
						'Definitely report this to <https://github.com/elm-lang/Elm/issues>\n');
				}
				updateInProgress = true;
				var timestep = timer.now();
				for (var i = inputs.length; i--; )
				{
					inputs[i].notify(timestep, id, v);
				}
				updateInProgress = false;
			}
			function setTimeout(func, delay)
			{
				return window.setTimeout(func, delay);
			}

			var listeners = [];
			function addListener(relevantInputs, domNode, eventName, func)
			{
				domNode.addEventListener(eventName, func);
				var listener = {
					relevantInputs: relevantInputs,
					domNode: domNode,
					eventName: eventName,
					func: func
				};
				listeners.push(listener);
			}

			var argsTracker = {};
			for (var name in args)
			{
				argsTracker[name] = {
					value: args[name],
					used: false
				};
			}

			// create the actual RTS. Any impure modules will attach themselves to this
			// object. This permits many Elm programs to be embedded per document.
			var elm = {
				notify: notify,
				setTimeout: setTimeout,
				node: container,
				addListener: addListener,
				inputs: inputs,
				timer: timer,
				argsTracker: argsTracker,
				ports: {},

				isFullscreen: function() { return display === Display.FULLSCREEN; },
				isEmbed: function() { return display === Display.COMPONENT; },
				isWorker: function() { return display === Display.NONE; }
			};

			function swap(newModule)
			{
				removeListeners(listeners);
				var div = document.createElement('div');
				var newElm = init(display, div, newModule, args, elm);
				inputs = [];

				return newElm;
			}

			function dispose()
			{
				removeListeners(listeners);
				inputs = [];
			}

			var Module = {};
			try
			{
				Module = module.make(elm);
				checkInputs(elm);
			}
			catch (error)
			{
				if (typeof container.appendChild === "function")
				{
					container.appendChild(errorNode(error.message));
				}
				else
				{
					console.error(error.message);
				}
				throw error;
			}

			if (display !== Display.NONE)
			{
				var graphicsNode = initGraphics(elm, Module);
			}

			var rootNode = { kids: inputs };
			trimDeadNodes(rootNode);
			inputs = rootNode.kids;
			filterListeners(inputs, listeners);

			addReceivers(elm.ports);

			if (typeof moduleToReplace !== 'undefined')
			{
				hotSwap(moduleToReplace, elm);

				// rerender scene if graphics are enabled.
				if (typeof graphicsNode !== 'undefined')
				{
					graphicsNode.notify(0, true, 0);
				}
			}

			return {
				swap: swap,
				ports: elm.ports,
				dispose: dispose
			};
		}

		function checkInputs(elm)
		{
			var argsTracker = elm.argsTracker;
			for (var name in argsTracker)
			{
				if (!argsTracker[name].used)
				{
					throw new Error(
						"Port Error:\nYou provided an argument named '" + name +
						"' but there is no corresponding port!\n\n" +
						"Maybe add a port '" + name + "' to your Elm module?\n" +
						"Maybe remove the '" + name + "' argument from your initialization code in JS?"
					);
				}
			}
		}

		function errorNode(message)
		{
			var code = document.createElement('code');

			var lines = message.split('\n');
			code.appendChild(document.createTextNode(lines[0]));
			code.appendChild(document.createElement('br'));
			code.appendChild(document.createElement('br'));
			for (var i = 1; i < lines.length; ++i)
			{
				code.appendChild(document.createTextNode('\u00A0 \u00A0 ' + lines[i].replace(/  /g, '\u00A0 ')));
				code.appendChild(document.createElement('br'));
			}
			code.appendChild(document.createElement('br'));
			code.appendChild(document.createTextNode('Open the developer console for more details.'));
			return code;
		}


		//// FILTER SIGNALS ////

		// TODO: move this code into the signal module and create a function
		// Signal.initializeGraph that actually instantiates everything.

		function filterListeners(inputs, listeners)
		{
			loop:
			for (var i = listeners.length; i--; )
			{
				var listener = listeners[i];
				for (var j = inputs.length; j--; )
				{
					if (listener.relevantInputs.indexOf(inputs[j].id) >= 0)
					{
						continue loop;
					}
				}
				listener.domNode.removeEventListener(listener.eventName, listener.func);
			}
		}

		function removeListeners(listeners)
		{
			for (var i = listeners.length; i--; )
			{
				var listener = listeners[i];
				listener.domNode.removeEventListener(listener.eventName, listener.func);
			}
		}

		// add receivers for built-in ports if they are defined
		function addReceivers(ports)
		{
			if ('title' in ports)
			{
				if (typeof ports.title === 'string')
				{
					document.title = ports.title;
				}
				else
				{
					ports.title.subscribe(function(v) { document.title = v; });
				}
			}
			if ('redirect' in ports)
			{
				ports.redirect.subscribe(function(v) {
					if (v.length > 0)
					{
						window.location = v;
					}
				});
			}
		}


		// returns a boolean representing whether the node is alive or not.
		function trimDeadNodes(node)
		{
			if (node.isOutput)
			{
				return true;
			}

			var liveKids = [];
			for (var i = node.kids.length; i--; )
			{
				var kid = node.kids[i];
				if (trimDeadNodes(kid))
				{
					liveKids.push(kid);
				}
			}
			node.kids = liveKids;

			return liveKids.length > 0;
		}


		////  RENDERING  ////

		function initGraphics(elm, Module)
		{
			if (!('main' in Module))
			{
				throw new Error("'main' is missing! What do I display?!");
			}

			var signalGraph = Module.main;

			// make sure the signal graph is actually a signal & extract the visual model
			if (!('notify' in signalGraph))
			{
				signalGraph = Elm.Signal.make(elm).constant(signalGraph);
			}
			var initialScene = signalGraph.value;

			// Figure out what the render functions should be
			var render;
			var update;
			if (initialScene.ctor === 'Element_elm_builtin')
			{
				var Element = Elm.Native.Graphics.Element.make(elm);
				render = Element.render;
				update = Element.updateAndReplace;
			}
			else
			{
				var VirtualDom = Elm.Native.VirtualDom.make(elm);
				render = VirtualDom.render;
				update = VirtualDom.updateAndReplace;
			}

			// Add the initialScene to the DOM
			var container = elm.node;
			var node = render(initialScene);
			while (container.firstChild)
			{
				container.removeChild(container.firstChild);
			}
			container.appendChild(node);

			var _requestAnimationFrame =
				typeof requestAnimationFrame !== 'undefined'
					? requestAnimationFrame
					: function(cb) { setTimeout(cb, 1000 / 60); }
					;

			// domUpdate is called whenever the main Signal changes.
			//
			// domUpdate and drawCallback implement a small state machine in order
			// to schedule only 1 draw per animation frame. This enforces that
			// once draw has been called, it will not be called again until the
			// next frame.
			//
			// drawCallback is scheduled whenever
			// 1. The state transitions from PENDING_REQUEST to EXTRA_REQUEST, or
			// 2. The state transitions from NO_REQUEST to PENDING_REQUEST
			//
			// Invariants:
			// 1. In the NO_REQUEST state, there is never a scheduled drawCallback.
			// 2. In the PENDING_REQUEST and EXTRA_REQUEST states, there is always exactly 1
			//    scheduled drawCallback.
			var NO_REQUEST = 0;
			var PENDING_REQUEST = 1;
			var EXTRA_REQUEST = 2;
			var state = NO_REQUEST;
			var savedScene = initialScene;
			var scheduledScene = initialScene;

			function domUpdate(newScene)
			{
				scheduledScene = newScene;

				switch (state)
				{
					case NO_REQUEST:
						_requestAnimationFrame(drawCallback);
						state = PENDING_REQUEST;
						return;
					case PENDING_REQUEST:
						state = PENDING_REQUEST;
						return;
					case EXTRA_REQUEST:
						state = PENDING_REQUEST;
						return;
				}
			}

			function drawCallback()
			{
				switch (state)
				{
					case NO_REQUEST:
						// This state should not be possible. How can there be no
						// request, yet somehow we are actively fulfilling a
						// request?
						throw new Error(
							'Unexpected draw callback.\n' +
							'Please report this to <https://github.com/elm-lang/core/issues>.'
						);

					case PENDING_REQUEST:
						// At this point, we do not *know* that another frame is
						// needed, but we make an extra request to rAF just in
						// case. It's possible to drop a frame if rAF is called
						// too late, so we just do it preemptively.
						_requestAnimationFrame(drawCallback);
						state = EXTRA_REQUEST;

						// There's also stuff we definitely need to draw.
						draw();
						return;

					case EXTRA_REQUEST:
						// Turns out the extra request was not needed, so we will
						// stop calling rAF. No reason to call it all the time if
						// no one needs it.
						state = NO_REQUEST;
						return;
				}
			}

			function draw()
			{
				update(elm.node.firstChild, savedScene, scheduledScene);
				if (elm.Native.Window)
				{
					elm.Native.Window.values.resizeIfNeeded();
				}
				savedScene = scheduledScene;
			}

			var renderer = Elm.Native.Signal.make(elm).output('main', domUpdate, signalGraph);

			// must check for resize after 'renderer' is created so
			// that changes show up.
			if (elm.Native.Window)
			{
				elm.Native.Window.values.resizeIfNeeded();
			}

			return renderer;
		}

		//// HOT SWAPPING ////

		// Returns boolean indicating if the swap was successful.
		// Requires that the two signal graphs have exactly the same
		// structure.
		function hotSwap(from, to)
		{
			function similar(nodeOld, nodeNew)
			{
				if (nodeOld.id !== nodeNew.id)
				{
					return false;
				}
				if (nodeOld.isOutput)
				{
					return nodeNew.isOutput;
				}
				return nodeOld.kids.length === nodeNew.kids.length;
			}
			function swap(nodeOld, nodeNew)
			{
				nodeNew.value = nodeOld.value;
				return true;
			}
			var canSwap = depthFirstTraversals(similar, from.inputs, to.inputs);
			if (canSwap)
			{
				depthFirstTraversals(swap, from.inputs, to.inputs);
			}
			from.node.parentNode.replaceChild(to.node, from.node);

			return canSwap;
		}

		// Returns false if the node operation f ever fails.
		function depthFirstTraversals(f, queueOld, queueNew)
		{
			if (queueOld.length !== queueNew.length)
			{
				return false;
			}
			queueOld = queueOld.slice(0);
			queueNew = queueNew.slice(0);

			var seen = [];
			while (queueOld.length > 0 && queueNew.length > 0)
			{
				var nodeOld = queueOld.pop();
				var nodeNew = queueNew.pop();
				if (seen.indexOf(nodeOld.id) < 0)
				{
					if (!f(nodeOld, nodeNew))
					{
						return false;
					}
					queueOld = queueOld.concat(nodeOld.kids || []);
					queueNew = queueNew.concat(nodeNew.kids || []);
					seen.push(nodeOld.id);
				}
			}
			return true;
		}
	}());

	function F2(fun)
	{
		function wrapper(a) { return function(b) { return fun(a,b); }; }
		wrapper.arity = 2;
		wrapper.func = fun;
		return wrapper;
	}

	function F3(fun)
	{
		function wrapper(a) {
			return function(b) { return function(c) { return fun(a, b, c); }; };
		}
		wrapper.arity = 3;
		wrapper.func = fun;
		return wrapper;
	}

	function F4(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return fun(a, b, c, d); }; }; };
		}
		wrapper.arity = 4;
		wrapper.func = fun;
		return wrapper;
	}

	function F5(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
		}
		wrapper.arity = 5;
		wrapper.func = fun;
		return wrapper;
	}

	function F6(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return fun(a, b, c, d, e, f); }; }; }; }; };
		}
		wrapper.arity = 6;
		wrapper.func = fun;
		return wrapper;
	}

	function F7(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
		}
		wrapper.arity = 7;
		wrapper.func = fun;
		return wrapper;
	}

	function F8(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return function(g) { return function(h) {
			return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
		}
		wrapper.arity = 8;
		wrapper.func = fun;
		return wrapper;
	}

	function F9(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return function(g) { return function(h) { return function(i) {
			return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
		}
		wrapper.arity = 9;
		wrapper.func = fun;
		return wrapper;
	}

	function A2(fun, a, b)
	{
		return fun.arity === 2
			? fun.func(a, b)
			: fun(a)(b);
	}
	function A3(fun, a, b, c)
	{
		return fun.arity === 3
			? fun.func(a, b, c)
			: fun(a)(b)(c);
	}
	function A4(fun, a, b, c, d)
	{
		return fun.arity === 4
			? fun.func(a, b, c, d)
			: fun(a)(b)(c)(d);
	}
	function A5(fun, a, b, c, d, e)
	{
		return fun.arity === 5
			? fun.func(a, b, c, d, e)
			: fun(a)(b)(c)(d)(e);
	}
	function A6(fun, a, b, c, d, e, f)
	{
		return fun.arity === 6
			? fun.func(a, b, c, d, e, f)
			: fun(a)(b)(c)(d)(e)(f);
	}
	function A7(fun, a, b, c, d, e, f, g)
	{
		return fun.arity === 7
			? fun.func(a, b, c, d, e, f, g)
			: fun(a)(b)(c)(d)(e)(f)(g);
	}
	function A8(fun, a, b, c, d, e, f, g, h)
	{
		return fun.arity === 8
			? fun.func(a, b, c, d, e, f, g, h)
			: fun(a)(b)(c)(d)(e)(f)(g)(h);
	}
	function A9(fun, a, b, c, d, e, f, g, h, i)
	{
		return fun.arity === 9
			? fun.func(a, b, c, d, e, f, g, h, i)
			: fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
	}
}

Elm.Native = Elm.Native || {};
Elm.Native.Utils = {};
Elm.Native.Utils.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Utils = localRuntime.Native.Utils || {};
	if (localRuntime.Native.Utils.values)
	{
		return localRuntime.Native.Utils.values;
	}


	// COMPARISONS

	function eq(l, r)
	{
		var stack = [{'x': l, 'y': r}];
		while (stack.length > 0)
		{
			var front = stack.pop();
			var x = front.x;
			var y = front.y;
			if (x === y)
			{
				continue;
			}
			if (typeof x === 'object')
			{
				var c = 0;
				for (var i in x)
				{
					++c;
					if (i in y)
					{
						if (i !== 'ctor')
						{
							stack.push({ 'x': x[i], 'y': y[i] });
						}
					}
					else
					{
						return false;
					}
				}
				if ('ctor' in x)
				{
					stack.push({'x': x.ctor, 'y': y.ctor});
				}
				if (c !== Object.keys(y).length)
				{
					return false;
				}
			}
			else if (typeof x === 'function')
			{
				throw new Error('Equality error: general function equality is ' +
								'undecidable, and therefore, unsupported');
			}
			else
			{
				return false;
			}
		}
		return true;
	}

	// code in Generate/JavaScript.hs depends on the particular
	// integer values assigned to LT, EQ, and GT
	var LT = -1, EQ = 0, GT = 1, ord = ['LT', 'EQ', 'GT'];

	function compare(x, y)
	{
		return {
			ctor: ord[cmp(x, y) + 1]
		};
	}

	function cmp(x, y) {
		var ord;
		if (typeof x !== 'object')
		{
			return x === y ? EQ : x < y ? LT : GT;
		}
		else if (x.isChar)
		{
			var a = x.toString();
			var b = y.toString();
			return a === b
				? EQ
				: a < b
					? LT
					: GT;
		}
		else if (x.ctor === '::' || x.ctor === '[]')
		{
			while (true)
			{
				if (x.ctor === '[]' && y.ctor === '[]')
				{
					return EQ;
				}
				if (x.ctor !== y.ctor)
				{
					return x.ctor === '[]' ? LT : GT;
				}
				ord = cmp(x._0, y._0);
				if (ord !== EQ)
				{
					return ord;
				}
				x = x._1;
				y = y._1;
			}
		}
		else if (x.ctor.slice(0, 6) === '_Tuple')
		{
			var n = x.ctor.slice(6) - 0;
			var err = 'cannot compare tuples with more than 6 elements.';
			if (n === 0) return EQ;
			if (n >= 1) { ord = cmp(x._0, y._0); if (ord !== EQ) return ord;
			if (n >= 2) { ord = cmp(x._1, y._1); if (ord !== EQ) return ord;
			if (n >= 3) { ord = cmp(x._2, y._2); if (ord !== EQ) return ord;
			if (n >= 4) { ord = cmp(x._3, y._3); if (ord !== EQ) return ord;
			if (n >= 5) { ord = cmp(x._4, y._4); if (ord !== EQ) return ord;
			if (n >= 6) { ord = cmp(x._5, y._5); if (ord !== EQ) return ord;
			if (n >= 7) throw new Error('Comparison error: ' + err); } } } } } }
			return EQ;
		}
		else
		{
			throw new Error('Comparison error: comparison is only defined on ints, ' +
							'floats, times, chars, strings, lists of comparable values, ' +
							'and tuples of comparable values.');
		}
	}


	// TUPLES

	var Tuple0 = {
		ctor: '_Tuple0'
	};

	function Tuple2(x, y)
	{
		return {
			ctor: '_Tuple2',
			_0: x,
			_1: y
		};
	}


	// LITERALS

	function chr(c)
	{
		var x = new String(c);
		x.isChar = true;
		return x;
	}

	function txt(str)
	{
		var t = new String(str);
		t.text = true;
		return t;
	}


	// GUID

	var count = 0;
	function guid(_)
	{
		return count++;
	}


	// RECORDS

	function update(oldRecord, updatedFields)
	{
		var newRecord = {};
		for (var key in oldRecord)
		{
			var value = (key in updatedFields) ? updatedFields[key] : oldRecord[key];
			newRecord[key] = value;
		}
		return newRecord;
	}


	// MOUSE COORDINATES

	function getXY(e)
	{
		var posx = 0;
		var posy = 0;
		if (e.pageX || e.pageY)
		{
			posx = e.pageX;
			posy = e.pageY;
		}
		else if (e.clientX || e.clientY)
		{
			posx = e.clientX + document.body.scrollLeft + document.documentElement.scrollLeft;
			posy = e.clientY + document.body.scrollTop + document.documentElement.scrollTop;
		}

		if (localRuntime.isEmbed())
		{
			var rect = localRuntime.node.getBoundingClientRect();
			var relx = rect.left + document.body.scrollLeft + document.documentElement.scrollLeft;
			var rely = rect.top + document.body.scrollTop + document.documentElement.scrollTop;
			// TODO: figure out if there is a way to avoid rounding here
			posx = posx - Math.round(relx) - localRuntime.node.clientLeft;
			posy = posy - Math.round(rely) - localRuntime.node.clientTop;
		}
		return Tuple2(posx, posy);
	}


	//// LIST STUFF ////

	var Nil = { ctor: '[]' };

	function Cons(hd, tl)
	{
		return {
			ctor: '::',
			_0: hd,
			_1: tl
		};
	}

	function list(arr)
	{
		var out = Nil;
		for (var i = arr.length; i--; )
		{
			out = Cons(arr[i], out);
		}
		return out;
	}

	function range(lo, hi)
	{
		var list = Nil;
		if (lo <= hi)
		{
			do
			{
				list = Cons(hi, list);
			}
			while (hi-- > lo);
		}
		return list;
	}

	function append(xs, ys)
	{
		// append Strings
		if (typeof xs === 'string')
		{
			return xs + ys;
		}

		// append Text
		if (xs.ctor.slice(0, 5) === 'Text:')
		{
			return {
				ctor: 'Text:Append',
				_0: xs,
				_1: ys
			};
		}


		// append Lists
		if (xs.ctor === '[]')
		{
			return ys;
		}
		var root = Cons(xs._0, Nil);
		var curr = root;
		xs = xs._1;
		while (xs.ctor !== '[]')
		{
			curr._1 = Cons(xs._0, Nil);
			xs = xs._1;
			curr = curr._1;
		}
		curr._1 = ys;
		return root;
	}


	// CRASHES

	function crash(moduleName, region)
	{
		return function(message) {
			throw new Error(
				'Ran into a `Debug.crash` in module `' + moduleName + '` ' + regionToString(region) + '\n'
				+ 'The message provided by the code author is:\n\n    '
				+ message
			);
		};
	}

	function crashCase(moduleName, region, value)
	{
		return function(message) {
			throw new Error(
				'Ran into a `Debug.crash` in module `' + moduleName + '`\n\n'
				+ 'This was caused by the `case` expression ' + regionToString(region) + '.\n'
				+ 'One of the branches ended with a crash and the following value got through:\n\n    ' + toString(value) + '\n\n'
				+ 'The message provided by the code author is:\n\n    '
				+ message
			);
		};
	}

	function regionToString(region)
	{
		if (region.start.line == region.end.line)
		{
			return 'on line ' + region.start.line;
		}
		return 'between lines ' + region.start.line + ' and ' + region.end.line;
	}


	// BAD PORTS

	function badPort(expected, received)
	{
		throw new Error(
			'Runtime error when sending values through a port.\n\n'
			+ 'Expecting ' + expected + ' but was given ' + formatValue(received)
		);
	}

	function formatValue(value)
	{
		// Explicity format undefined values as "undefined"
		// because JSON.stringify(undefined) unhelpfully returns ""
		return (value === undefined) ? "undefined" : JSON.stringify(value);
	}


	// TO STRING

	var _Array;
	var Dict;
	var List;

	var toString = function(v)
	{
		var type = typeof v;
		if (type === 'function')
		{
			var name = v.func ? v.func.name : v.name;
			return '<function' + (name === '' ? '' : ': ') + name + '>';
		}
		else if (type === 'boolean')
		{
			return v ? 'True' : 'False';
		}
		else if (type === 'number')
		{
			return v + '';
		}
		else if ((v instanceof String) && v.isChar)
		{
			return '\'' + addSlashes(v, true) + '\'';
		}
		else if (type === 'string')
		{
			return '"' + addSlashes(v, false) + '"';
		}
		else if (type === 'object' && 'ctor' in v)
		{
			if (v.ctor.substring(0, 6) === '_Tuple')
			{
				var output = [];
				for (var k in v)
				{
					if (k === 'ctor') continue;
					output.push(toString(v[k]));
				}
				return '(' + output.join(',') + ')';
			}
			else if (v.ctor === '_Array')
			{
				if (!_Array)
				{
					_Array = Elm.Array.make(localRuntime);
				}
				var list = _Array.toList(v);
				return 'Array.fromList ' + toString(list);
			}
			else if (v.ctor === '::')
			{
				var output = '[' + toString(v._0);
				v = v._1;
				while (v.ctor === '::')
				{
					output += ',' + toString(v._0);
					v = v._1;
				}
				return output + ']';
			}
			else if (v.ctor === '[]')
			{
				return '[]';
			}
			else if (v.ctor === 'RBNode_elm_builtin' || v.ctor === 'RBEmpty_elm_builtin' || v.ctor === 'Set_elm_builtin')
			{
				if (!Dict)
				{
					Dict = Elm.Dict.make(localRuntime);
				}
				var list;
				var name;
				if (v.ctor === 'Set_elm_builtin')
				{
					if (!List)
					{
						List = Elm.List.make(localRuntime);
					}
					name = 'Set';
					list = A2(List.map, function(x) {return x._0; }, Dict.toList(v._0));
				}
				else
				{
					name = 'Dict';
					list = Dict.toList(v);
				}
				return name + '.fromList ' + toString(list);
			}
			else if (v.ctor.slice(0, 5) === 'Text:')
			{
				return '<text>';
			}
			else if (v.ctor === 'Element_elm_builtin')
			{
				return '<element>'
			}
			else if (v.ctor === 'Form_elm_builtin')
			{
				return '<form>'
			}
			else
			{
				var output = '';
				for (var i in v)
				{
					if (i === 'ctor') continue;
					var str = toString(v[i]);
					var parenless = str[0] === '{' || str[0] === '<' || str.indexOf(' ') < 0;
					output += ' ' + (parenless ? str : '(' + str + ')');
				}
				return v.ctor + output;
			}
		}
		else if (type === 'object' && 'notify' in v && 'id' in v)
		{
			return '<signal>';
		}
		else if (type === 'object')
		{
			var output = [];
			for (var k in v)
			{
				output.push(k + ' = ' + toString(v[k]));
			}
			if (output.length === 0)
			{
				return '{}';
			}
			return '{ ' + output.join(', ') + ' }';
		}
		return '<internal structure>';
	};

	function addSlashes(str, isChar)
	{
		var s = str.replace(/\\/g, '\\\\')
				  .replace(/\n/g, '\\n')
				  .replace(/\t/g, '\\t')
				  .replace(/\r/g, '\\r')
				  .replace(/\v/g, '\\v')
				  .replace(/\0/g, '\\0');
		if (isChar)
		{
			return s.replace(/\'/g, '\\\'');
		}
		else
		{
			return s.replace(/\"/g, '\\"');
		}
	}


	return localRuntime.Native.Utils.values = {
		eq: eq,
		cmp: cmp,
		compare: F2(compare),
		Tuple0: Tuple0,
		Tuple2: Tuple2,
		chr: chr,
		txt: txt,
		update: update,
		guid: guid,
		getXY: getXY,

		Nil: Nil,
		Cons: Cons,
		list: list,
		range: range,
		append: F2(append),

		crash: crash,
		crashCase: crashCase,
		badPort: badPort,

		toString: toString
	};
};

Elm.Basics = Elm.Basics || {};
Elm.Basics.make = function (_elm) {
   "use strict";
   _elm.Basics = _elm.Basics || {};
   if (_elm.Basics.values) return _elm.Basics.values;
   var _U = Elm.Native.Utils.make(_elm),$Native$Basics = Elm.Native.Basics.make(_elm),$Native$Utils = Elm.Native.Utils.make(_elm);
   var _op = {};
   var uncurry = F2(function (f,_p0) {    var _p1 = _p0;return A2(f,_p1._0,_p1._1);});
   var curry = F3(function (f,a,b) {    return f({ctor: "_Tuple2",_0: a,_1: b});});
   var flip = F3(function (f,b,a) {    return A2(f,a,b);});
   var snd = function (_p2) {    var _p3 = _p2;return _p3._1;};
   var fst = function (_p4) {    var _p5 = _p4;return _p5._0;};
   var always = F2(function (a,_p6) {    return a;});
   var identity = function (x) {    return x;};
   _op["<|"] = F2(function (f,x) {    return f(x);});
   _op["|>"] = F2(function (x,f) {    return f(x);});
   _op[">>"] = F3(function (f,g,x) {    return g(f(x));});
   _op["<<"] = F3(function (g,f,x) {    return g(f(x));});
   _op["++"] = $Native$Utils.append;
   var toString = $Native$Utils.toString;
   var isInfinite = $Native$Basics.isInfinite;
   var isNaN = $Native$Basics.isNaN;
   var toFloat = $Native$Basics.toFloat;
   var ceiling = $Native$Basics.ceiling;
   var floor = $Native$Basics.floor;
   var truncate = $Native$Basics.truncate;
   var round = $Native$Basics.round;
   var not = $Native$Basics.not;
   var xor = $Native$Basics.xor;
   _op["||"] = $Native$Basics.or;
   _op["&&"] = $Native$Basics.and;
   var max = $Native$Basics.max;
   var min = $Native$Basics.min;
   var GT = {ctor: "GT"};
   var EQ = {ctor: "EQ"};
   var LT = {ctor: "LT"};
   var compare = $Native$Basics.compare;
   _op[">="] = $Native$Basics.ge;
   _op["<="] = $Native$Basics.le;
   _op[">"] = $Native$Basics.gt;
   _op["<"] = $Native$Basics.lt;
   _op["/="] = $Native$Basics.neq;
   _op["=="] = $Native$Basics.eq;
   var e = $Native$Basics.e;
   var pi = $Native$Basics.pi;
   var clamp = $Native$Basics.clamp;
   var logBase = $Native$Basics.logBase;
   var abs = $Native$Basics.abs;
   var negate = $Native$Basics.negate;
   var sqrt = $Native$Basics.sqrt;
   var atan2 = $Native$Basics.atan2;
   var atan = $Native$Basics.atan;
   var asin = $Native$Basics.asin;
   var acos = $Native$Basics.acos;
   var tan = $Native$Basics.tan;
   var sin = $Native$Basics.sin;
   var cos = $Native$Basics.cos;
   _op["^"] = $Native$Basics.exp;
   _op["%"] = $Native$Basics.mod;
   var rem = $Native$Basics.rem;
   _op["//"] = $Native$Basics.div;
   _op["/"] = $Native$Basics.floatDiv;
   _op["*"] = $Native$Basics.mul;
   _op["-"] = $Native$Basics.sub;
   _op["+"] = $Native$Basics.add;
   var toPolar = $Native$Basics.toPolar;
   var fromPolar = $Native$Basics.fromPolar;
   var turns = $Native$Basics.turns;
   var degrees = $Native$Basics.degrees;
   var radians = function (t) {    return t;};
   return _elm.Basics.values = {_op: _op
                               ,max: max
                               ,min: min
                               ,compare: compare
                               ,not: not
                               ,xor: xor
                               ,rem: rem
                               ,negate: negate
                               ,abs: abs
                               ,sqrt: sqrt
                               ,clamp: clamp
                               ,logBase: logBase
                               ,e: e
                               ,pi: pi
                               ,cos: cos
                               ,sin: sin
                               ,tan: tan
                               ,acos: acos
                               ,asin: asin
                               ,atan: atan
                               ,atan2: atan2
                               ,round: round
                               ,floor: floor
                               ,ceiling: ceiling
                               ,truncate: truncate
                               ,toFloat: toFloat
                               ,degrees: degrees
                               ,radians: radians
                               ,turns: turns
                               ,toPolar: toPolar
                               ,fromPolar: fromPolar
                               ,isNaN: isNaN
                               ,isInfinite: isInfinite
                               ,toString: toString
                               ,fst: fst
                               ,snd: snd
                               ,identity: identity
                               ,always: always
                               ,flip: flip
                               ,curry: curry
                               ,uncurry: uncurry
                               ,LT: LT
                               ,EQ: EQ
                               ,GT: GT};
};
Elm.Maybe = Elm.Maybe || {};
Elm.Maybe.make = function (_elm) {
   "use strict";
   _elm.Maybe = _elm.Maybe || {};
   if (_elm.Maybe.values) return _elm.Maybe.values;
   var _U = Elm.Native.Utils.make(_elm);
   var _op = {};
   var withDefault = F2(function ($default,maybe) {    var _p0 = maybe;if (_p0.ctor === "Just") {    return _p0._0;} else {    return $default;}});
   var Nothing = {ctor: "Nothing"};
   var oneOf = function (maybes) {
      oneOf: while (true) {
         var _p1 = maybes;
         if (_p1.ctor === "[]") {
               return Nothing;
            } else {
               var _p3 = _p1._0;
               var _p2 = _p3;
               if (_p2.ctor === "Nothing") {
                     var _v3 = _p1._1;
                     maybes = _v3;
                     continue oneOf;
                  } else {
                     return _p3;
                  }
            }
      }
   };
   var andThen = F2(function (maybeValue,callback) {
      var _p4 = maybeValue;
      if (_p4.ctor === "Just") {
            return callback(_p4._0);
         } else {
            return Nothing;
         }
   });
   var Just = function (a) {    return {ctor: "Just",_0: a};};
   var map = F2(function (f,maybe) {    var _p5 = maybe;if (_p5.ctor === "Just") {    return Just(f(_p5._0));} else {    return Nothing;}});
   var map2 = F3(function (func,ma,mb) {
      var _p6 = {ctor: "_Tuple2",_0: ma,_1: mb};
      if (_p6.ctor === "_Tuple2" && _p6._0.ctor === "Just" && _p6._1.ctor === "Just") {
            return Just(A2(func,_p6._0._0,_p6._1._0));
         } else {
            return Nothing;
         }
   });
   var map3 = F4(function (func,ma,mb,mc) {
      var _p7 = {ctor: "_Tuple3",_0: ma,_1: mb,_2: mc};
      if (_p7.ctor === "_Tuple3" && _p7._0.ctor === "Just" && _p7._1.ctor === "Just" && _p7._2.ctor === "Just") {
            return Just(A3(func,_p7._0._0,_p7._1._0,_p7._2._0));
         } else {
            return Nothing;
         }
   });
   var map4 = F5(function (func,ma,mb,mc,md) {
      var _p8 = {ctor: "_Tuple4",_0: ma,_1: mb,_2: mc,_3: md};
      if (_p8.ctor === "_Tuple4" && _p8._0.ctor === "Just" && _p8._1.ctor === "Just" && _p8._2.ctor === "Just" && _p8._3.ctor === "Just") {
            return Just(A4(func,_p8._0._0,_p8._1._0,_p8._2._0,_p8._3._0));
         } else {
            return Nothing;
         }
   });
   var map5 = F6(function (func,ma,mb,mc,md,me) {
      var _p9 = {ctor: "_Tuple5",_0: ma,_1: mb,_2: mc,_3: md,_4: me};
      if (_p9.ctor === "_Tuple5" && _p9._0.ctor === "Just" && _p9._1.ctor === "Just" && _p9._2.ctor === "Just" && _p9._3.ctor === "Just" && _p9._4.ctor === "Just")
      {
            return Just(A5(func,_p9._0._0,_p9._1._0,_p9._2._0,_p9._3._0,_p9._4._0));
         } else {
            return Nothing;
         }
   });
   return _elm.Maybe.values = {_op: _op
                              ,andThen: andThen
                              ,map: map
                              ,map2: map2
                              ,map3: map3
                              ,map4: map4
                              ,map5: map5
                              ,withDefault: withDefault
                              ,oneOf: oneOf
                              ,Just: Just
                              ,Nothing: Nothing};
};
Elm.Native.List = {};
Elm.Native.List.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.List = localRuntime.Native.List || {};
	if (localRuntime.Native.List.values)
	{
		return localRuntime.Native.List.values;
	}
	if ('values' in Elm.Native.List)
	{
		return localRuntime.Native.List.values = Elm.Native.List.values;
	}

	var Utils = Elm.Native.Utils.make(localRuntime);

	var Nil = Utils.Nil;
	var Cons = Utils.Cons;

	var fromArray = Utils.list;

	function toArray(xs)
	{
		var out = [];
		while (xs.ctor !== '[]')
		{
			out.push(xs._0);
			xs = xs._1;
		}
		return out;
	}

	// f defined similarly for both foldl and foldr (NB: different from Haskell)
	// ie, foldl : (a -> b -> b) -> b -> [a] -> b
	function foldl(f, b, xs)
	{
		var acc = b;
		while (xs.ctor !== '[]')
		{
			acc = A2(f, xs._0, acc);
			xs = xs._1;
		}
		return acc;
	}

	function foldr(f, b, xs)
	{
		var arr = toArray(xs);
		var acc = b;
		for (var i = arr.length; i--; )
		{
			acc = A2(f, arr[i], acc);
		}
		return acc;
	}

	function map2(f, xs, ys)
	{
		var arr = [];
		while (xs.ctor !== '[]' && ys.ctor !== '[]')
		{
			arr.push(A2(f, xs._0, ys._0));
			xs = xs._1;
			ys = ys._1;
		}
		return fromArray(arr);
	}

	function map3(f, xs, ys, zs)
	{
		var arr = [];
		while (xs.ctor !== '[]' && ys.ctor !== '[]' && zs.ctor !== '[]')
		{
			arr.push(A3(f, xs._0, ys._0, zs._0));
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}

	function map4(f, ws, xs, ys, zs)
	{
		var arr = [];
		while (   ws.ctor !== '[]'
			   && xs.ctor !== '[]'
			   && ys.ctor !== '[]'
			   && zs.ctor !== '[]')
		{
			arr.push(A4(f, ws._0, xs._0, ys._0, zs._0));
			ws = ws._1;
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}

	function map5(f, vs, ws, xs, ys, zs)
	{
		var arr = [];
		while (   vs.ctor !== '[]'
			   && ws.ctor !== '[]'
			   && xs.ctor !== '[]'
			   && ys.ctor !== '[]'
			   && zs.ctor !== '[]')
		{
			arr.push(A5(f, vs._0, ws._0, xs._0, ys._0, zs._0));
			vs = vs._1;
			ws = ws._1;
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}

	function sortBy(f, xs)
	{
		return fromArray(toArray(xs).sort(function(a, b) {
			return Utils.cmp(f(a), f(b));
		}));
	}

	function sortWith(f, xs)
	{
		return fromArray(toArray(xs).sort(function(a, b) {
			var ord = f(a)(b).ctor;
			return ord === 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
		}));
	}

	function take(n, xs)
	{
		var arr = [];
		while (xs.ctor !== '[]' && n > 0)
		{
			arr.push(xs._0);
			xs = xs._1;
			--n;
		}
		return fromArray(arr);
	}


	Elm.Native.List.values = {
		Nil: Nil,
		Cons: Cons,
		cons: F2(Cons),
		toArray: toArray,
		fromArray: fromArray,

		foldl: F3(foldl),
		foldr: F3(foldr),

		map2: F3(map2),
		map3: F4(map3),
		map4: F5(map4),
		map5: F6(map5),
		sortBy: F2(sortBy),
		sortWith: F2(sortWith),
		take: F2(take)
	};
	return localRuntime.Native.List.values = Elm.Native.List.values;
};

Elm.List = Elm.List || {};
Elm.List.make = function (_elm) {
   "use strict";
   _elm.List = _elm.List || {};
   if (_elm.List.values) return _elm.List.values;
   var _U = Elm.Native.Utils.make(_elm),$Basics = Elm.Basics.make(_elm),$Maybe = Elm.Maybe.make(_elm),$Native$List = Elm.Native.List.make(_elm);
   var _op = {};
   var sortWith = $Native$List.sortWith;
   var sortBy = $Native$List.sortBy;
   var sort = function (xs) {    return A2(sortBy,$Basics.identity,xs);};
   var drop = F2(function (n,list) {
      drop: while (true) if (_U.cmp(n,0) < 1) return list; else {
            var _p0 = list;
            if (_p0.ctor === "[]") {
                  return list;
               } else {
                  var _v1 = n - 1,_v2 = _p0._1;
                  n = _v1;
                  list = _v2;
                  continue drop;
               }
         }
   });
   var take = $Native$List.take;
   var map5 = $Native$List.map5;
   var map4 = $Native$List.map4;
   var map3 = $Native$List.map3;
   var map2 = $Native$List.map2;
   var any = F2(function (isOkay,list) {
      any: while (true) {
         var _p1 = list;
         if (_p1.ctor === "[]") {
               return false;
            } else {
               if (isOkay(_p1._0)) return true; else {
                     var _v4 = isOkay,_v5 = _p1._1;
                     isOkay = _v4;
                     list = _v5;
                     continue any;
                  }
            }
      }
   });
   var all = F2(function (isOkay,list) {    return $Basics.not(A2(any,function (_p2) {    return $Basics.not(isOkay(_p2));},list));});
   var foldr = $Native$List.foldr;
   var foldl = $Native$List.foldl;
   var length = function (xs) {    return A3(foldl,F2(function (_p3,i) {    return i + 1;}),0,xs);};
   var sum = function (numbers) {    return A3(foldl,F2(function (x,y) {    return x + y;}),0,numbers);};
   var product = function (numbers) {    return A3(foldl,F2(function (x,y) {    return x * y;}),1,numbers);};
   var maximum = function (list) {
      var _p4 = list;
      if (_p4.ctor === "::") {
            return $Maybe.Just(A3(foldl,$Basics.max,_p4._0,_p4._1));
         } else {
            return $Maybe.Nothing;
         }
   };
   var minimum = function (list) {
      var _p5 = list;
      if (_p5.ctor === "::") {
            return $Maybe.Just(A3(foldl,$Basics.min,_p5._0,_p5._1));
         } else {
            return $Maybe.Nothing;
         }
   };
   var indexedMap = F2(function (f,xs) {    return A3(map2,f,_U.range(0,length(xs) - 1),xs);});
   var member = F2(function (x,xs) {    return A2(any,function (a) {    return _U.eq(a,x);},xs);});
   var isEmpty = function (xs) {    var _p6 = xs;if (_p6.ctor === "[]") {    return true;} else {    return false;}};
   var tail = function (list) {    var _p7 = list;if (_p7.ctor === "::") {    return $Maybe.Just(_p7._1);} else {    return $Maybe.Nothing;}};
   var head = function (list) {    var _p8 = list;if (_p8.ctor === "::") {    return $Maybe.Just(_p8._0);} else {    return $Maybe.Nothing;}};
   _op["::"] = $Native$List.cons;
   var map = F2(function (f,xs) {    return A3(foldr,F2(function (x,acc) {    return A2(_op["::"],f(x),acc);}),_U.list([]),xs);});
   var filter = F2(function (pred,xs) {
      var conditionalCons = F2(function (x,xs$) {    return pred(x) ? A2(_op["::"],x,xs$) : xs$;});
      return A3(foldr,conditionalCons,_U.list([]),xs);
   });
   var maybeCons = F3(function (f,mx,xs) {    var _p9 = f(mx);if (_p9.ctor === "Just") {    return A2(_op["::"],_p9._0,xs);} else {    return xs;}});
   var filterMap = F2(function (f,xs) {    return A3(foldr,maybeCons(f),_U.list([]),xs);});
   var reverse = function (list) {    return A3(foldl,F2(function (x,y) {    return A2(_op["::"],x,y);}),_U.list([]),list);};
   var scanl = F3(function (f,b,xs) {
      var scan1 = F2(function (x,accAcc) {
         var _p10 = accAcc;
         if (_p10.ctor === "::") {
               return A2(_op["::"],A2(f,x,_p10._0),accAcc);
            } else {
               return _U.list([]);
            }
      });
      return reverse(A3(foldl,scan1,_U.list([b]),xs));
   });
   var append = F2(function (xs,ys) {
      var _p11 = ys;
      if (_p11.ctor === "[]") {
            return xs;
         } else {
            return A3(foldr,F2(function (x,y) {    return A2(_op["::"],x,y);}),ys,xs);
         }
   });
   var concat = function (lists) {    return A3(foldr,append,_U.list([]),lists);};
   var concatMap = F2(function (f,list) {    return concat(A2(map,f,list));});
   var partition = F2(function (pred,list) {
      var step = F2(function (x,_p12) {
         var _p13 = _p12;
         var _p15 = _p13._0;
         var _p14 = _p13._1;
         return pred(x) ? {ctor: "_Tuple2",_0: A2(_op["::"],x,_p15),_1: _p14} : {ctor: "_Tuple2",_0: _p15,_1: A2(_op["::"],x,_p14)};
      });
      return A3(foldr,step,{ctor: "_Tuple2",_0: _U.list([]),_1: _U.list([])},list);
   });
   var unzip = function (pairs) {
      var step = F2(function (_p17,_p16) {
         var _p18 = _p17;
         var _p19 = _p16;
         return {ctor: "_Tuple2",_0: A2(_op["::"],_p18._0,_p19._0),_1: A2(_op["::"],_p18._1,_p19._1)};
      });
      return A3(foldr,step,{ctor: "_Tuple2",_0: _U.list([]),_1: _U.list([])},pairs);
   };
   var intersperse = F2(function (sep,xs) {
      var _p20 = xs;
      if (_p20.ctor === "[]") {
            return _U.list([]);
         } else {
            var step = F2(function (x,rest) {    return A2(_op["::"],sep,A2(_op["::"],x,rest));});
            var spersed = A3(foldr,step,_U.list([]),_p20._1);
            return A2(_op["::"],_p20._0,spersed);
         }
   });
   var repeatHelp = F3(function (result,n,value) {
      repeatHelp: while (true) if (_U.cmp(n,0) < 1) return result; else {
            var _v18 = A2(_op["::"],value,result),_v19 = n - 1,_v20 = value;
            result = _v18;
            n = _v19;
            value = _v20;
            continue repeatHelp;
         }
   });
   var repeat = F2(function (n,value) {    return A3(repeatHelp,_U.list([]),n,value);});
   return _elm.List.values = {_op: _op
                             ,isEmpty: isEmpty
                             ,length: length
                             ,reverse: reverse
                             ,member: member
                             ,head: head
                             ,tail: tail
                             ,filter: filter
                             ,take: take
                             ,drop: drop
                             ,repeat: repeat
                             ,append: append
                             ,concat: concat
                             ,intersperse: intersperse
                             ,partition: partition
                             ,unzip: unzip
                             ,map: map
                             ,map2: map2
                             ,map3: map3
                             ,map4: map4
                             ,map5: map5
                             ,filterMap: filterMap
                             ,concatMap: concatMap
                             ,indexedMap: indexedMap
                             ,foldr: foldr
                             ,foldl: foldl
                             ,sum: sum
                             ,product: product
                             ,maximum: maximum
                             ,minimum: minimum
                             ,all: all
                             ,any: any
                             ,scanl: scanl
                             ,sort: sort
                             ,sortBy: sortBy
                             ,sortWith: sortWith};
};
Elm.Native.Transform2D = {};
Elm.Native.Transform2D.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Transform2D = localRuntime.Native.Transform2D || {};
	if (localRuntime.Native.Transform2D.values)
	{
		return localRuntime.Native.Transform2D.values;
	}

	var A;
	if (typeof Float32Array === 'undefined')
	{
		A = function(arr)
		{
			this.length = arr.length;
			this[0] = arr[0];
			this[1] = arr[1];
			this[2] = arr[2];
			this[3] = arr[3];
			this[4] = arr[4];
			this[5] = arr[5];
		};
	}
	else
	{
		A = Float32Array;
	}

	// layout of matrix in an array is
	//
	//   | m11 m12 dx |
	//   | m21 m22 dy |
	//   |  0   0   1 |
	//
	//  new A([ m11, m12, dx, m21, m22, dy ])

	var identity = new A([1, 0, 0, 0, 1, 0]);
	function matrix(m11, m12, m21, m22, dx, dy)
	{
		return new A([m11, m12, dx, m21, m22, dy]);
	}

	function rotation(t)
	{
		var c = Math.cos(t);
		var s = Math.sin(t);
		return new A([c, -s, 0, s, c, 0]);
	}

	function rotate(t, m)
	{
		var c = Math.cos(t);
		var s = Math.sin(t);
		var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4];
		return new A([m11 * c + m12 * s, -m11 * s + m12 * c, m[2],
					  m21 * c + m22 * s, -m21 * s + m22 * c, m[5]]);
	}
	/*
	function move(xy,m) {
		var x = xy._0;
		var y = xy._1;
		var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4];
		return new A([m11, m12, m11*x + m12*y + m[2],
					  m21, m22, m21*x + m22*y + m[5]]);
	}
	function scale(s,m) { return new A([m[0]*s, m[1]*s, m[2], m[3]*s, m[4]*s, m[5]]); }
	function scaleX(x,m) { return new A([m[0]*x, m[1], m[2], m[3]*x, m[4], m[5]]); }
	function scaleY(y,m) { return new A([m[0], m[1]*y, m[2], m[3], m[4]*y, m[5]]); }
	function reflectX(m) { return new A([-m[0], m[1], m[2], -m[3], m[4], m[5]]); }
	function reflectY(m) { return new A([m[0], -m[1], m[2], m[3], -m[4], m[5]]); }

	function transform(m11, m21, m12, m22, mdx, mdy, n) {
		var n11 = n[0], n12 = n[1], n21 = n[3], n22 = n[4], ndx = n[2], ndy = n[5];
		return new A([m11*n11 + m12*n21,
					  m11*n12 + m12*n22,
					  m11*ndx + m12*ndy + mdx,
					  m21*n11 + m22*n21,
					  m21*n12 + m22*n22,
					  m21*ndx + m22*ndy + mdy]);
	}
	*/
	function multiply(m, n)
	{
		var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4], mdx = m[2], mdy = m[5];
		var n11 = n[0], n12 = n[1], n21 = n[3], n22 = n[4], ndx = n[2], ndy = n[5];
		return new A([m11 * n11 + m12 * n21,
					  m11 * n12 + m12 * n22,
					  m11 * ndx + m12 * ndy + mdx,
					  m21 * n11 + m22 * n21,
					  m21 * n12 + m22 * n22,
					  m21 * ndx + m22 * ndy + mdy]);
	}

	return localRuntime.Native.Transform2D.values = {
		identity: identity,
		matrix: F6(matrix),
		rotation: rotation,
		multiply: F2(multiply)
		/*
		transform: F7(transform),
		rotate: F2(rotate),
		move: F2(move),
		scale: F2(scale),
		scaleX: F2(scaleX),
		scaleY: F2(scaleY),
		reflectX: reflectX,
		reflectY: reflectY
		*/
	};
};

Elm.Transform2D = Elm.Transform2D || {};
Elm.Transform2D.make = function (_elm) {
   "use strict";
   _elm.Transform2D = _elm.Transform2D || {};
   if (_elm.Transform2D.values) return _elm.Transform2D.values;
   var _U = Elm.Native.Utils.make(_elm),$Native$Transform2D = Elm.Native.Transform2D.make(_elm);
   var _op = {};
   var multiply = $Native$Transform2D.multiply;
   var rotation = $Native$Transform2D.rotation;
   var matrix = $Native$Transform2D.matrix;
   var translation = F2(function (x,y) {    return A6(matrix,1,0,0,1,x,y);});
   var scale = function (s) {    return A6(matrix,s,0,0,s,0,0);};
   var scaleX = function (x) {    return A6(matrix,x,0,0,1,0,0);};
   var scaleY = function (y) {    return A6(matrix,1,0,0,y,0,0);};
   var identity = $Native$Transform2D.identity;
   var Transform2D = {ctor: "Transform2D"};
   return _elm.Transform2D.values = {_op: _op
                                    ,identity: identity
                                    ,matrix: matrix
                                    ,multiply: multiply
                                    ,rotation: rotation
                                    ,translation: translation
                                    ,scale: scale
                                    ,scaleX: scaleX
                                    ,scaleY: scaleY};
};

// setup
Elm.Native = Elm.Native || {};
Elm.Native.Graphics = Elm.Native.Graphics || {};
Elm.Native.Graphics.Collage = Elm.Native.Graphics.Collage || {};

// definition
Elm.Native.Graphics.Collage.make = function(localRuntime) {
	'use strict';

	// attempt to short-circuit
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Graphics = localRuntime.Native.Graphics || {};
	localRuntime.Native.Graphics.Collage = localRuntime.Native.Graphics.Collage || {};
	if ('values' in localRuntime.Native.Graphics.Collage)
	{
		return localRuntime.Native.Graphics.Collage.values;
	}

	// okay, we cannot short-ciruit, so now we define everything
	var Color = Elm.Native.Color.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var NativeElement = Elm.Native.Graphics.Element.make(localRuntime);
	var Transform = Elm.Transform2D.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);

	function setStrokeStyle(ctx, style)
	{
		ctx.lineWidth = style.width;

		var cap = style.cap.ctor;
		ctx.lineCap = cap === 'Flat'
			? 'butt'
			: cap === 'Round'
				? 'round'
				: 'square';

		var join = style.join.ctor;
		ctx.lineJoin = join === 'Smooth'
			? 'round'
			: join === 'Sharp'
				? 'miter'
				: 'bevel';

		ctx.miterLimit = style.join._0 || 10;
		ctx.strokeStyle = Color.toCss(style.color);
	}

	function setFillStyle(redo, ctx, style)
	{
		var sty = style.ctor;
		ctx.fillStyle = sty === 'Solid'
			? Color.toCss(style._0)
			: sty === 'Texture'
				? texture(redo, ctx, style._0)
				: gradient(ctx, style._0);
	}

	function trace(ctx, path)
	{
		var points = List.toArray(path);
		var i = points.length - 1;
		if (i <= 0)
		{
			return;
		}
		ctx.moveTo(points[i]._0, points[i]._1);
		while (i--)
		{
			ctx.lineTo(points[i]._0, points[i]._1);
		}
		if (path.closed)
		{
			i = points.length - 1;
			ctx.lineTo(points[i]._0, points[i]._1);
		}
	}

	function line(ctx, style, path)
	{
		if (style.dashing.ctor === '[]')
		{
			trace(ctx, path);
		}
		else
		{
			customLineHelp(ctx, style, path);
		}
		ctx.scale(1, -1);
		ctx.stroke();
	}

	function customLineHelp(ctx, style, path)
	{
		var points = List.toArray(path);
		if (path.closed)
		{
			points.push(points[0]);
		}
		var pattern = List.toArray(style.dashing);
		var i = points.length - 1;
		if (i <= 0)
		{
			return;
		}
		var x0 = points[i]._0, y0 = points[i]._1;
		var x1 = 0, y1 = 0, dx = 0, dy = 0, remaining = 0;
		var pindex = 0, plen = pattern.length;
		var draw = true, segmentLength = pattern[0];
		ctx.moveTo(x0, y0);
		while (i--)
		{
			x1 = points[i]._0;
			y1 = points[i]._1;
			dx = x1 - x0;
			dy = y1 - y0;
			remaining = Math.sqrt(dx * dx + dy * dy);
			while (segmentLength <= remaining)
			{
				x0 += dx * segmentLength / remaining;
				y0 += dy * segmentLength / remaining;
				ctx[draw ? 'lineTo' : 'moveTo'](x0, y0);
				// update starting position
				dx = x1 - x0;
				dy = y1 - y0;
				remaining = Math.sqrt(dx * dx + dy * dy);
				// update pattern
				draw = !draw;
				pindex = (pindex + 1) % plen;
				segmentLength = pattern[pindex];
			}
			if (remaining > 0)
			{
				ctx[draw ? 'lineTo' : 'moveTo'](x1, y1);
				segmentLength -= remaining;
			}
			x0 = x1;
			y0 = y1;
		}
	}

	function drawLine(ctx, style, path)
	{
		setStrokeStyle(ctx, style);
		return line(ctx, style, path);
	}

	function texture(redo, ctx, src)
	{
		var img = new Image();
		img.src = src;
		img.onload = redo;
		return ctx.createPattern(img, 'repeat');
	}

	function gradient(ctx, grad)
	{
		var g;
		var stops = [];
		if (grad.ctor === 'Linear')
		{
			var p0 = grad._0, p1 = grad._1;
			g = ctx.createLinearGradient(p0._0, -p0._1, p1._0, -p1._1);
			stops = List.toArray(grad._2);
		}
		else
		{
			var p0 = grad._0, p2 = grad._2;
			g = ctx.createRadialGradient(p0._0, -p0._1, grad._1, p2._0, -p2._1, grad._3);
			stops = List.toArray(grad._4);
		}
		var len = stops.length;
		for (var i = 0; i < len; ++i)
		{
			var stop = stops[i];
			g.addColorStop(stop._0, Color.toCss(stop._1));
		}
		return g;
	}

	function drawShape(redo, ctx, style, path)
	{
		trace(ctx, path);
		setFillStyle(redo, ctx, style);
		ctx.scale(1, -1);
		ctx.fill();
	}


	// TEXT RENDERING

	function fillText(redo, ctx, text)
	{
		drawText(ctx, text, ctx.fillText);
	}

	function strokeText(redo, ctx, style, text)
	{
		setStrokeStyle(ctx, style);
		// Use native canvas API for dashes only for text for now
		// Degrades to non-dashed on IE 9 + 10
		if (style.dashing.ctor !== '[]' && ctx.setLineDash)
		{
			var pattern = List.toArray(style.dashing);
			ctx.setLineDash(pattern);
		}
		drawText(ctx, text, ctx.strokeText);
	}

	function drawText(ctx, text, canvasDrawFn)
	{
		var textChunks = chunkText(defaultContext, text);

		var totalWidth = 0;
		var maxHeight = 0;
		var numChunks = textChunks.length;

		ctx.scale(1,-1);

		for (var i = numChunks; i--; )
		{
			var chunk = textChunks[i];
			ctx.font = chunk.font;
			var metrics = ctx.measureText(chunk.text);
			chunk.width = metrics.width;
			totalWidth += chunk.width;
			if (chunk.height > maxHeight)
			{
				maxHeight = chunk.height;
			}
		}

		var x = -totalWidth / 2.0;
		for (var i = 0; i < numChunks; ++i)
		{
			var chunk = textChunks[i];
			ctx.font = chunk.font;
			ctx.fillStyle = chunk.color;
			canvasDrawFn.call(ctx, chunk.text, x, maxHeight / 2);
			x += chunk.width;
		}
	}

	function toFont(props)
	{
		return [
			props['font-style'],
			props['font-variant'],
			props['font-weight'],
			props['font-size'],
			props['font-family']
		].join(' ');
	}


	// Convert the object returned by the text module
	// into something we can use for styling canvas text
	function chunkText(context, text)
	{
		var tag = text.ctor;
		if (tag === 'Text:Append')
		{
			var leftChunks = chunkText(context, text._0);
			var rightChunks = chunkText(context, text._1);
			return leftChunks.concat(rightChunks);
		}
		if (tag === 'Text:Text')
		{
			return [{
				text: text._0,
				color: context.color,
				height: context['font-size'].slice(0, -2) | 0,
				font: toFont(context)
			}];
		}
		if (tag === 'Text:Meta')
		{
			var newContext = freshContext(text._0, context);
			return chunkText(newContext, text._1);
		}
	}

	function freshContext(props, ctx)
	{
		return {
			'font-style': props['font-style'] || ctx['font-style'],
			'font-variant': props['font-variant'] || ctx['font-variant'],
			'font-weight': props['font-weight'] || ctx['font-weight'],
			'font-size': props['font-size'] || ctx['font-size'],
			'font-family': props['font-family'] || ctx['font-family'],
			'color': props['color'] || ctx['color']
		};
	}

	var defaultContext = {
		'font-style': 'normal',
		'font-variant': 'normal',
		'font-weight': 'normal',
		'font-size': '12px',
		'font-family': 'sans-serif',
		'color': 'black'
	};


	// IMAGES

	function drawImage(redo, ctx, form)
	{
		var img = new Image();
		img.onload = redo;
		img.src = form._3;
		var w = form._0,
			h = form._1,
			pos = form._2,
			srcX = pos._0,
			srcY = pos._1,
			srcW = w,
			srcH = h,
			destX = -w / 2,
			destY = -h / 2,
			destW = w,
			destH = h;

		ctx.scale(1, -1);
		ctx.drawImage(img, srcX, srcY, srcW, srcH, destX, destY, destW, destH);
	}

	function renderForm(redo, ctx, form)
	{
		ctx.save();

		var x = form.x,
			y = form.y,
			theta = form.theta,
			scale = form.scale;

		if (x !== 0 || y !== 0)
		{
			ctx.translate(x, y);
		}
		if (theta !== 0)
		{
			ctx.rotate(theta % (Math.PI * 2));
		}
		if (scale !== 1)
		{
			ctx.scale(scale, scale);
		}
		if (form.alpha !== 1)
		{
			ctx.globalAlpha = ctx.globalAlpha * form.alpha;
		}

		ctx.beginPath();
		var f = form.form;
		switch (f.ctor)
		{
			case 'FPath':
				drawLine(ctx, f._0, f._1);
				break;

			case 'FImage':
				drawImage(redo, ctx, f);
				break;

			case 'FShape':
				if (f._0.ctor === 'Line')
				{
					f._1.closed = true;
					drawLine(ctx, f._0._0, f._1);
				}
				else
				{
					drawShape(redo, ctx, f._0._0, f._1);
				}
				break;

			case 'FText':
				fillText(redo, ctx, f._0);
				break;

			case 'FOutlinedText':
				strokeText(redo, ctx, f._0, f._1);
				break;
		}
		ctx.restore();
	}

	function formToMatrix(form)
	{
	   var scale = form.scale;
	   var matrix = A6( Transform.matrix, scale, 0, 0, scale, form.x, form.y );

	   var theta = form.theta;
	   if (theta !== 0)
	   {
		   matrix = A2( Transform.multiply, matrix, Transform.rotation(theta) );
	   }

	   return matrix;
	}

	function str(n)
	{
		if (n < 0.00001 && n > -0.00001)
		{
			return 0;
		}
		return n;
	}

	function makeTransform(w, h, form, matrices)
	{
		var props = form.form._0._0.props;
		var m = A6( Transform.matrix, 1, 0, 0, -1,
					(w - props.width ) / 2,
					(h - props.height) / 2 );
		var len = matrices.length;
		for (var i = 0; i < len; ++i)
		{
			m = A2( Transform.multiply, m, matrices[i] );
		}
		m = A2( Transform.multiply, m, formToMatrix(form) );

		return 'matrix(' +
			str( m[0]) + ', ' + str( m[3]) + ', ' +
			str(-m[1]) + ', ' + str(-m[4]) + ', ' +
			str( m[2]) + ', ' + str( m[5]) + ')';
	}

	function stepperHelp(list)
	{
		var arr = List.toArray(list);
		var i = 0;
		function peekNext()
		{
			return i < arr.length ? arr[i]._0.form.ctor : '';
		}
		// assumes that there is a next element
		function next()
		{
			var out = arr[i]._0;
			++i;
			return out;
		}
		return {
			peekNext: peekNext,
			next: next
		};
	}

	function formStepper(forms)
	{
		var ps = [stepperHelp(forms)];
		var matrices = [];
		var alphas = [];
		function peekNext()
		{
			var len = ps.length;
			var formType = '';
			for (var i = 0; i < len; ++i )
			{
				if (formType = ps[i].peekNext()) return formType;
			}
			return '';
		}
		// assumes that there is a next element
		function next(ctx)
		{
			while (!ps[0].peekNext())
			{
				ps.shift();
				matrices.pop();
				alphas.shift();
				if (ctx)
				{
					ctx.restore();
				}
			}
			var out = ps[0].next();
			var f = out.form;
			if (f.ctor === 'FGroup')
			{
				ps.unshift(stepperHelp(f._1));
				var m = A2(Transform.multiply, f._0, formToMatrix(out));
				ctx.save();
				ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
				matrices.push(m);

				var alpha = (alphas[0] || 1) * out.alpha;
				alphas.unshift(alpha);
				ctx.globalAlpha = alpha;
			}
			return out;
		}
		function transforms()
		{
			return matrices;
		}
		function alpha()
		{
			return alphas[0] || 1;
		}
		return {
			peekNext: peekNext,
			next: next,
			transforms: transforms,
			alpha: alpha
		};
	}

	function makeCanvas(w, h)
	{
		var canvas = NativeElement.createNode('canvas');
		canvas.style.width  = w + 'px';
		canvas.style.height = h + 'px';
		canvas.style.display = 'block';
		canvas.style.position = 'absolute';
		var ratio = window.devicePixelRatio || 1;
		canvas.width  = w * ratio;
		canvas.height = h * ratio;
		return canvas;
	}

	function render(model)
	{
		var div = NativeElement.createNode('div');
		div.style.overflow = 'hidden';
		div.style.position = 'relative';
		update(div, model, model);
		return div;
	}

	function nodeStepper(w, h, div)
	{
		var kids = div.childNodes;
		var i = 0;
		var ratio = window.devicePixelRatio || 1;

		function transform(transforms, ctx)
		{
			ctx.translate( w / 2 * ratio, h / 2 * ratio );
			ctx.scale( ratio, -ratio );
			var len = transforms.length;
			for (var i = 0; i < len; ++i)
			{
				var m = transforms[i];
				ctx.save();
				ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
			}
			return ctx;
		}
		function nextContext(transforms)
		{
			while (i < kids.length)
			{
				var node = kids[i];
				if (node.getContext)
				{
					node.width = w * ratio;
					node.height = h * ratio;
					node.style.width = w + 'px';
					node.style.height = h + 'px';
					++i;
					return transform(transforms, node.getContext('2d'));
				}
				div.removeChild(node);
			}
			var canvas = makeCanvas(w, h);
			div.appendChild(canvas);
			// we have added a new node, so we must step our position
			++i;
			return transform(transforms, canvas.getContext('2d'));
		}
		function addElement(matrices, alpha, form)
		{
			var kid = kids[i];
			var elem = form.form._0;

			var node = (!kid || kid.getContext)
				? NativeElement.render(elem)
				: NativeElement.update(kid, kid.oldElement, elem);

			node.style.position = 'absolute';
			node.style.opacity = alpha * form.alpha * elem._0.props.opacity;
			NativeElement.addTransform(node.style, makeTransform(w, h, form, matrices));
			node.oldElement = elem;
			++i;
			if (!kid)
			{
				div.appendChild(node);
			}
			else
			{
				div.insertBefore(node, kid);
			}
		}
		function clearRest()
		{
			while (i < kids.length)
			{
				div.removeChild(kids[i]);
			}
		}
		return {
			nextContext: nextContext,
			addElement: addElement,
			clearRest: clearRest
		};
	}


	function update(div, _, model)
	{
		var w = model.w;
		var h = model.h;

		var forms = formStepper(model.forms);
		var nodes = nodeStepper(w, h, div);
		var ctx = null;
		var formType = '';

		while (formType = forms.peekNext())
		{
			// make sure we have context if we need it
			if (ctx === null && formType !== 'FElement')
			{
				ctx = nodes.nextContext(forms.transforms());
				ctx.globalAlpha = forms.alpha();
			}

			var form = forms.next(ctx);
			// if it is FGroup, all updates are made within formStepper when next is called.
			if (formType === 'FElement')
			{
				// update or insert an element, get a new context
				nodes.addElement(forms.transforms(), forms.alpha(), form);
				ctx = null;
			}
			else if (formType !== 'FGroup')
			{
				renderForm(function() { update(div, model, model); }, ctx, form);
			}
		}
		nodes.clearRest();
		return div;
	}


	function collage(w, h, forms)
	{
		return A3(NativeElement.newElement, w, h, {
			ctor: 'Custom',
			type: 'Collage',
			render: render,
			update: update,
			model: {w: w, h: h, forms: forms}
		});
	}

	return localRuntime.Native.Graphics.Collage.values = {
		collage: F3(collage)
	};
};

Elm.Native.Color = {};
Elm.Native.Color.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Color = localRuntime.Native.Color || {};
	if (localRuntime.Native.Color.values)
	{
		return localRuntime.Native.Color.values;
	}

	function toCss(c)
	{
		var format = '';
		var colors = '';
		if (c.ctor === 'RGBA')
		{
			format = 'rgb';
			colors = c._0 + ', ' + c._1 + ', ' + c._2;
		}
		else
		{
			format = 'hsl';
			colors = (c._0 * 180 / Math.PI) + ', ' +
					 (c._1 * 100) + '%, ' +
					 (c._2 * 100) + '%';
		}
		if (c._3 === 1)
		{
			return format + '(' + colors + ')';
		}
		else
		{
			return format + 'a(' + colors + ', ' + c._3 + ')';
		}
	}

	return localRuntime.Native.Color.values = {
		toCss: toCss
	};
};

Elm.Color = Elm.Color || {};
Elm.Color.make = function (_elm) {
   "use strict";
   _elm.Color = _elm.Color || {};
   if (_elm.Color.values) return _elm.Color.values;
   var _U = Elm.Native.Utils.make(_elm),$Basics = Elm.Basics.make(_elm);
   var _op = {};
   var Radial = F5(function (a,b,c,d,e) {    return {ctor: "Radial",_0: a,_1: b,_2: c,_3: d,_4: e};});
   var radial = Radial;
   var Linear = F3(function (a,b,c) {    return {ctor: "Linear",_0: a,_1: b,_2: c};});
   var linear = Linear;
   var fmod = F2(function (f,n) {    var integer = $Basics.floor(f);return $Basics.toFloat(A2($Basics._op["%"],integer,n)) + f - $Basics.toFloat(integer);});
   var rgbToHsl = F3(function (red,green,blue) {
      var b = $Basics.toFloat(blue) / 255;
      var g = $Basics.toFloat(green) / 255;
      var r = $Basics.toFloat(red) / 255;
      var cMax = A2($Basics.max,A2($Basics.max,r,g),b);
      var cMin = A2($Basics.min,A2($Basics.min,r,g),b);
      var c = cMax - cMin;
      var lightness = (cMax + cMin) / 2;
      var saturation = _U.eq(lightness,0) ? 0 : c / (1 - $Basics.abs(2 * lightness - 1));
      var hue = $Basics.degrees(60) * (_U.eq(cMax,r) ? A2(fmod,(g - b) / c,6) : _U.eq(cMax,g) ? (b - r) / c + 2 : (r - g) / c + 4);
      return {ctor: "_Tuple3",_0: hue,_1: saturation,_2: lightness};
   });
   var hslToRgb = F3(function (hue,saturation,lightness) {
      var hue$ = hue / $Basics.degrees(60);
      var chroma = (1 - $Basics.abs(2 * lightness - 1)) * saturation;
      var x = chroma * (1 - $Basics.abs(A2(fmod,hue$,2) - 1));
      var _p0 = _U.cmp(hue$,0) < 0 ? {ctor: "_Tuple3",_0: 0,_1: 0,_2: 0} : _U.cmp(hue$,1) < 0 ? {ctor: "_Tuple3",_0: chroma,_1: x,_2: 0} : _U.cmp(hue$,
      2) < 0 ? {ctor: "_Tuple3",_0: x,_1: chroma,_2: 0} : _U.cmp(hue$,3) < 0 ? {ctor: "_Tuple3",_0: 0,_1: chroma,_2: x} : _U.cmp(hue$,4) < 0 ? {ctor: "_Tuple3"
                                                                                                                                               ,_0: 0
                                                                                                                                               ,_1: x
                                                                                                                                               ,_2: chroma} : _U.cmp(hue$,
      5) < 0 ? {ctor: "_Tuple3",_0: x,_1: 0,_2: chroma} : _U.cmp(hue$,6) < 0 ? {ctor: "_Tuple3",_0: chroma,_1: 0,_2: x} : {ctor: "_Tuple3",_0: 0,_1: 0,_2: 0};
      var r = _p0._0;
      var g = _p0._1;
      var b = _p0._2;
      var m = lightness - chroma / 2;
      return {ctor: "_Tuple3",_0: r + m,_1: g + m,_2: b + m};
   });
   var toRgb = function (color) {
      var _p1 = color;
      if (_p1.ctor === "RGBA") {
            return {red: _p1._0,green: _p1._1,blue: _p1._2,alpha: _p1._3};
         } else {
            var _p2 = A3(hslToRgb,_p1._0,_p1._1,_p1._2);
            var r = _p2._0;
            var g = _p2._1;
            var b = _p2._2;
            return {red: $Basics.round(255 * r),green: $Basics.round(255 * g),blue: $Basics.round(255 * b),alpha: _p1._3};
         }
   };
   var toHsl = function (color) {
      var _p3 = color;
      if (_p3.ctor === "HSLA") {
            return {hue: _p3._0,saturation: _p3._1,lightness: _p3._2,alpha: _p3._3};
         } else {
            var _p4 = A3(rgbToHsl,_p3._0,_p3._1,_p3._2);
            var h = _p4._0;
            var s = _p4._1;
            var l = _p4._2;
            return {hue: h,saturation: s,lightness: l,alpha: _p3._3};
         }
   };
   var HSLA = F4(function (a,b,c,d) {    return {ctor: "HSLA",_0: a,_1: b,_2: c,_3: d};});
   var hsla = F4(function (hue,saturation,lightness,alpha) {
      return A4(HSLA,hue - $Basics.turns($Basics.toFloat($Basics.floor(hue / (2 * $Basics.pi)))),saturation,lightness,alpha);
   });
   var hsl = F3(function (hue,saturation,lightness) {    return A4(hsla,hue,saturation,lightness,1);});
   var complement = function (color) {
      var _p5 = color;
      if (_p5.ctor === "HSLA") {
            return A4(hsla,_p5._0 + $Basics.degrees(180),_p5._1,_p5._2,_p5._3);
         } else {
            var _p6 = A3(rgbToHsl,_p5._0,_p5._1,_p5._2);
            var h = _p6._0;
            var s = _p6._1;
            var l = _p6._2;
            return A4(hsla,h + $Basics.degrees(180),s,l,_p5._3);
         }
   };
   var grayscale = function (p) {    return A4(HSLA,0,0,1 - p,1);};
   var greyscale = function (p) {    return A4(HSLA,0,0,1 - p,1);};
   var RGBA = F4(function (a,b,c,d) {    return {ctor: "RGBA",_0: a,_1: b,_2: c,_3: d};});
   var rgba = RGBA;
   var rgb = F3(function (r,g,b) {    return A4(RGBA,r,g,b,1);});
   var lightRed = A4(RGBA,239,41,41,1);
   var red = A4(RGBA,204,0,0,1);
   var darkRed = A4(RGBA,164,0,0,1);
   var lightOrange = A4(RGBA,252,175,62,1);
   var orange = A4(RGBA,245,121,0,1);
   var darkOrange = A4(RGBA,206,92,0,1);
   var lightYellow = A4(RGBA,255,233,79,1);
   var yellow = A4(RGBA,237,212,0,1);
   var darkYellow = A4(RGBA,196,160,0,1);
   var lightGreen = A4(RGBA,138,226,52,1);
   var green = A4(RGBA,115,210,22,1);
   var darkGreen = A4(RGBA,78,154,6,1);
   var lightBlue = A4(RGBA,114,159,207,1);
   var blue = A4(RGBA,52,101,164,1);
   var darkBlue = A4(RGBA,32,74,135,1);
   var lightPurple = A4(RGBA,173,127,168,1);
   var purple = A4(RGBA,117,80,123,1);
   var darkPurple = A4(RGBA,92,53,102,1);
   var lightBrown = A4(RGBA,233,185,110,1);
   var brown = A4(RGBA,193,125,17,1);
   var darkBrown = A4(RGBA,143,89,2,1);
   var black = A4(RGBA,0,0,0,1);
   var white = A4(RGBA,255,255,255,1);
   var lightGrey = A4(RGBA,238,238,236,1);
   var grey = A4(RGBA,211,215,207,1);
   var darkGrey = A4(RGBA,186,189,182,1);
   var lightGray = A4(RGBA,238,238,236,1);
   var gray = A4(RGBA,211,215,207,1);
   var darkGray = A4(RGBA,186,189,182,1);
   var lightCharcoal = A4(RGBA,136,138,133,1);
   var charcoal = A4(RGBA,85,87,83,1);
   var darkCharcoal = A4(RGBA,46,52,54,1);
   return _elm.Color.values = {_op: _op
                              ,rgb: rgb
                              ,rgba: rgba
                              ,hsl: hsl
                              ,hsla: hsla
                              ,greyscale: greyscale
                              ,grayscale: grayscale
                              ,complement: complement
                              ,linear: linear
                              ,radial: radial
                              ,toRgb: toRgb
                              ,toHsl: toHsl
                              ,red: red
                              ,orange: orange
                              ,yellow: yellow
                              ,green: green
                              ,blue: blue
                              ,purple: purple
                              ,brown: brown
                              ,lightRed: lightRed
                              ,lightOrange: lightOrange
                              ,lightYellow: lightYellow
                              ,lightGreen: lightGreen
                              ,lightBlue: lightBlue
                              ,lightPurple: lightPurple
                              ,lightBrown: lightBrown
                              ,darkRed: darkRed
                              ,darkOrange: darkOrange
                              ,darkYellow: darkYellow
                              ,darkGreen: darkGreen
                              ,darkBlue: darkBlue
                              ,darkPurple: darkPurple
                              ,darkBrown: darkBrown
                              ,white: white
                              ,lightGrey: lightGrey
                              ,grey: grey
                              ,darkGrey: darkGrey
                              ,lightCharcoal: lightCharcoal
                              ,charcoal: charcoal
                              ,darkCharcoal: darkCharcoal
                              ,black: black
                              ,lightGray: lightGray
                              ,gray: gray
                              ,darkGray: darkGray};
};

// setup
Elm.Native = Elm.Native || {};
Elm.Native.Graphics = Elm.Native.Graphics || {};
Elm.Native.Graphics.Element = Elm.Native.Graphics.Element || {};

// definition
Elm.Native.Graphics.Element.make = function(localRuntime) {
	'use strict';

	// attempt to short-circuit
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Graphics = localRuntime.Native.Graphics || {};
	localRuntime.Native.Graphics.Element = localRuntime.Native.Graphics.Element || {};
	if ('values' in localRuntime.Native.Graphics.Element)
	{
		return localRuntime.Native.Graphics.Element.values;
	}

	var Color = Elm.Native.Color.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);
	var Text = Elm.Native.Text.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);


	// CREATION

	var createNode =
		typeof document === 'undefined'
			?
				function(_)
				{
					return {
						style: {},
						appendChild: function() {}
					};
				}
			:
				function(elementType)
				{
					var node = document.createElement(elementType);
					node.style.padding = '0';
					node.style.margin = '0';
					return node;
				}
			;


	function newElement(width, height, elementPrim)
	{
		return {
			ctor: 'Element_elm_builtin',
			_0: {
				element: elementPrim,
				props: {
					id: Utils.guid(),
					width: width,
					height: height,
					opacity: 1,
					color: Maybe.Nothing,
					href: '',
					tag: '',
					hover: Utils.Tuple0,
					click: Utils.Tuple0
				}
			}
		};
	}


	// PROPERTIES

	function setProps(elem, node)
	{
		var props = elem.props;

		var element = elem.element;
		var width = props.width - (element.adjustWidth || 0);
		var height = props.height - (element.adjustHeight || 0);
		node.style.width  = (width | 0) + 'px';
		node.style.height = (height | 0) + 'px';

		if (props.opacity !== 1)
		{
			node.style.opacity = props.opacity;
		}

		if (props.color.ctor === 'Just')
		{
			node.style.backgroundColor = Color.toCss(props.color._0);
		}

		if (props.tag !== '')
		{
			node.id = props.tag;
		}

		if (props.hover.ctor !== '_Tuple0')
		{
			addHover(node, props.hover);
		}

		if (props.click.ctor !== '_Tuple0')
		{
			addClick(node, props.click);
		}

		if (props.href !== '')
		{
			var anchor = createNode('a');
			anchor.href = props.href;
			anchor.style.display = 'block';
			anchor.style.pointerEvents = 'auto';
			anchor.appendChild(node);
			node = anchor;
		}

		return node;
	}

	function addClick(e, handler)
	{
		e.style.pointerEvents = 'auto';
		e.elm_click_handler = handler;
		function trigger(ev)
		{
			e.elm_click_handler(Utils.Tuple0);
			ev.stopPropagation();
		}
		e.elm_click_trigger = trigger;
		e.addEventListener('click', trigger);
	}

	function removeClick(e, handler)
	{
		if (e.elm_click_trigger)
		{
			e.removeEventListener('click', e.elm_click_trigger);
			e.elm_click_trigger = null;
			e.elm_click_handler = null;
		}
	}

	function addHover(e, handler)
	{
		e.style.pointerEvents = 'auto';
		e.elm_hover_handler = handler;
		e.elm_hover_count = 0;

		function over(evt)
		{
			if (e.elm_hover_count++ > 0) return;
			e.elm_hover_handler(true);
			evt.stopPropagation();
		}
		function out(evt)
		{
			if (e.contains(evt.toElement || evt.relatedTarget)) return;
			e.elm_hover_count = 0;
			e.elm_hover_handler(false);
			evt.stopPropagation();
		}
		e.elm_hover_over = over;
		e.elm_hover_out = out;
		e.addEventListener('mouseover', over);
		e.addEventListener('mouseout', out);
	}

	function removeHover(e)
	{
		e.elm_hover_handler = null;
		if (e.elm_hover_over)
		{
			e.removeEventListener('mouseover', e.elm_hover_over);
			e.elm_hover_over = null;
		}
		if (e.elm_hover_out)
		{
			e.removeEventListener('mouseout', e.elm_hover_out);
			e.elm_hover_out = null;
		}
	}


	// IMAGES

	function image(props, img)
	{
		switch (img._0.ctor)
		{
			case 'Plain':
				return plainImage(img._3);

			case 'Fitted':
				return fittedImage(props.width, props.height, img._3);

			case 'Cropped':
				return croppedImage(img, props.width, props.height, img._3);

			case 'Tiled':
				return tiledImage(img._3);
		}
	}

	function plainImage(src)
	{
		var img = createNode('img');
		img.src = src;
		img.name = src;
		img.style.display = 'block';
		return img;
	}

	function tiledImage(src)
	{
		var div = createNode('div');
		div.style.backgroundImage = 'url(' + src + ')';
		return div;
	}

	function fittedImage(w, h, src)
	{
		var div = createNode('div');
		div.style.background = 'url(' + src + ') no-repeat center';
		div.style.webkitBackgroundSize = 'cover';
		div.style.MozBackgroundSize = 'cover';
		div.style.OBackgroundSize = 'cover';
		div.style.backgroundSize = 'cover';
		return div;
	}

	function croppedImage(elem, w, h, src)
	{
		var pos = elem._0._0;
		var e = createNode('div');
		e.style.overflow = 'hidden';

		var img = createNode('img');
		img.onload = function() {
			var sw = w / elem._1, sh = h / elem._2;
			img.style.width = ((this.width * sw) | 0) + 'px';
			img.style.height = ((this.height * sh) | 0) + 'px';
			img.style.marginLeft = ((- pos._0 * sw) | 0) + 'px';
			img.style.marginTop = ((- pos._1 * sh) | 0) + 'px';
		};
		img.src = src;
		img.name = src;
		e.appendChild(img);
		return e;
	}


	// FLOW

	function goOut(node)
	{
		node.style.position = 'absolute';
		return node;
	}
	function goDown(node)
	{
		return node;
	}
	function goRight(node)
	{
		node.style.styleFloat = 'left';
		node.style.cssFloat = 'left';
		return node;
	}

	var directionTable = {
		DUp: goDown,
		DDown: goDown,
		DLeft: goRight,
		DRight: goRight,
		DIn: goOut,
		DOut: goOut
	};
	function needsReversal(dir)
	{
		return dir === 'DUp' || dir === 'DLeft' || dir === 'DIn';
	}

	function flow(dir, elist)
	{
		var array = List.toArray(elist);
		var container = createNode('div');
		var goDir = directionTable[dir];
		if (goDir === goOut)
		{
			container.style.pointerEvents = 'none';
		}
		if (needsReversal(dir))
		{
			array.reverse();
		}
		var len = array.length;
		for (var i = 0; i < len; ++i)
		{
			container.appendChild(goDir(render(array[i])));
		}
		return container;
	}


	// CONTAINER

	function toPos(pos)
	{
		return pos.ctor === 'Absolute'
			? pos._0 + 'px'
			: (pos._0 * 100) + '%';
	}

	// must clear right, left, top, bottom, and transform
	// before calling this function
	function setPos(pos, wrappedElement, e)
	{
		var elem = wrappedElement._0;
		var element = elem.element;
		var props = elem.props;
		var w = props.width + (element.adjustWidth ? element.adjustWidth : 0);
		var h = props.height + (element.adjustHeight ? element.adjustHeight : 0);

		e.style.position = 'absolute';
		e.style.margin = 'auto';
		var transform = '';

		switch (pos.horizontal.ctor)
		{
			case 'P':
				e.style.right = toPos(pos.x);
				e.style.removeProperty('left');
				break;

			case 'Z':
				transform = 'translateX(' + ((-w / 2) | 0) + 'px) ';

			case 'N':
				e.style.left = toPos(pos.x);
				e.style.removeProperty('right');
				break;
		}
		switch (pos.vertical.ctor)
		{
			case 'N':
				e.style.bottom = toPos(pos.y);
				e.style.removeProperty('top');
				break;

			case 'Z':
				transform += 'translateY(' + ((-h / 2) | 0) + 'px)';

			case 'P':
				e.style.top = toPos(pos.y);
				e.style.removeProperty('bottom');
				break;
		}
		if (transform !== '')
		{
			addTransform(e.style, transform);
		}
		return e;
	}

	function addTransform(style, transform)
	{
		style.transform       = transform;
		style.msTransform     = transform;
		style.MozTransform    = transform;
		style.webkitTransform = transform;
		style.OTransform      = transform;
	}

	function container(pos, elem)
	{
		var e = render(elem);
		setPos(pos, elem, e);
		var div = createNode('div');
		div.style.position = 'relative';
		div.style.overflow = 'hidden';
		div.appendChild(e);
		return div;
	}


	function rawHtml(elem)
	{
		var html = elem.html;
		var align = elem.align;

		var div = createNode('div');
		div.innerHTML = html;
		div.style.visibility = 'hidden';
		if (align)
		{
			div.style.textAlign = align;
		}
		div.style.visibility = 'visible';
		div.style.pointerEvents = 'auto';
		return div;
	}


	// RENDER

	function render(wrappedElement)
	{
		var elem = wrappedElement._0;
		return setProps(elem, makeElement(elem));
	}

	function makeElement(e)
	{
		var elem = e.element;
		switch (elem.ctor)
		{
			case 'Image':
				return image(e.props, elem);

			case 'Flow':
				return flow(elem._0.ctor, elem._1);

			case 'Container':
				return container(elem._0, elem._1);

			case 'Spacer':
				return createNode('div');

			case 'RawHtml':
				return rawHtml(elem);

			case 'Custom':
				return elem.render(elem.model);
		}
	}

	function updateAndReplace(node, curr, next)
	{
		var newNode = update(node, curr, next);
		if (newNode !== node)
		{
			node.parentNode.replaceChild(newNode, node);
		}
		return newNode;
	}


	// UPDATE

	function update(node, wrappedCurrent, wrappedNext)
	{
		var curr = wrappedCurrent._0;
		var next = wrappedNext._0;
		var rootNode = node;
		if (node.tagName === 'A')
		{
			node = node.firstChild;
		}
		if (curr.props.id === next.props.id)
		{
			updateProps(node, curr, next);
			return rootNode;
		}
		if (curr.element.ctor !== next.element.ctor)
		{
			return render(wrappedNext);
		}
		var nextE = next.element;
		var currE = curr.element;
		switch (nextE.ctor)
		{
			case 'Spacer':
				updateProps(node, curr, next);
				return rootNode;

			case 'RawHtml':
				if(currE.html.valueOf() !== nextE.html.valueOf())
				{
					node.innerHTML = nextE.html;
				}
				updateProps(node, curr, next);
				return rootNode;

			case 'Image':
				if (nextE._0.ctor === 'Plain')
				{
					if (nextE._3 !== currE._3)
					{
						node.src = nextE._3;
					}
				}
				else if (!Utils.eq(nextE, currE)
					|| next.props.width !== curr.props.width
					|| next.props.height !== curr.props.height)
				{
					return render(wrappedNext);
				}
				updateProps(node, curr, next);
				return rootNode;

			case 'Flow':
				var arr = List.toArray(nextE._1);
				for (var i = arr.length; i--; )
				{
					arr[i] = arr[i]._0.element.ctor;
				}
				if (nextE._0.ctor !== currE._0.ctor)
				{
					return render(wrappedNext);
				}
				var nexts = List.toArray(nextE._1);
				var kids = node.childNodes;
				if (nexts.length !== kids.length)
				{
					return render(wrappedNext);
				}
				var currs = List.toArray(currE._1);
				var dir = nextE._0.ctor;
				var goDir = directionTable[dir];
				var toReverse = needsReversal(dir);
				var len = kids.length;
				for (var i = len; i--; )
				{
					var subNode = kids[toReverse ? len - i - 1 : i];
					goDir(updateAndReplace(subNode, currs[i], nexts[i]));
				}
				updateProps(node, curr, next);
				return rootNode;

			case 'Container':
				var subNode = node.firstChild;
				var newSubNode = updateAndReplace(subNode, currE._1, nextE._1);
				setPos(nextE._0, nextE._1, newSubNode);
				updateProps(node, curr, next);
				return rootNode;

			case 'Custom':
				if (currE.type === nextE.type)
				{
					var updatedNode = nextE.update(node, currE.model, nextE.model);
					updateProps(updatedNode, curr, next);
					return updatedNode;
				}
				return render(wrappedNext);
		}
	}

	function updateProps(node, curr, next)
	{
		var nextProps = next.props;
		var currProps = curr.props;

		var element = next.element;
		var width = nextProps.width - (element.adjustWidth || 0);
		var height = nextProps.height - (element.adjustHeight || 0);
		if (width !== currProps.width)
		{
			node.style.width = (width | 0) + 'px';
		}
		if (height !== currProps.height)
		{
			node.style.height = (height | 0) + 'px';
		}

		if (nextProps.opacity !== currProps.opacity)
		{
			node.style.opacity = nextProps.opacity;
		}

		var nextColor = nextProps.color.ctor === 'Just'
			? Color.toCss(nextProps.color._0)
			: '';
		if (node.style.backgroundColor !== nextColor)
		{
			node.style.backgroundColor = nextColor;
		}

		if (nextProps.tag !== currProps.tag)
		{
			node.id = nextProps.tag;
		}

		if (nextProps.href !== currProps.href)
		{
			if (currProps.href === '')
			{
				// add a surrounding href
				var anchor = createNode('a');
				anchor.href = nextProps.href;
				anchor.style.display = 'block';
				anchor.style.pointerEvents = 'auto';

				node.parentNode.replaceChild(anchor, node);
				anchor.appendChild(node);
			}
			else if (nextProps.href === '')
			{
				// remove the surrounding href
				var anchor = node.parentNode;
				anchor.parentNode.replaceChild(node, anchor);
			}
			else
			{
				// just update the link
				node.parentNode.href = nextProps.href;
			}
		}

		// update click and hover handlers
		var removed = false;

		// update hover handlers
		if (currProps.hover.ctor === '_Tuple0')
		{
			if (nextProps.hover.ctor !== '_Tuple0')
			{
				addHover(node, nextProps.hover);
			}
		}
		else
		{
			if (nextProps.hover.ctor === '_Tuple0')
			{
				removed = true;
				removeHover(node);
			}
			else
			{
				node.elm_hover_handler = nextProps.hover;
			}
		}

		// update click handlers
		if (currProps.click.ctor === '_Tuple0')
		{
			if (nextProps.click.ctor !== '_Tuple0')
			{
				addClick(node, nextProps.click);
			}
		}
		else
		{
			if (nextProps.click.ctor === '_Tuple0')
			{
				removed = true;
				removeClick(node);
			}
			else
			{
				node.elm_click_handler = nextProps.click;
			}
		}

		// stop capturing clicks if
		if (removed
			&& nextProps.hover.ctor === '_Tuple0'
			&& nextProps.click.ctor === '_Tuple0')
		{
			node.style.pointerEvents = 'none';
		}
	}


	// TEXT

	function block(align)
	{
		return function(text)
		{
			var raw = {
				ctor: 'RawHtml',
				html: Text.renderHtml(text),
				align: align
			};
			var pos = htmlHeight(0, raw);
			return newElement(pos._0, pos._1, raw);
		};
	}

	function markdown(text)
	{
		var raw = {
			ctor: 'RawHtml',
			html: text,
			align: null
		};
		var pos = htmlHeight(0, raw);
		return newElement(pos._0, pos._1, raw);
	}

	var htmlHeight =
		typeof document !== 'undefined'
			? realHtmlHeight
			: function(a, b) { return Utils.Tuple2(0, 0); };

	function realHtmlHeight(width, rawHtml)
	{
		// create dummy node
		var temp = document.createElement('div');
		temp.innerHTML = rawHtml.html;
		if (width > 0)
		{
			temp.style.width = width + 'px';
		}
		temp.style.visibility = 'hidden';
		temp.style.styleFloat = 'left';
		temp.style.cssFloat = 'left';

		document.body.appendChild(temp);

		// get dimensions
		var style = window.getComputedStyle(temp, null);
		var w = Math.ceil(style.getPropertyValue('width').slice(0, -2) - 0);
		var h = Math.ceil(style.getPropertyValue('height').slice(0, -2) - 0);
		document.body.removeChild(temp);
		return Utils.Tuple2(w, h);
	}


	return localRuntime.Native.Graphics.Element.values = {
		render: render,
		update: update,
		updateAndReplace: updateAndReplace,

		createNode: createNode,
		newElement: F3(newElement),
		addTransform: addTransform,
		htmlHeight: F2(htmlHeight),
		guid: Utils.guid,

		block: block,
		markdown: markdown
	};
};

Elm.Native.Text = {};
Elm.Native.Text.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Text = localRuntime.Native.Text || {};
	if (localRuntime.Native.Text.values)
	{
		return localRuntime.Native.Text.values;
	}

	var toCss = Elm.Native.Color.make(localRuntime).toCss;
	var List = Elm.Native.List.make(localRuntime);


	// CONSTRUCTORS

	function fromString(str)
	{
		return {
			ctor: 'Text:Text',
			_0: str
		};
	}

	function append(a, b)
	{
		return {
			ctor: 'Text:Append',
			_0: a,
			_1: b
		};
	}

	function addMeta(field, value, text)
	{
		var newProps = {};
		var newText = {
			ctor: 'Text:Meta',
			_0: newProps,
			_1: text
		};

		if (text.ctor === 'Text:Meta')
		{
			newText._1 = text._1;
			var props = text._0;
			for (var i = metaKeys.length; i--; )
			{
				var key = metaKeys[i];
				var val = props[key];
				if (val)
				{
					newProps[key] = val;
				}
			}
		}
		newProps[field] = value;
		return newText;
	}

	var metaKeys = [
		'font-size',
		'font-family',
		'font-style',
		'font-weight',
		'href',
		'text-decoration',
		'color'
	];


	// conversions from Elm values to CSS

	function toTypefaces(list)
	{
		var typefaces = List.toArray(list);
		for (var i = typefaces.length; i--; )
		{
			var typeface = typefaces[i];
			if (typeface.indexOf(' ') > -1)
			{
				typefaces[i] = "'" + typeface + "'";
			}
		}
		return typefaces.join(',');
	}

	function toLine(line)
	{
		var ctor = line.ctor;
		return ctor === 'Under'
			? 'underline'
			: ctor === 'Over'
				? 'overline'
				: 'line-through';
	}

	// setting styles of Text

	function style(style, text)
	{
		var newText = addMeta('color', toCss(style.color), text);
		var props = newText._0;

		if (style.typeface.ctor !== '[]')
		{
			props['font-family'] = toTypefaces(style.typeface);
		}
		if (style.height.ctor !== 'Nothing')
		{
			props['font-size'] = style.height._0 + 'px';
		}
		if (style.bold)
		{
			props['font-weight'] = 'bold';
		}
		if (style.italic)
		{
			props['font-style'] = 'italic';
		}
		if (style.line.ctor !== 'Nothing')
		{
			props['text-decoration'] = toLine(style.line._0);
		}
		return newText;
	}

	function height(px, text)
	{
		return addMeta('font-size', px + 'px', text);
	}

	function typeface(names, text)
	{
		return addMeta('font-family', toTypefaces(names), text);
	}

	function monospace(text)
	{
		return addMeta('font-family', 'monospace', text);
	}

	function italic(text)
	{
		return addMeta('font-style', 'italic', text);
	}

	function bold(text)
	{
		return addMeta('font-weight', 'bold', text);
	}

	function link(href, text)
	{
		return addMeta('href', href, text);
	}

	function line(line, text)
	{
		return addMeta('text-decoration', toLine(line), text);
	}

	function color(color, text)
	{
		return addMeta('color', toCss(color), text);
	}


	// RENDER

	function renderHtml(text)
	{
		var tag = text.ctor;
		if (tag === 'Text:Append')
		{
			return renderHtml(text._0) + renderHtml(text._1);
		}
		if (tag === 'Text:Text')
		{
			return properEscape(text._0);
		}
		if (tag === 'Text:Meta')
		{
			return renderMeta(text._0, renderHtml(text._1));
		}
	}

	function renderMeta(metas, string)
	{
		var href = metas.href;
		if (href)
		{
			string = '<a href="' + href + '">' + string + '</a>';
		}
		var styles = '';
		for (var key in metas)
		{
			if (key === 'href')
			{
				continue;
			}
			styles += key + ':' + metas[key] + ';';
		}
		if (styles)
		{
			string = '<span style="' + styles + '">' + string + '</span>';
		}
		return string;
	}

	function properEscape(str)
	{
		if (str.length === 0)
		{
			return str;
		}
		str = str //.replace(/&/g,  '&#38;')
			.replace(/"/g,  '&#34;')
			.replace(/'/g,  '&#39;')
			.replace(/</g,  '&#60;')
			.replace(/>/g,  '&#62;');
		var arr = str.split('\n');
		for (var i = arr.length; i--; )
		{
			arr[i] = makeSpaces(arr[i]);
		}
		return arr.join('<br/>');
	}

	function makeSpaces(s)
	{
		if (s.length === 0)
		{
			return s;
		}
		var arr = s.split('');
		if (arr[0] === ' ')
		{
			arr[0] = '&nbsp;';
		}
		for (var i = arr.length; --i; )
		{
			if (arr[i][0] === ' ' && arr[i - 1] === ' ')
			{
				arr[i - 1] = arr[i - 1] + arr[i];
				arr[i] = '';
			}
		}
		for (var i = arr.length; i--; )
		{
			if (arr[i].length > 1 && arr[i][0] === ' ')
			{
				var spaces = arr[i].split('');
				for (var j = spaces.length - 2; j >= 0; j -= 2)
				{
					spaces[j] = '&nbsp;';
				}
				arr[i] = spaces.join('');
			}
		}
		arr = arr.join('');
		if (arr[arr.length - 1] === ' ')
		{
			return arr.slice(0, -1) + '&nbsp;';
		}
		return arr;
	}


	return localRuntime.Native.Text.values = {
		fromString: fromString,
		append: F2(append),

		height: F2(height),
		italic: italic,
		bold: bold,
		line: F2(line),
		monospace: monospace,
		typeface: F2(typeface),
		color: F2(color),
		link: F2(link),
		style: F2(style),

		toTypefaces: toTypefaces,
		toLine: toLine,
		renderHtml: renderHtml
	};
};

Elm.Text = Elm.Text || {};
Elm.Text.make = function (_elm) {
   "use strict";
   _elm.Text = _elm.Text || {};
   if (_elm.Text.values) return _elm.Text.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Color = Elm.Color.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Text = Elm.Native.Text.make(_elm);
   var _op = {};
   var line = $Native$Text.line;
   var italic = $Native$Text.italic;
   var bold = $Native$Text.bold;
   var color = $Native$Text.color;
   var height = $Native$Text.height;
   var link = $Native$Text.link;
   var monospace = $Native$Text.monospace;
   var typeface = $Native$Text.typeface;
   var style = $Native$Text.style;
   var append = $Native$Text.append;
   var fromString = $Native$Text.fromString;
   var empty = fromString("");
   var concat = function (texts) {    return A3($List.foldr,append,empty,texts);};
   var join = F2(function (seperator,texts) {    return concat(A2($List.intersperse,seperator,texts));});
   var defaultStyle = {typeface: _U.list([]),height: $Maybe.Nothing,color: $Color.black,bold: false,italic: false,line: $Maybe.Nothing};
   var Style = F6(function (a,b,c,d,e,f) {    return {typeface: a,height: b,color: c,bold: d,italic: e,line: f};});
   var Through = {ctor: "Through"};
   var Over = {ctor: "Over"};
   var Under = {ctor: "Under"};
   var Text = {ctor: "Text"};
   return _elm.Text.values = {_op: _op
                             ,fromString: fromString
                             ,empty: empty
                             ,append: append
                             ,concat: concat
                             ,join: join
                             ,link: link
                             ,style: style
                             ,defaultStyle: defaultStyle
                             ,typeface: typeface
                             ,monospace: monospace
                             ,height: height
                             ,color: color
                             ,bold: bold
                             ,italic: italic
                             ,line: line
                             ,Style: Style
                             ,Under: Under
                             ,Over: Over
                             ,Through: Through};
};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Element = Elm.Graphics.Element || {};
Elm.Graphics.Element.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Element = _elm.Graphics.Element || {};
   if (_elm.Graphics.Element.values) return _elm.Graphics.Element.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Graphics$Element = Elm.Native.Graphics.Element.make(_elm),
   $Text = Elm.Text.make(_elm);
   var _op = {};
   var DOut = {ctor: "DOut"};
   var outward = DOut;
   var DIn = {ctor: "DIn"};
   var inward = DIn;
   var DRight = {ctor: "DRight"};
   var right = DRight;
   var DLeft = {ctor: "DLeft"};
   var left = DLeft;
   var DDown = {ctor: "DDown"};
   var down = DDown;
   var DUp = {ctor: "DUp"};
   var up = DUp;
   var RawPosition = F4(function (a,b,c,d) {    return {horizontal: a,vertical: b,x: c,y: d};});
   var Position = function (a) {    return {ctor: "Position",_0: a};};
   var Relative = function (a) {    return {ctor: "Relative",_0: a};};
   var relative = Relative;
   var Absolute = function (a) {    return {ctor: "Absolute",_0: a};};
   var absolute = Absolute;
   var N = {ctor: "N"};
   var bottomLeft = Position({horizontal: N,vertical: N,x: Absolute(0),y: Absolute(0)});
   var bottomLeftAt = F2(function (x,y) {    return Position({horizontal: N,vertical: N,x: x,y: y});});
   var Z = {ctor: "Z"};
   var middle = Position({horizontal: Z,vertical: Z,x: Relative(0.5),y: Relative(0.5)});
   var midLeft = Position({horizontal: N,vertical: Z,x: Absolute(0),y: Relative(0.5)});
   var midBottom = Position({horizontal: Z,vertical: N,x: Relative(0.5),y: Absolute(0)});
   var middleAt = F2(function (x,y) {    return Position({horizontal: Z,vertical: Z,x: x,y: y});});
   var midLeftAt = F2(function (x,y) {    return Position({horizontal: N,vertical: Z,x: x,y: y});});
   var midBottomAt = F2(function (x,y) {    return Position({horizontal: Z,vertical: N,x: x,y: y});});
   var P = {ctor: "P"};
   var topLeft = Position({horizontal: N,vertical: P,x: Absolute(0),y: Absolute(0)});
   var topRight = Position({horizontal: P,vertical: P,x: Absolute(0),y: Absolute(0)});
   var bottomRight = Position({horizontal: P,vertical: N,x: Absolute(0),y: Absolute(0)});
   var midRight = Position({horizontal: P,vertical: Z,x: Absolute(0),y: Relative(0.5)});
   var midTop = Position({horizontal: Z,vertical: P,x: Relative(0.5),y: Absolute(0)});
   var topLeftAt = F2(function (x,y) {    return Position({horizontal: N,vertical: P,x: x,y: y});});
   var topRightAt = F2(function (x,y) {    return Position({horizontal: P,vertical: P,x: x,y: y});});
   var bottomRightAt = F2(function (x,y) {    return Position({horizontal: P,vertical: N,x: x,y: y});});
   var midRightAt = F2(function (x,y) {    return Position({horizontal: P,vertical: Z,x: x,y: y});});
   var midTopAt = F2(function (x,y) {    return Position({horizontal: Z,vertical: P,x: x,y: y});});
   var justified = $Native$Graphics$Element.block("justify");
   var centered = $Native$Graphics$Element.block("center");
   var rightAligned = $Native$Graphics$Element.block("right");
   var leftAligned = $Native$Graphics$Element.block("left");
   var show = function (value) {    return leftAligned($Text.monospace($Text.fromString($Basics.toString(value))));};
   var Tiled = {ctor: "Tiled"};
   var Cropped = function (a) {    return {ctor: "Cropped",_0: a};};
   var Fitted = {ctor: "Fitted"};
   var Plain = {ctor: "Plain"};
   var Custom = {ctor: "Custom"};
   var RawHtml = {ctor: "RawHtml"};
   var Spacer = {ctor: "Spacer"};
   var Flow = F2(function (a,b) {    return {ctor: "Flow",_0: a,_1: b};});
   var Container = F2(function (a,b) {    return {ctor: "Container",_0: a,_1: b};});
   var Image = F4(function (a,b,c,d) {    return {ctor: "Image",_0: a,_1: b,_2: c,_3: d};});
   var newElement = $Native$Graphics$Element.newElement;
   var image = F3(function (w,h,src) {    return A3(newElement,w,h,A4(Image,Plain,w,h,src));});
   var fittedImage = F3(function (w,h,src) {    return A3(newElement,w,h,A4(Image,Fitted,w,h,src));});
   var croppedImage = F4(function (pos,w,h,src) {    return A3(newElement,w,h,A4(Image,Cropped(pos),w,h,src));});
   var tiledImage = F3(function (w,h,src) {    return A3(newElement,w,h,A4(Image,Tiled,w,h,src));});
   var container = F4(function (w,h,_p0,e) {    var _p1 = _p0;return A3(newElement,w,h,A2(Container,_p1._0,e));});
   var spacer = F2(function (w,h) {    return A3(newElement,w,h,Spacer);});
   var sizeOf = function (_p2) {    var _p3 = _p2;var _p4 = _p3._0;return {ctor: "_Tuple2",_0: _p4.props.width,_1: _p4.props.height};};
   var heightOf = function (_p5) {    var _p6 = _p5;return _p6._0.props.height;};
   var widthOf = function (_p7) {    var _p8 = _p7;return _p8._0.props.width;};
   var above = F2(function (hi,lo) {
      return A3(newElement,A2($Basics.max,widthOf(hi),widthOf(lo)),heightOf(hi) + heightOf(lo),A2(Flow,DDown,_U.list([hi,lo])));
   });
   var below = F2(function (lo,hi) {
      return A3(newElement,A2($Basics.max,widthOf(hi),widthOf(lo)),heightOf(hi) + heightOf(lo),A2(Flow,DDown,_U.list([hi,lo])));
   });
   var beside = F2(function (lft,rht) {
      return A3(newElement,widthOf(lft) + widthOf(rht),A2($Basics.max,heightOf(lft),heightOf(rht)),A2(Flow,right,_U.list([lft,rht])));
   });
   var layers = function (es) {
      var hs = A2($List.map,heightOf,es);
      var ws = A2($List.map,widthOf,es);
      return A3(newElement,A2($Maybe.withDefault,0,$List.maximum(ws)),A2($Maybe.withDefault,0,$List.maximum(hs)),A2(Flow,DOut,es));
   };
   var empty = A2(spacer,0,0);
   var flow = F2(function (dir,es) {
      var newFlow = F2(function (w,h) {    return A3(newElement,w,h,A2(Flow,dir,es));});
      var maxOrZero = function (list) {    return A2($Maybe.withDefault,0,$List.maximum(list));};
      var hs = A2($List.map,heightOf,es);
      var ws = A2($List.map,widthOf,es);
      if (_U.eq(es,_U.list([]))) return empty; else {
            var _p9 = dir;
            switch (_p9.ctor)
            {case "DUp": return A2(newFlow,maxOrZero(ws),$List.sum(hs));
               case "DDown": return A2(newFlow,maxOrZero(ws),$List.sum(hs));
               case "DLeft": return A2(newFlow,$List.sum(ws),maxOrZero(hs));
               case "DRight": return A2(newFlow,$List.sum(ws),maxOrZero(hs));
               case "DIn": return A2(newFlow,maxOrZero(ws),maxOrZero(hs));
               default: return A2(newFlow,maxOrZero(ws),maxOrZero(hs));}
         }
   });
   var Properties = F9(function (a,b,c,d,e,f,g,h,i) {    return {id: a,width: b,height: c,opacity: d,color: e,href: f,tag: g,hover: h,click: i};});
   var Element_elm_builtin = function (a) {    return {ctor: "Element_elm_builtin",_0: a};};
   var width = F2(function (newWidth,_p10) {
      var _p11 = _p10;
      var _p14 = _p11._0.props;
      var _p13 = _p11._0.element;
      var newHeight = function () {
         var _p12 = _p13;
         switch (_p12.ctor)
         {case "Image": return $Basics.round($Basics.toFloat(_p12._2) / $Basics.toFloat(_p12._1) * $Basics.toFloat(newWidth));
            case "RawHtml": return $Basics.snd(A2($Native$Graphics$Element.htmlHeight,newWidth,_p13));
            default: return _p14.height;}
      }();
      return Element_elm_builtin({element: _p13,props: _U.update(_p14,{width: newWidth,height: newHeight})});
   });
   var height = F2(function (newHeight,_p15) {
      var _p16 = _p15;
      return Element_elm_builtin({element: _p16._0.element,props: _U.update(_p16._0.props,{height: newHeight})});
   });
   var size = F3(function (w,h,e) {    return A2(height,h,A2(width,w,e));});
   var opacity = F2(function (givenOpacity,_p17) {
      var _p18 = _p17;
      return Element_elm_builtin({element: _p18._0.element,props: _U.update(_p18._0.props,{opacity: givenOpacity})});
   });
   var color = F2(function (clr,_p19) {
      var _p20 = _p19;
      return Element_elm_builtin({element: _p20._0.element,props: _U.update(_p20._0.props,{color: $Maybe.Just(clr)})});
   });
   var tag = F2(function (name,_p21) {    var _p22 = _p21;return Element_elm_builtin({element: _p22._0.element,props: _U.update(_p22._0.props,{tag: name})});});
   var link = F2(function (href,_p23) {
      var _p24 = _p23;
      return Element_elm_builtin({element: _p24._0.element,props: _U.update(_p24._0.props,{href: href})});
   });
   return _elm.Graphics.Element.values = {_op: _op
                                         ,image: image
                                         ,fittedImage: fittedImage
                                         ,croppedImage: croppedImage
                                         ,tiledImage: tiledImage
                                         ,leftAligned: leftAligned
                                         ,rightAligned: rightAligned
                                         ,centered: centered
                                         ,justified: justified
                                         ,show: show
                                         ,width: width
                                         ,height: height
                                         ,size: size
                                         ,color: color
                                         ,opacity: opacity
                                         ,link: link
                                         ,tag: tag
                                         ,widthOf: widthOf
                                         ,heightOf: heightOf
                                         ,sizeOf: sizeOf
                                         ,flow: flow
                                         ,up: up
                                         ,down: down
                                         ,left: left
                                         ,right: right
                                         ,inward: inward
                                         ,outward: outward
                                         ,layers: layers
                                         ,above: above
                                         ,below: below
                                         ,beside: beside
                                         ,empty: empty
                                         ,spacer: spacer
                                         ,container: container
                                         ,middle: middle
                                         ,midTop: midTop
                                         ,midBottom: midBottom
                                         ,midLeft: midLeft
                                         ,midRight: midRight
                                         ,topLeft: topLeft
                                         ,topRight: topRight
                                         ,bottomLeft: bottomLeft
                                         ,bottomRight: bottomRight
                                         ,absolute: absolute
                                         ,relative: relative
                                         ,middleAt: middleAt
                                         ,midTopAt: midTopAt
                                         ,midBottomAt: midBottomAt
                                         ,midLeftAt: midLeftAt
                                         ,midRightAt: midRightAt
                                         ,topLeftAt: topLeftAt
                                         ,topRightAt: topRightAt
                                         ,bottomLeftAt: bottomLeftAt
                                         ,bottomRightAt: bottomRightAt};
};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Collage = Elm.Graphics.Collage || {};
Elm.Graphics.Collage.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Collage = _elm.Graphics.Collage || {};
   if (_elm.Graphics.Collage.values) return _elm.Graphics.Collage.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $List = Elm.List.make(_elm),
   $Native$Graphics$Collage = Elm.Native.Graphics.Collage.make(_elm),
   $Text = Elm.Text.make(_elm),
   $Transform2D = Elm.Transform2D.make(_elm);
   var _op = {};
   var Shape = function (a) {    return {ctor: "Shape",_0: a};};
   var polygon = function (points) {    return Shape(points);};
   var rect = F2(function (w,h) {
      var hh = h / 2;
      var hw = w / 2;
      return Shape(_U.list([{ctor: "_Tuple2",_0: 0 - hw,_1: 0 - hh}
                           ,{ctor: "_Tuple2",_0: 0 - hw,_1: hh}
                           ,{ctor: "_Tuple2",_0: hw,_1: hh}
                           ,{ctor: "_Tuple2",_0: hw,_1: 0 - hh}]));
   });
   var square = function (n) {    return A2(rect,n,n);};
   var oval = F2(function (w,h) {
      var hh = h / 2;
      var hw = w / 2;
      var n = 50;
      var t = 2 * $Basics.pi / n;
      var f = function (i) {    return {ctor: "_Tuple2",_0: hw * $Basics.cos(t * i),_1: hh * $Basics.sin(t * i)};};
      return Shape(A2($List.map,f,_U.range(0,n - 1)));
   });
   var circle = function (r) {    return A2(oval,2 * r,2 * r);};
   var ngon = F2(function (n,r) {
      var m = $Basics.toFloat(n);
      var t = 2 * $Basics.pi / m;
      var f = function (i) {    return {ctor: "_Tuple2",_0: r * $Basics.cos(t * i),_1: r * $Basics.sin(t * i)};};
      return Shape(A2($List.map,f,_U.range(0,m - 1)));
   });
   var Path = function (a) {    return {ctor: "Path",_0: a};};
   var path = function (ps) {    return Path(ps);};
   var segment = F2(function (p1,p2) {    return Path(_U.list([p1,p2]));});
   var collage = $Native$Graphics$Collage.collage;
   var Fill = function (a) {    return {ctor: "Fill",_0: a};};
   var Line = function (a) {    return {ctor: "Line",_0: a};};
   var FGroup = F2(function (a,b) {    return {ctor: "FGroup",_0: a,_1: b};});
   var FElement = function (a) {    return {ctor: "FElement",_0: a};};
   var FImage = F4(function (a,b,c,d) {    return {ctor: "FImage",_0: a,_1: b,_2: c,_3: d};});
   var FText = function (a) {    return {ctor: "FText",_0: a};};
   var FOutlinedText = F2(function (a,b) {    return {ctor: "FOutlinedText",_0: a,_1: b};});
   var FShape = F2(function (a,b) {    return {ctor: "FShape",_0: a,_1: b};});
   var FPath = F2(function (a,b) {    return {ctor: "FPath",_0: a,_1: b};});
   var LineStyle = F6(function (a,b,c,d,e,f) {    return {color: a,width: b,cap: c,join: d,dashing: e,dashOffset: f};});
   var Clipped = {ctor: "Clipped"};
   var Sharp = function (a) {    return {ctor: "Sharp",_0: a};};
   var Smooth = {ctor: "Smooth"};
   var Padded = {ctor: "Padded"};
   var Round = {ctor: "Round"};
   var Flat = {ctor: "Flat"};
   var defaultLine = {color: $Color.black,width: 1,cap: Flat,join: Sharp(10),dashing: _U.list([]),dashOffset: 0};
   var solid = function (clr) {    return _U.update(defaultLine,{color: clr});};
   var dashed = function (clr) {    return _U.update(defaultLine,{color: clr,dashing: _U.list([8,4])});};
   var dotted = function (clr) {    return _U.update(defaultLine,{color: clr,dashing: _U.list([3,3])});};
   var Grad = function (a) {    return {ctor: "Grad",_0: a};};
   var Texture = function (a) {    return {ctor: "Texture",_0: a};};
   var Solid = function (a) {    return {ctor: "Solid",_0: a};};
   var Form_elm_builtin = function (a) {    return {ctor: "Form_elm_builtin",_0: a};};
   var form = function (f) {    return Form_elm_builtin({theta: 0,scale: 1,x: 0,y: 0,alpha: 1,form: f});};
   var fill = F2(function (style,_p0) {    var _p1 = _p0;return form(A2(FShape,Fill(style),_p1._0));});
   var filled = F2(function (color,shape) {    return A2(fill,Solid(color),shape);});
   var textured = F2(function (src,shape) {    return A2(fill,Texture(src),shape);});
   var gradient = F2(function (grad,shape) {    return A2(fill,Grad(grad),shape);});
   var outlined = F2(function (style,_p2) {    var _p3 = _p2;return form(A2(FShape,Line(style),_p3._0));});
   var traced = F2(function (style,_p4) {    var _p5 = _p4;return form(A2(FPath,style,_p5._0));});
   var sprite = F4(function (w,h,pos,src) {    return form(A4(FImage,w,h,pos,src));});
   var toForm = function (e) {    return form(FElement(e));};
   var group = function (fs) {    return form(A2(FGroup,$Transform2D.identity,fs));};
   var groupTransform = F2(function (matrix,fs) {    return form(A2(FGroup,matrix,fs));});
   var text = function (t) {    return form(FText(t));};
   var outlinedText = F2(function (ls,t) {    return form(A2(FOutlinedText,ls,t));});
   var move = F2(function (_p7,_p6) {
      var _p8 = _p7;
      var _p9 = _p6;
      var _p10 = _p9._0;
      return Form_elm_builtin(_U.update(_p10,{x: _p10.x + _p8._0,y: _p10.y + _p8._1}));
   });
   var moveX = F2(function (x,_p11) {    var _p12 = _p11;var _p13 = _p12._0;return Form_elm_builtin(_U.update(_p13,{x: _p13.x + x}));});
   var moveY = F2(function (y,_p14) {    var _p15 = _p14;var _p16 = _p15._0;return Form_elm_builtin(_U.update(_p16,{y: _p16.y + y}));});
   var scale = F2(function (s,_p17) {    var _p18 = _p17;var _p19 = _p18._0;return Form_elm_builtin(_U.update(_p19,{scale: _p19.scale * s}));});
   var rotate = F2(function (t,_p20) {    var _p21 = _p20;var _p22 = _p21._0;return Form_elm_builtin(_U.update(_p22,{theta: _p22.theta + t}));});
   var alpha = F2(function (a,_p23) {    var _p24 = _p23;return Form_elm_builtin(_U.update(_p24._0,{alpha: a}));});
   return _elm.Graphics.Collage.values = {_op: _op
                                         ,collage: collage
                                         ,toForm: toForm
                                         ,filled: filled
                                         ,textured: textured
                                         ,gradient: gradient
                                         ,outlined: outlined
                                         ,traced: traced
                                         ,text: text
                                         ,outlinedText: outlinedText
                                         ,move: move
                                         ,moveX: moveX
                                         ,moveY: moveY
                                         ,scale: scale
                                         ,rotate: rotate
                                         ,alpha: alpha
                                         ,group: group
                                         ,groupTransform: groupTransform
                                         ,rect: rect
                                         ,oval: oval
                                         ,square: square
                                         ,circle: circle
                                         ,ngon: ngon
                                         ,polygon: polygon
                                         ,segment: segment
                                         ,path: path
                                         ,solid: solid
                                         ,dashed: dashed
                                         ,dotted: dotted
                                         ,defaultLine: defaultLine
                                         ,LineStyle: LineStyle
                                         ,Flat: Flat
                                         ,Round: Round
                                         ,Padded: Padded
                                         ,Smooth: Smooth
                                         ,Sharp: Sharp
                                         ,Clipped: Clipped};
};
Elm.Native.Debug = {};
Elm.Native.Debug.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Debug = localRuntime.Native.Debug || {};
	if (localRuntime.Native.Debug.values)
	{
		return localRuntime.Native.Debug.values;
	}

	var toString = Elm.Native.Utils.make(localRuntime).toString;

	function log(tag, value)
	{
		var msg = tag + ': ' + toString(value);
		var process = process || {};
		if (process.stdout)
		{
			process.stdout.write(msg);
		}
		else
		{
			console.log(msg);
		}
		return value;
	}

	function crash(message)
	{
		throw new Error(message);
	}

	function tracePath(tag, form)
	{
		if (localRuntime.debug)
		{
			return localRuntime.debug.trace(tag, form);
		}
		return form;
	}

	function watch(tag, value)
	{
		if (localRuntime.debug)
		{
			localRuntime.debug.watch(tag, value);
		}
		return value;
	}

	function watchSummary(tag, summarize, value)
	{
		if (localRuntime.debug)
		{
			localRuntime.debug.watch(tag, summarize(value));
		}
		return value;
	}

	return localRuntime.Native.Debug.values = {
		crash: crash,
		tracePath: F2(tracePath),
		log: F2(log),
		watch: F2(watch),
		watchSummary: F3(watchSummary)
	};
};

Elm.Debug = Elm.Debug || {};
Elm.Debug.make = function (_elm) {
   "use strict";
   _elm.Debug = _elm.Debug || {};
   if (_elm.Debug.values) return _elm.Debug.values;
   var _U = Elm.Native.Utils.make(_elm),$Graphics$Collage = Elm.Graphics.Collage.make(_elm),$Native$Debug = Elm.Native.Debug.make(_elm);
   var _op = {};
   var trace = $Native$Debug.tracePath;
   var watchSummary = $Native$Debug.watchSummary;
   var watch = $Native$Debug.watch;
   var crash = $Native$Debug.crash;
   var log = $Native$Debug.log;
   return _elm.Debug.values = {_op: _op,log: log,crash: crash,watch: watch,watchSummary: watchSummary,trace: trace};
};
Elm.Result = Elm.Result || {};
Elm.Result.make = function (_elm) {
   "use strict";
   _elm.Result = _elm.Result || {};
   if (_elm.Result.values) return _elm.Result.values;
   var _U = Elm.Native.Utils.make(_elm),$Maybe = Elm.Maybe.make(_elm);
   var _op = {};
   var toMaybe = function (result) {    var _p0 = result;if (_p0.ctor === "Ok") {    return $Maybe.Just(_p0._0);} else {    return $Maybe.Nothing;}};
   var withDefault = F2(function (def,result) {    var _p1 = result;if (_p1.ctor === "Ok") {    return _p1._0;} else {    return def;}});
   var Err = function (a) {    return {ctor: "Err",_0: a};};
   var andThen = F2(function (result,callback) {    var _p2 = result;if (_p2.ctor === "Ok") {    return callback(_p2._0);} else {    return Err(_p2._0);}});
   var Ok = function (a) {    return {ctor: "Ok",_0: a};};
   var map = F2(function (func,ra) {    var _p3 = ra;if (_p3.ctor === "Ok") {    return Ok(func(_p3._0));} else {    return Err(_p3._0);}});
   var map2 = F3(function (func,ra,rb) {
      var _p4 = {ctor: "_Tuple2",_0: ra,_1: rb};
      if (_p4._0.ctor === "Ok") {
            if (_p4._1.ctor === "Ok") {
                  return Ok(A2(func,_p4._0._0,_p4._1._0));
               } else {
                  return Err(_p4._1._0);
               }
         } else {
            return Err(_p4._0._0);
         }
   });
   var map3 = F4(function (func,ra,rb,rc) {
      var _p5 = {ctor: "_Tuple3",_0: ra,_1: rb,_2: rc};
      if (_p5._0.ctor === "Ok") {
            if (_p5._1.ctor === "Ok") {
                  if (_p5._2.ctor === "Ok") {
                        return Ok(A3(func,_p5._0._0,_p5._1._0,_p5._2._0));
                     } else {
                        return Err(_p5._2._0);
                     }
               } else {
                  return Err(_p5._1._0);
               }
         } else {
            return Err(_p5._0._0);
         }
   });
   var map4 = F5(function (func,ra,rb,rc,rd) {
      var _p6 = {ctor: "_Tuple4",_0: ra,_1: rb,_2: rc,_3: rd};
      if (_p6._0.ctor === "Ok") {
            if (_p6._1.ctor === "Ok") {
                  if (_p6._2.ctor === "Ok") {
                        if (_p6._3.ctor === "Ok") {
                              return Ok(A4(func,_p6._0._0,_p6._1._0,_p6._2._0,_p6._3._0));
                           } else {
                              return Err(_p6._3._0);
                           }
                     } else {
                        return Err(_p6._2._0);
                     }
               } else {
                  return Err(_p6._1._0);
               }
         } else {
            return Err(_p6._0._0);
         }
   });
   var map5 = F6(function (func,ra,rb,rc,rd,re) {
      var _p7 = {ctor: "_Tuple5",_0: ra,_1: rb,_2: rc,_3: rd,_4: re};
      if (_p7._0.ctor === "Ok") {
            if (_p7._1.ctor === "Ok") {
                  if (_p7._2.ctor === "Ok") {
                        if (_p7._3.ctor === "Ok") {
                              if (_p7._4.ctor === "Ok") {
                                    return Ok(A5(func,_p7._0._0,_p7._1._0,_p7._2._0,_p7._3._0,_p7._4._0));
                                 } else {
                                    return Err(_p7._4._0);
                                 }
                           } else {
                              return Err(_p7._3._0);
                           }
                     } else {
                        return Err(_p7._2._0);
                     }
               } else {
                  return Err(_p7._1._0);
               }
         } else {
            return Err(_p7._0._0);
         }
   });
   var formatError = F2(function (f,result) {    var _p8 = result;if (_p8.ctor === "Ok") {    return Ok(_p8._0);} else {    return Err(f(_p8._0));}});
   var fromMaybe = F2(function (err,maybe) {    var _p9 = maybe;if (_p9.ctor === "Just") {    return Ok(_p9._0);} else {    return Err(err);}});
   return _elm.Result.values = {_op: _op
                               ,withDefault: withDefault
                               ,map: map
                               ,map2: map2
                               ,map3: map3
                               ,map4: map4
                               ,map5: map5
                               ,andThen: andThen
                               ,toMaybe: toMaybe
                               ,fromMaybe: fromMaybe
                               ,formatError: formatError
                               ,Ok: Ok
                               ,Err: Err};
};
Elm.Native.Signal = {};

Elm.Native.Signal.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Signal = localRuntime.Native.Signal || {};
	if (localRuntime.Native.Signal.values)
	{
		return localRuntime.Native.Signal.values;
	}


	var Task = Elm.Native.Task.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);


	function broadcastToKids(node, timestamp, update)
	{
		var kids = node.kids;
		for (var i = kids.length; i--; )
		{
			kids[i].notify(timestamp, update, node.id);
		}
	}


	// INPUT

	function input(name, base)
	{
		var node = {
			id: Utils.guid(),
			name: 'input-' + name,
			value: base,
			parents: [],
			kids: []
		};

		node.notify = function(timestamp, targetId, value) {
			var update = targetId === node.id;
			if (update)
			{
				node.value = value;
			}
			broadcastToKids(node, timestamp, update);
			return update;
		};

		localRuntime.inputs.push(node);

		return node;
	}

	function constant(value)
	{
		return input('constant', value);
	}


	// MAILBOX

	function mailbox(base)
	{
		var signal = input('mailbox', base);

		function send(value) {
			return Task.asyncFunction(function(callback) {
				localRuntime.setTimeout(function() {
					localRuntime.notify(signal.id, value);
				}, 0);
				callback(Task.succeed(Utils.Tuple0));
			});
		}

		return {
			signal: signal,
			address: {
				ctor: 'Address',
				_0: send
			}
		};
	}

	function sendMessage(message)
	{
		Task.perform(message._0);
	}


	// OUTPUT

	function output(name, handler, parent)
	{
		var node = {
			id: Utils.guid(),
			name: 'output-' + name,
			parents: [parent],
			isOutput: true
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentUpdate)
			{
				handler(parent.value);
			}
		};

		parent.kids.push(node);

		return node;
	}


	// MAP

	function mapMany(refreshValue, args)
	{
		var node = {
			id: Utils.guid(),
			name: 'map' + args.length,
			value: refreshValue(),
			parents: args,
			kids: []
		};

		var numberOfParents = args.length;
		var count = 0;
		var update = false;

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			++count;

			update = update || parentUpdate;

			if (count === numberOfParents)
			{
				if (update)
				{
					node.value = refreshValue();
				}
				broadcastToKids(node, timestamp, update);
				update = false;
				count = 0;
			}
		};

		for (var i = numberOfParents; i--; )
		{
			args[i].kids.push(node);
		}

		return node;
	}


	function map(func, a)
	{
		function refreshValue()
		{
			return func(a.value);
		}
		return mapMany(refreshValue, [a]);
	}


	function map2(func, a, b)
	{
		function refreshValue()
		{
			return A2( func, a.value, b.value );
		}
		return mapMany(refreshValue, [a, b]);
	}


	function map3(func, a, b, c)
	{
		function refreshValue()
		{
			return A3( func, a.value, b.value, c.value );
		}
		return mapMany(refreshValue, [a, b, c]);
	}


	function map4(func, a, b, c, d)
	{
		function refreshValue()
		{
			return A4( func, a.value, b.value, c.value, d.value );
		}
		return mapMany(refreshValue, [a, b, c, d]);
	}


	function map5(func, a, b, c, d, e)
	{
		function refreshValue()
		{
			return A5( func, a.value, b.value, c.value, d.value, e.value );
		}
		return mapMany(refreshValue, [a, b, c, d, e]);
	}


	// FOLD

	function foldp(update, state, signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'foldp',
			parents: [signal],
			kids: [],
			value: state
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentUpdate)
			{
				node.value = A2( update, signal.value, node.value );
			}
			broadcastToKids(node, timestamp, parentUpdate);
		};

		signal.kids.push(node);

		return node;
	}


	// TIME

	function timestamp(signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'timestamp',
			value: Utils.Tuple2(localRuntime.timer.programStart, signal.value),
			parents: [signal],
			kids: []
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentUpdate)
			{
				node.value = Utils.Tuple2(timestamp, signal.value);
			}
			broadcastToKids(node, timestamp, parentUpdate);
		};

		signal.kids.push(node);

		return node;
	}


	function delay(time, signal)
	{
		var delayed = input('delay-input-' + time, signal.value);

		function handler(value)
		{
			setTimeout(function() {
				localRuntime.notify(delayed.id, value);
			}, time);
		}

		output('delay-output-' + time, handler, signal);

		return delayed;
	}


	// MERGING

	function genericMerge(tieBreaker, leftStream, rightStream)
	{
		var node = {
			id: Utils.guid(),
			name: 'merge',
			value: A2(tieBreaker, leftStream.value, rightStream.value),
			parents: [leftStream, rightStream],
			kids: []
		};

		var left = { touched: false, update: false, value: null };
		var right = { touched: false, update: false, value: null };

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentID === leftStream.id)
			{
				left.touched = true;
				left.update = parentUpdate;
				left.value = leftStream.value;
			}
			if (parentID === rightStream.id)
			{
				right.touched = true;
				right.update = parentUpdate;
				right.value = rightStream.value;
			}

			if (left.touched && right.touched)
			{
				var update = false;
				if (left.update && right.update)
				{
					node.value = A2(tieBreaker, left.value, right.value);
					update = true;
				}
				else if (left.update)
				{
					node.value = left.value;
					update = true;
				}
				else if (right.update)
				{
					node.value = right.value;
					update = true;
				}
				left.touched = false;
				right.touched = false;

				broadcastToKids(node, timestamp, update);
			}
		};

		leftStream.kids.push(node);
		rightStream.kids.push(node);

		return node;
	}


	// FILTERING

	function filterMap(toMaybe, base, signal)
	{
		var maybe = toMaybe(signal.value);
		var node = {
			id: Utils.guid(),
			name: 'filterMap',
			value: maybe.ctor === 'Nothing' ? base : maybe._0,
			parents: [signal],
			kids: []
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			var update = false;
			if (parentUpdate)
			{
				var maybe = toMaybe(signal.value);
				if (maybe.ctor === 'Just')
				{
					update = true;
					node.value = maybe._0;
				}
			}
			broadcastToKids(node, timestamp, update);
		};

		signal.kids.push(node);

		return node;
	}


	// SAMPLING

	function sampleOn(ticker, signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'sampleOn',
			value: signal.value,
			parents: [ticker, signal],
			kids: []
		};

		var signalTouch = false;
		var tickerTouch = false;
		var tickerUpdate = false;

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentID === ticker.id)
			{
				tickerTouch = true;
				tickerUpdate = parentUpdate;
			}
			if (parentID === signal.id)
			{
				signalTouch = true;
			}

			if (tickerTouch && signalTouch)
			{
				if (tickerUpdate)
				{
					node.value = signal.value;
				}
				tickerTouch = false;
				signalTouch = false;

				broadcastToKids(node, timestamp, tickerUpdate);
			}
		};

		ticker.kids.push(node);
		signal.kids.push(node);

		return node;
	}


	// DROP REPEATS

	function dropRepeats(signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'dropRepeats',
			value: signal.value,
			parents: [signal],
			kids: []
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			var update = false;
			if (parentUpdate && !Utils.eq(node.value, signal.value))
			{
				node.value = signal.value;
				update = true;
			}
			broadcastToKids(node, timestamp, update);
		};

		signal.kids.push(node);

		return node;
	}


	return localRuntime.Native.Signal.values = {
		input: input,
		constant: constant,
		mailbox: mailbox,
		sendMessage: sendMessage,
		output: output,
		map: F2(map),
		map2: F3(map2),
		map3: F4(map3),
		map4: F5(map4),
		map5: F6(map5),
		foldp: F3(foldp),
		genericMerge: F3(genericMerge),
		filterMap: F3(filterMap),
		sampleOn: F2(sampleOn),
		dropRepeats: dropRepeats,
		timestamp: timestamp,
		delay: F2(delay)
	};
};

Elm.Native.Task = {};

Elm.Native.Task.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Task = localRuntime.Native.Task || {};
	if (localRuntime.Native.Task.values)
	{
		return localRuntime.Native.Task.values;
	}

	var Result = Elm.Result.make(localRuntime);
	var Signal;
	var Utils = Elm.Native.Utils.make(localRuntime);


	// CONSTRUCTORS

	function succeed(value)
	{
		return {
			tag: 'Succeed',
			value: value
		};
	}

	function fail(error)
	{
		return {
			tag: 'Fail',
			value: error
		};
	}

	function asyncFunction(func)
	{
		return {
			tag: 'Async',
			asyncFunction: func
		};
	}

	function andThen(task, callback)
	{
		return {
			tag: 'AndThen',
			task: task,
			callback: callback
		};
	}

	function catch_(task, callback)
	{
		return {
			tag: 'Catch',
			task: task,
			callback: callback
		};
	}


	// RUNNER

	function perform(task) {
		runTask({ task: task }, function() {});
	}

	function performSignal(name, signal)
	{
		var workQueue = [];

		function onComplete()
		{
			workQueue.shift();

			if (workQueue.length > 0)
			{
				var task = workQueue[0];

				setTimeout(function() {
					runTask(task, onComplete);
				}, 0);
			}
		}

		function register(task)
		{
			var root = { task: task };
			workQueue.push(root);
			if (workQueue.length === 1)
			{
				runTask(root, onComplete);
			}
		}

		if (!Signal)
		{
			Signal = Elm.Native.Signal.make(localRuntime);
		}
		Signal.output('perform-tasks-' + name, register, signal);

		register(signal.value);

		return signal;
	}

	function mark(status, task)
	{
		return { status: status, task: task };
	}

	function runTask(root, onComplete)
	{
		var result = mark('runnable', root.task);
		while (result.status === 'runnable')
		{
			result = stepTask(onComplete, root, result.task);
		}

		if (result.status === 'done')
		{
			root.task = result.task;
			onComplete();
		}

		if (result.status === 'blocked')
		{
			root.task = result.task;
		}
	}

	function stepTask(onComplete, root, task)
	{
		var tag = task.tag;

		if (tag === 'Succeed' || tag === 'Fail')
		{
			return mark('done', task);
		}

		if (tag === 'Async')
		{
			var placeHolder = {};
			var couldBeSync = true;
			var wasSync = false;

			task.asyncFunction(function(result) {
				placeHolder.tag = result.tag;
				placeHolder.value = result.value;
				if (couldBeSync)
				{
					wasSync = true;
				}
				else
				{
					runTask(root, onComplete);
				}
			});
			couldBeSync = false;
			return mark(wasSync ? 'done' : 'blocked', placeHolder);
		}

		if (tag === 'AndThen' || tag === 'Catch')
		{
			var result = mark('runnable', task.task);
			while (result.status === 'runnable')
			{
				result = stepTask(onComplete, root, result.task);
			}

			if (result.status === 'done')
			{
				var activeTask = result.task;
				var activeTag = activeTask.tag;

				var succeedChain = activeTag === 'Succeed' && tag === 'AndThen';
				var failChain = activeTag === 'Fail' && tag === 'Catch';

				return (succeedChain || failChain)
					? mark('runnable', task.callback(activeTask.value))
					: mark('runnable', activeTask);
			}
			if (result.status === 'blocked')
			{
				return mark('blocked', {
					tag: tag,
					task: result.task,
					callback: task.callback
				});
			}
		}
	}


	// THREADS

	function sleep(time) {
		return asyncFunction(function(callback) {
			setTimeout(function() {
				callback(succeed(Utils.Tuple0));
			}, time);
		});
	}

	function spawn(task) {
		return asyncFunction(function(callback) {
			var id = setTimeout(function() {
				perform(task);
			}, 0);
			callback(succeed(id));
		});
	}


	return localRuntime.Native.Task.values = {
		succeed: succeed,
		fail: fail,
		asyncFunction: asyncFunction,
		andThen: F2(andThen),
		catch_: F2(catch_),
		perform: perform,
		performSignal: performSignal,
		spawn: spawn,
		sleep: sleep
	};
};

Elm.Task = Elm.Task || {};
Elm.Task.make = function (_elm) {
   "use strict";
   _elm.Task = _elm.Task || {};
   if (_elm.Task.values) return _elm.Task.values;
   var _U = Elm.Native.Utils.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Task = Elm.Native.Task.make(_elm),
   $Result = Elm.Result.make(_elm);
   var _op = {};
   var sleep = $Native$Task.sleep;
   var spawn = $Native$Task.spawn;
   var ThreadID = function (a) {    return {ctor: "ThreadID",_0: a};};
   var onError = $Native$Task.catch_;
   var andThen = $Native$Task.andThen;
   var fail = $Native$Task.fail;
   var mapError = F2(function (f,task) {    return A2(onError,task,function (err) {    return fail(f(err));});});
   var succeed = $Native$Task.succeed;
   var map = F2(function (func,taskA) {    return A2(andThen,taskA,function (a) {    return succeed(func(a));});});
   var map2 = F3(function (func,taskA,taskB) {
      return A2(andThen,taskA,function (a) {    return A2(andThen,taskB,function (b) {    return succeed(A2(func,a,b));});});
   });
   var map3 = F4(function (func,taskA,taskB,taskC) {
      return A2(andThen,
      taskA,
      function (a) {
         return A2(andThen,taskB,function (b) {    return A2(andThen,taskC,function (c) {    return succeed(A3(func,a,b,c));});});
      });
   });
   var map4 = F5(function (func,taskA,taskB,taskC,taskD) {
      return A2(andThen,
      taskA,
      function (a) {
         return A2(andThen,
         taskB,
         function (b) {
            return A2(andThen,taskC,function (c) {    return A2(andThen,taskD,function (d) {    return succeed(A4(func,a,b,c,d));});});
         });
      });
   });
   var map5 = F6(function (func,taskA,taskB,taskC,taskD,taskE) {
      return A2(andThen,
      taskA,
      function (a) {
         return A2(andThen,
         taskB,
         function (b) {
            return A2(andThen,
            taskC,
            function (c) {
               return A2(andThen,taskD,function (d) {    return A2(andThen,taskE,function (e) {    return succeed(A5(func,a,b,c,d,e));});});
            });
         });
      });
   });
   var andMap = F2(function (taskFunc,taskValue) {
      return A2(andThen,taskFunc,function (func) {    return A2(andThen,taskValue,function (value) {    return succeed(func(value));});});
   });
   var sequence = function (tasks) {
      var _p0 = tasks;
      if (_p0.ctor === "[]") {
            return succeed(_U.list([]));
         } else {
            return A3(map2,F2(function (x,y) {    return A2($List._op["::"],x,y);}),_p0._0,sequence(_p0._1));
         }
   };
   var toMaybe = function (task) {    return A2(onError,A2(map,$Maybe.Just,task),function (_p1) {    return succeed($Maybe.Nothing);});};
   var fromMaybe = F2(function ($default,maybe) {    var _p2 = maybe;if (_p2.ctor === "Just") {    return succeed(_p2._0);} else {    return fail($default);}});
   var toResult = function (task) {    return A2(onError,A2(map,$Result.Ok,task),function (msg) {    return succeed($Result.Err(msg));});};
   var fromResult = function (result) {    var _p3 = result;if (_p3.ctor === "Ok") {    return succeed(_p3._0);} else {    return fail(_p3._0);}};
   var Task = {ctor: "Task"};
   return _elm.Task.values = {_op: _op
                             ,succeed: succeed
                             ,fail: fail
                             ,map: map
                             ,map2: map2
                             ,map3: map3
                             ,map4: map4
                             ,map5: map5
                             ,andMap: andMap
                             ,sequence: sequence
                             ,andThen: andThen
                             ,onError: onError
                             ,mapError: mapError
                             ,toMaybe: toMaybe
                             ,fromMaybe: fromMaybe
                             ,toResult: toResult
                             ,fromResult: fromResult
                             ,spawn: spawn
                             ,sleep: sleep};
};
Elm.Signal = Elm.Signal || {};
Elm.Signal.make = function (_elm) {
   "use strict";
   _elm.Signal = _elm.Signal || {};
   if (_elm.Signal.values) return _elm.Signal.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Signal = Elm.Native.Signal.make(_elm),
   $Task = Elm.Task.make(_elm);
   var _op = {};
   var send = F2(function (_p0,value) {
      var _p1 = _p0;
      return A2($Task.onError,_p1._0(value),function (_p2) {    return $Task.succeed({ctor: "_Tuple0"});});
   });
   var Message = function (a) {    return {ctor: "Message",_0: a};};
   var message = F2(function (_p3,value) {    var _p4 = _p3;return Message(_p4._0(value));});
   var mailbox = $Native$Signal.mailbox;
   var Address = function (a) {    return {ctor: "Address",_0: a};};
   var forwardTo = F2(function (_p5,f) {    var _p6 = _p5;return Address(function (x) {    return _p6._0(f(x));});});
   var Mailbox = F2(function (a,b) {    return {address: a,signal: b};});
   var sampleOn = $Native$Signal.sampleOn;
   var dropRepeats = $Native$Signal.dropRepeats;
   var filterMap = $Native$Signal.filterMap;
   var filter = F3(function (isOk,base,signal) {
      return A3(filterMap,function (value) {    return isOk(value) ? $Maybe.Just(value) : $Maybe.Nothing;},base,signal);
   });
   var merge = F2(function (left,right) {    return A3($Native$Signal.genericMerge,$Basics.always,left,right);});
   var mergeMany = function (signalList) {
      var _p7 = $List.reverse(signalList);
      if (_p7.ctor === "[]") {
            return _U.crashCase("Signal",{start: {line: 184,column: 3},end: {line: 189,column: 40}},_p7)("mergeMany was given an empty list!");
         } else {
            return A3($List.foldl,merge,_p7._0,_p7._1);
         }
   };
   var foldp = $Native$Signal.foldp;
   var map5 = $Native$Signal.map5;
   var map4 = $Native$Signal.map4;
   var map3 = $Native$Signal.map3;
   var map2 = $Native$Signal.map2;
   var map = $Native$Signal.map;
   var constant = $Native$Signal.constant;
   var Signal = {ctor: "Signal"};
   return _elm.Signal.values = {_op: _op
                               ,merge: merge
                               ,mergeMany: mergeMany
                               ,map: map
                               ,map2: map2
                               ,map3: map3
                               ,map4: map4
                               ,map5: map5
                               ,constant: constant
                               ,dropRepeats: dropRepeats
                               ,filter: filter
                               ,filterMap: filterMap
                               ,sampleOn: sampleOn
                               ,foldp: foldp
                               ,mailbox: mailbox
                               ,send: send
                               ,message: message
                               ,forwardTo: forwardTo
                               ,Mailbox: Mailbox};
};
Elm.Signal = Elm.Signal || {};
Elm.Signal.Discrete = Elm.Signal.Discrete || {};
Elm.Signal.Discrete.make = function (_elm) {
   "use strict";
   _elm.Signal = _elm.Signal || {};
   _elm.Signal.Discrete = _elm.Signal.Discrete || {};
   if (_elm.Signal.Discrete.values) return _elm.Signal.Discrete.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var folde = F3(function (step,base,evt) {    return A3($Signal.foldp,F2(function (_p0,b) {    return step(b);}),base,evt);});
   var es = $Signal.map($Basics.always({ctor: "_Tuple0"}));
   var whenEqual = F2(function (value,input) {
      var keepIf = $Signal.filter;
      var matchEvent = A3(keepIf,F2(function (x,y) {    return _U.eq(x,y);})(value),value,input);
      return es(matchEvent);
   });
   var whenChangeTo = F2(function (value,input) {    return A2(whenEqual,value,$Signal.dropRepeats(input));});
   var whenChange = function (input) {    return es($Signal.dropRepeats(input));};
   return _elm.Signal.Discrete.values = {_op: _op,es: es,whenEqual: whenEqual,whenChange: whenChange,whenChangeTo: whenChangeTo,folde: folde};
};
Elm.Signal = Elm.Signal || {};
Elm.Signal.Extra = Elm.Signal.Extra || {};
Elm.Signal.Extra.make = function (_elm) {
   "use strict";
   _elm.Signal = _elm.Signal || {};
   _elm.Signal.Extra = _elm.Signal.Extra || {};
   if (_elm.Signal.Extra.values) return _elm.Signal.Extra.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var unsafeFromJust = function (maybe) {
      var _p0 = maybe;
      if (_p0.ctor === "Just") {
            return _p0._0;
         } else {
            return _U.crashCase("Signal.Extra",{start: {line: 510,column: 3},end: {line: 515,column: 59}},_p0)("This case should have been unreachable");
         }
   };
   var passiveMap2 = F2(function (func,a) {    return function (_p2) {    return A3($Signal.map2,func,a,A2($Signal.sampleOn,a,_p2));};});
   var withPassive = passiveMap2(F2(function (x,y) {    return x(y);}));
   var combine = A2($List.foldr,$Signal.map2(F2(function (x,y) {    return A2($List._op["::"],x,y);})),$Signal.constant(_U.list([])));
   var mergeMany = F2(function (original,others) {    return A3($List.foldl,$Signal.merge,original,others);});
   var filter = function (initial) {    return A2($Signal.filterMap,$Basics.identity,initial);};
   var keepIf = $Signal.filter;
   var runBuffer$ = F3(function (l,n,input) {
      var f = F2(function (inp,prev) {
         var l = $List.length(prev);
         return _U.cmp(l,n) < 0 ? A2($Basics._op["++"],prev,_U.list([inp])) : A2($Basics._op["++"],A2($List.drop,l - n + 1,prev),_U.list([inp]));
      });
      return A3($Signal.foldp,f,l,input);
   });
   var runBuffer = runBuffer$(_U.list([]));
   var initSignal = function (s) {    return A2($Signal.sampleOn,$Signal.constant({ctor: "_Tuple0"}),s);};
   var zip4 = $Signal.map4(F4(function (v0,v1,v2,v3) {    return {ctor: "_Tuple4",_0: v0,_1: v1,_2: v2,_3: v3};}));
   var zip3 = $Signal.map3(F3(function (v0,v1,v2) {    return {ctor: "_Tuple3",_0: v0,_1: v1,_2: v2};}));
   var zip = $Signal.map2(F2(function (v0,v1) {    return {ctor: "_Tuple2",_0: v0,_1: v1};}));
   var keepWhen = F3(function (boolSig,a,aSig) {
      return A2($Signal.map,$Basics.snd,A3(keepIf,$Basics.fst,{ctor: "_Tuple2",_0: true,_1: a},A2($Signal.sampleOn,aSig,A2(zip,boolSig,aSig))));
   });
   var sampleWhen = F3(function (bs,def,sig) {
      return A2($Signal.map,$Basics.snd,A3(keepIf,$Basics.fst,{ctor: "_Tuple2",_0: true,_1: def},A2(zip,bs,sig)));
   });
   var andMap = $Signal.map2(F2(function (x,y) {    return x(y);}));
   _op["~"] = andMap;
   var applyMany = F2(function (fs,l) {    return A2(_op["~"],fs,combine(l));});
   _op["~>"] = $Basics.flip($Signal.map);
   var foldpWith = F4(function (unpack,step,init,input) {
      var step$ = F2(function (a,_p3) {    var _p4 = _p3;return unpack(A2(step,a,_p4._1));});
      return A2(_op["~>"],A3($Signal.foldp,step$,init,input),$Basics.fst);
   });
   _op["<~"] = $Signal.map;
   var unzip = function (pairS) {    return {ctor: "_Tuple2",_0: A2(_op["<~"],$Basics.fst,pairS),_1: A2(_op["<~"],$Basics.snd,pairS)};};
   var unzip3 = function (pairS) {
      return {ctor: "_Tuple3"
             ,_0: A2(_op["<~"],function (_p5) {    var _p6 = _p5;return _p6._0;},pairS)
             ,_1: A2(_op["<~"],function (_p7) {    var _p8 = _p7;return _p8._1;},pairS)
             ,_2: A2(_op["<~"],function (_p9) {    var _p10 = _p9;return _p10._2;},pairS)};
   };
   var unzip4 = function (pairS) {
      return {ctor: "_Tuple4"
             ,_0: A2(_op["<~"],function (_p11) {    var _p12 = _p11;return _p12._0;},pairS)
             ,_1: A2(_op["<~"],function (_p13) {    var _p14 = _p13;return _p14._1;},pairS)
             ,_2: A2(_op["<~"],function (_p15) {    var _p16 = _p15;return _p16._2;},pairS)
             ,_3: A2(_op["<~"],function (_p17) {    var _p18 = _p17;return _p18._3;},pairS)};
   };
   var foldp$ = F3(function (fun,initFun,input) {
      var fun$ = F2(function (_p19,mb) {    var _p20 = _p19;return $Maybe.Just(A2(fun,_p20._0,A2($Maybe.withDefault,_p20._1,mb)));});
      var initial = A2(_op["~>"],initSignal(input),initFun);
      var rest = A3($Signal.foldp,fun$,$Maybe.Nothing,A2(zip,input,initial));
      return A2(_op["<~"],unsafeFromJust,A2($Signal.merge,A2(_op["<~"],$Maybe.Just,initial),rest));
   });
   var deltas = function (signal) {
      var initial = function (value) {    return {ctor: "_Tuple2",_0: value,_1: value};};
      var step = F2(function (value,delta) {    return {ctor: "_Tuple2",_0: $Basics.snd(delta),_1: value};});
      return A3(foldp$,step,initial,signal);
   };
   var foldps = F3(function (f,bs,aS) {
      return A2(_op["<~"],$Basics.fst,A3($Signal.foldp,F2(function (a,_p21) {    var _p22 = _p21;return A2(f,a,_p22._1);}),bs,aS));
   });
   var delayRound = F2(function (b,bS) {
      return A3(foldps,F2(function ($new,old) {    return {ctor: "_Tuple2",_0: old,_1: $new};}),{ctor: "_Tuple2",_0: b,_1: b},bS);
   });
   var filterFold = F2(function (f,initial) {
      var f$ = F2(function (a,s) {    var res = A2(f,a,s);return {ctor: "_Tuple2",_0: res,_1: A2($Maybe.withDefault,s,res)};});
      return function (_p23) {
         return A2(filter,initial,A3(foldps,f$,{ctor: "_Tuple2",_0: $Maybe.Just(initial),_1: initial},_p23));
      };
   });
   var foldps$ = F3(function (f,iF,aS) {
      return A2(_op["<~"],$Basics.fst,A3(foldp$,F2(function (a,_p24) {    var _p25 = _p24;return A2(f,a,_p25._1);}),iF,aS));
   });
   var switchHelper = F4(function (filter,b,l,r) {
      var lAndR = A2($Signal.merge,
      A3(filter,b,$Maybe.Nothing,A2(_op["<~"],$Maybe.Just,l)),
      A3(filter,A2(_op["<~"],$Basics.not,b),$Maybe.Nothing,A2(_op["<~"],$Maybe.Just,r)));
      var base = A2(_op["~"],
      A2(_op["~"],A2(_op["<~"],F3(function (bi,li,ri) {    return $Maybe.Just(bi ? li : ri);}),initSignal(b)),initSignal(l)),
      initSignal(r));
      return A2(_op["<~"],unsafeFromJust,A2($Signal.merge,base,lAndR));
   });
   var switchWhen = F3(function (b,l,r) {    return A4(switchHelper,keepWhen,b,l,r);});
   var switchSample = F3(function (b,l,r) {    return A4(switchHelper,sampleWhen,b,l,r);});
   var keepThen = F3(function (choice,base,signal) {    return A3(switchSample,choice,signal,$Signal.constant(base));});
   var keepWhenI = F2(function (fs,s) {
      return A2(_op["~>"],A3(keepWhen,A2($Signal.merge,$Signal.constant(true),fs),$Maybe.Nothing,A2(_op["<~"],$Maybe.Just,s)),unsafeFromJust);
   });
   var fairMerge = F3(function (resolve,left,right) {
      var merged = A2($Signal.merge,left,right);
      var boolRight = A2(_op["<~"],$Basics.always(false),right);
      var boolLeft = A2(_op["<~"],$Basics.always(true),left);
      var bothUpdated = A2(_op["~"],
      A2(_op["<~"],F2(function (x,y) {    return !_U.eq(x,y);}),A2($Signal.merge,boolLeft,boolRight)),
      A2($Signal.merge,boolRight,boolLeft));
      var keep = keepWhenI(bothUpdated);
      var resolved = A2(_op["~"],A2(_op["<~"],resolve,keep(left)),keep(right));
      return A2($Signal.merge,resolved,merged);
   });
   var mapMany = F2(function (f,l) {    return A2(_op["<~"],f,combine(l));});
   return _elm.Signal.Extra.values = {_op: _op
                                     ,andMap: andMap
                                     ,zip: zip
                                     ,zip3: zip3
                                     ,zip4: zip4
                                     ,unzip: unzip
                                     ,unzip3: unzip3
                                     ,unzip4: unzip4
                                     ,foldp$: foldp$
                                     ,foldps: foldps
                                     ,foldps$: foldps$
                                     ,runBuffer: runBuffer
                                     ,runBuffer$: runBuffer$
                                     ,deltas: deltas
                                     ,delayRound: delayRound
                                     ,keepIf: keepIf
                                     ,keepWhen: keepWhen
                                     ,sampleWhen: sampleWhen
                                     ,switchWhen: switchWhen
                                     ,keepWhenI: keepWhenI
                                     ,switchSample: switchSample
                                     ,keepThen: keepThen
                                     ,filter: filter
                                     ,filterFold: filterFold
                                     ,fairMerge: fairMerge
                                     ,mergeMany: mergeMany
                                     ,combine: combine
                                     ,mapMany: mapMany
                                     ,applyMany: applyMany
                                     ,passiveMap2: passiveMap2
                                     ,withPassive: withPassive};
};
Elm.Native.Time = {};

Elm.Native.Time.make = function(localRuntime)
{
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Time = localRuntime.Native.Time || {};
	if (localRuntime.Native.Time.values)
	{
		return localRuntime.Native.Time.values;
	}

	var NS = Elm.Native.Signal.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);


	// FRAMES PER SECOND

	function fpsWhen(desiredFPS, isOn)
	{
		var msPerFrame = 1000 / desiredFPS;
		var ticker = NS.input('fps-' + desiredFPS, null);

		function notifyTicker()
		{
			localRuntime.notify(ticker.id, null);
		}

		function firstArg(x, y)
		{
			return x;
		}

		// input fires either when isOn changes, or when ticker fires.
		// Its value is a tuple with the current timestamp, and the state of isOn
		var input = NS.timestamp(A3(NS.map2, F2(firstArg), NS.dropRepeats(isOn), ticker));

		var initialState = {
			isOn: false,
			time: localRuntime.timer.programStart,
			delta: 0
		};

		var timeoutId;

		function update(input, state)
		{
			var currentTime = input._0;
			var isOn = input._1;
			var wasOn = state.isOn;
			var previousTime = state.time;

			if (isOn)
			{
				timeoutId = localRuntime.setTimeout(notifyTicker, msPerFrame);
			}
			else if (wasOn)
			{
				clearTimeout(timeoutId);
			}

			return {
				isOn: isOn,
				time: currentTime,
				delta: (isOn && !wasOn) ? 0 : currentTime - previousTime
			};
		}

		return A2(
			NS.map,
			function(state) { return state.delta; },
			A3(NS.foldp, F2(update), update(input.value, initialState), input)
		);
	}


	// EVERY

	function every(t)
	{
		var ticker = NS.input('every-' + t, null);
		function tellTime()
		{
			localRuntime.notify(ticker.id, null);
		}
		var clock = A2(NS.map, fst, NS.timestamp(ticker));
		setInterval(tellTime, t);
		return clock;
	}


	function fst(pair)
	{
		return pair._0;
	}


	function read(s)
	{
		var t = Date.parse(s);
		return isNaN(t) ? Maybe.Nothing : Maybe.Just(t);
	}

	return localRuntime.Native.Time.values = {
		fpsWhen: F2(fpsWhen),
		every: every,
		toDate: function(t) { return new Date(t); },
		read: read
	};
};

Elm.Time = Elm.Time || {};
Elm.Time.make = function (_elm) {
   "use strict";
   _elm.Time = _elm.Time || {};
   if (_elm.Time.values) return _elm.Time.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Native$Signal = Elm.Native.Signal.make(_elm),
   $Native$Time = Elm.Native.Time.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var delay = $Native$Signal.delay;
   var since = F2(function (time,signal) {
      var stop = A2($Signal.map,$Basics.always(-1),A2(delay,time,signal));
      var start = A2($Signal.map,$Basics.always(1),signal);
      var delaydiff = A3($Signal.foldp,F2(function (x,y) {    return x + y;}),0,A2($Signal.merge,start,stop));
      return A2($Signal.map,F2(function (x,y) {    return !_U.eq(x,y);})(0),delaydiff);
   });
   var timestamp = $Native$Signal.timestamp;
   var every = $Native$Time.every;
   var fpsWhen = $Native$Time.fpsWhen;
   var fps = function (targetFrames) {    return A2(fpsWhen,targetFrames,$Signal.constant(true));};
   var inMilliseconds = function (t) {    return t;};
   var millisecond = 1;
   var second = 1000 * millisecond;
   var minute = 60 * second;
   var hour = 60 * minute;
   var inHours = function (t) {    return t / hour;};
   var inMinutes = function (t) {    return t / minute;};
   var inSeconds = function (t) {    return t / second;};
   return _elm.Time.values = {_op: _op
                             ,millisecond: millisecond
                             ,second: second
                             ,minute: minute
                             ,hour: hour
                             ,inMilliseconds: inMilliseconds
                             ,inSeconds: inSeconds
                             ,inMinutes: inMinutes
                             ,inHours: inHours
                             ,fps: fps
                             ,fpsWhen: fpsWhen
                             ,every: every
                             ,timestamp: timestamp
                             ,delay: delay
                             ,since: since};
};
Elm.Signal = Elm.Signal || {};
Elm.Signal.Time = Elm.Signal.Time || {};
Elm.Signal.Time.make = function (_elm) {
   "use strict";
   _elm.Signal = _elm.Signal || {};
   _elm.Signal.Time = _elm.Signal.Time || {};
   if (_elm.Signal.Time.values) return _elm.Signal.Time.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Signal$Discrete = Elm.Signal.Discrete.make(_elm),
   $Signal$Extra = Elm.Signal.Extra.make(_elm),
   $Time = Elm.Time.make(_elm);
   var _op = {};
   var timestamp = $Time.timestamp;
   var delay = $Time.delay;
   var since = $Time.since;
   var settledAfter = F2(function (delay,sig) {
      var trailing = A2($Signal$Discrete.whenChangeTo,false,A2(since,delay,sig));
      return A2($Signal.sampleOn,trailing,sig);
   });
   var dropWithin = F2(function (delay,sig) {
      var leading = A2($Signal$Discrete.whenChangeTo,true,A2(since,delay,sig));
      return A2($Signal.sampleOn,leading,sig);
   });
   var timestamps = function (s) {    return A2($Signal$Extra._op["~>"],timestamp(s),$Basics.fst);};
   var limitRate = F2(function (period,sig) {
      var within = F2(function (newt,oldt) {    return _U.cmp(newt - oldt,period) > 0 ? newt : oldt;});
      var windowStart = A3($Signal.foldp,within,0,timestamps(sig));
      return A2($Signal.sampleOn,$Signal$Discrete.whenChange(windowStart),sig);
   });
   var startTime = timestamps($Signal.constant({ctor: "_Tuple0"}));
   var relativeTime = function (s) {    return A2($Signal$Extra._op["~"],A2($Signal$Extra._op["<~"],F2(function (x,y) {    return x - y;}),s),startTime);};
   return _elm.Signal.Time.values = {_op: _op
                                    ,limitRate: limitRate
                                    ,dropWithin: dropWithin
                                    ,settledAfter: settledAfter
                                    ,startTime: startTime
                                    ,relativeTime: relativeTime
                                    ,since: since
                                    ,delay: delay
                                    ,timestamp: timestamp};
};
Elm.Native.Array = {};
Elm.Native.Array.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Array = localRuntime.Native.Array || {};
	if (localRuntime.Native.Array.values)
	{
		return localRuntime.Native.Array.values;
	}
	if ('values' in Elm.Native.Array)
	{
		return localRuntime.Native.Array.values = Elm.Native.Array.values;
	}

	var List = Elm.Native.List.make(localRuntime);

	// A RRB-Tree has two distinct data types.
	// Leaf -> "height"  is always 0
	//         "table"   is an array of elements
	// Node -> "height"  is always greater than 0
	//         "table"   is an array of child nodes
	//         "lengths" is an array of accumulated lengths of the child nodes

	// M is the maximal table size. 32 seems fast. E is the allowed increase
	// of search steps when concatting to find an index. Lower values will
	// decrease balancing, but will increase search steps.
	var M = 32;
	var E = 2;

	// An empty array.
	var empty = {
		ctor: '_Array',
		height: 0,
		table: []
	};


	function get(i, array)
	{
		if (i < 0 || i >= length(array))
		{
			throw new Error(
				'Index ' + i + ' is out of range. Check the length of ' +
				'your array first or use getMaybe or getWithDefault.');
		}
		return unsafeGet(i, array);
	}


	function unsafeGet(i, array)
	{
		for (var x = array.height; x > 0; x--)
		{
			var slot = i >> (x * 5);
			while (array.lengths[slot] <= i)
			{
				slot++;
			}
			if (slot > 0)
			{
				i -= array.lengths[slot - 1];
			}
			array = array.table[slot];
		}
		return array.table[i];
	}


	// Sets the value at the index i. Only the nodes leading to i will get
	// copied and updated.
	function set(i, item, array)
	{
		if (i < 0 || length(array) <= i)
		{
			return array;
		}
		return unsafeSet(i, item, array);
	}


	function unsafeSet(i, item, array)
	{
		array = nodeCopy(array);

		if (array.height === 0)
		{
			array.table[i] = item;
		}
		else
		{
			var slot = getSlot(i, array);
			if (slot > 0)
			{
				i -= array.lengths[slot - 1];
			}
			array.table[slot] = unsafeSet(i, item, array.table[slot]);
		}
		return array;
	}


	function initialize(len, f)
	{
		if (len <= 0)
		{
			return empty;
		}
		var h = Math.floor( Math.log(len) / Math.log(M) );
		return initialize_(f, h, 0, len);
	}

	function initialize_(f, h, from, to)
	{
		if (h === 0)
		{
			var table = new Array((to - from) % (M + 1));
			for (var i = 0; i < table.length; i++)
			{
			  table[i] = f(from + i);
			}
			return {
				ctor: '_Array',
				height: 0,
				table: table
			};
		}

		var step = Math.pow(M, h);
		var table = new Array(Math.ceil((to - from) / step));
		var lengths = new Array(table.length);
		for (var i = 0; i < table.length; i++)
		{
			table[i] = initialize_(f, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
			lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
		}
		return {
			ctor: '_Array',
			height: h,
			table: table,
			lengths: lengths
		};
	}

	function fromList(list)
	{
		if (list === List.Nil)
		{
			return empty;
		}

		// Allocate M sized blocks (table) and write list elements to it.
		var table = new Array(M);
		var nodes = [];
		var i = 0;

		while (list.ctor !== '[]')
		{
			table[i] = list._0;
			list = list._1;
			i++;

			// table is full, so we can push a leaf containing it into the
			// next node.
			if (i === M)
			{
				var leaf = {
					ctor: '_Array',
					height: 0,
					table: table
				};
				fromListPush(leaf, nodes);
				table = new Array(M);
				i = 0;
			}
		}

		// Maybe there is something left on the table.
		if (i > 0)
		{
			var leaf = {
				ctor: '_Array',
				height: 0,
				table: table.splice(0, i)
			};
			fromListPush(leaf, nodes);
		}

		// Go through all of the nodes and eventually push them into higher nodes.
		for (var h = 0; h < nodes.length - 1; h++)
		{
			if (nodes[h].table.length > 0)
			{
				fromListPush(nodes[h], nodes);
			}
		}

		var head = nodes[nodes.length - 1];
		if (head.height > 0 && head.table.length === 1)
		{
			return head.table[0];
		}
		else
		{
			return head;
		}
	}

	// Push a node into a higher node as a child.
	function fromListPush(toPush, nodes)
	{
		var h = toPush.height;

		// Maybe the node on this height does not exist.
		if (nodes.length === h)
		{
			var node = {
				ctor: '_Array',
				height: h + 1,
				table: [],
				lengths: []
			};
			nodes.push(node);
		}

		nodes[h].table.push(toPush);
		var len = length(toPush);
		if (nodes[h].lengths.length > 0)
		{
			len += nodes[h].lengths[nodes[h].lengths.length - 1];
		}
		nodes[h].lengths.push(len);

		if (nodes[h].table.length === M)
		{
			fromListPush(nodes[h], nodes);
			nodes[h] = {
				ctor: '_Array',
				height: h + 1,
				table: [],
				lengths: []
			};
		}
	}

	// Pushes an item via push_ to the bottom right of a tree.
	function push(item, a)
	{
		var pushed = push_(item, a);
		if (pushed !== null)
		{
			return pushed;
		}

		var newTree = create(item, a.height);
		return siblise(a, newTree);
	}

	// Recursively tries to push an item to the bottom-right most
	// tree possible. If there is no space left for the item,
	// null will be returned.
	function push_(item, a)
	{
		// Handle resursion stop at leaf level.
		if (a.height === 0)
		{
			if (a.table.length < M)
			{
				var newA = {
					ctor: '_Array',
					height: 0,
					table: a.table.slice()
				};
				newA.table.push(item);
				return newA;
			}
			else
			{
			  return null;
			}
		}

		// Recursively push
		var pushed = push_(item, botRight(a));

		// There was space in the bottom right tree, so the slot will
		// be updated.
		if (pushed !== null)
		{
			var newA = nodeCopy(a);
			newA.table[newA.table.length - 1] = pushed;
			newA.lengths[newA.lengths.length - 1]++;
			return newA;
		}

		// When there was no space left, check if there is space left
		// for a new slot with a tree which contains only the item
		// at the bottom.
		if (a.table.length < M)
		{
			var newSlot = create(item, a.height - 1);
			var newA = nodeCopy(a);
			newA.table.push(newSlot);
			newA.lengths.push(newA.lengths[newA.lengths.length - 1] + length(newSlot));
			return newA;
		}
		else
		{
			return null;
		}
	}

	// Converts an array into a list of elements.
	function toList(a)
	{
		return toList_(List.Nil, a);
	}

	function toList_(list, a)
	{
		for (var i = a.table.length - 1; i >= 0; i--)
		{
			list =
				a.height === 0
					? List.Cons(a.table[i], list)
					: toList_(list, a.table[i]);
		}
		return list;
	}

	// Maps a function over the elements of an array.
	function map(f, a)
	{
		var newA = {
			ctor: '_Array',
			height: a.height,
			table: new Array(a.table.length)
		};
		if (a.height > 0)
		{
			newA.lengths = a.lengths;
		}
		for (var i = 0; i < a.table.length; i++)
		{
			newA.table[i] =
				a.height === 0
					? f(a.table[i])
					: map(f, a.table[i]);
		}
		return newA;
	}

	// Maps a function over the elements with their index as first argument.
	function indexedMap(f, a)
	{
		return indexedMap_(f, a, 0);
	}

	function indexedMap_(f, a, from)
	{
		var newA = {
			ctor: '_Array',
			height: a.height,
			table: new Array(a.table.length)
		};
		if (a.height > 0)
		{
			newA.lengths = a.lengths;
		}
		for (var i = 0; i < a.table.length; i++)
		{
			newA.table[i] =
				a.height === 0
					? A2(f, from + i, a.table[i])
					: indexedMap_(f, a.table[i], i == 0 ? from : from + a.lengths[i - 1]);
		}
		return newA;
	}

	function foldl(f, b, a)
	{
		if (a.height === 0)
		{
			for (var i = 0; i < a.table.length; i++)
			{
				b = A2(f, a.table[i], b);
			}
		}
		else
		{
			for (var i = 0; i < a.table.length; i++)
			{
				b = foldl(f, b, a.table[i]);
			}
		}
		return b;
	}

	function foldr(f, b, a)
	{
		if (a.height === 0)
		{
			for (var i = a.table.length; i--; )
			{
				b = A2(f, a.table[i], b);
			}
		}
		else
		{
			for (var i = a.table.length; i--; )
			{
				b = foldr(f, b, a.table[i]);
			}
		}
		return b;
	}

	// TODO: currently, it slices the right, then the left. This can be
	// optimized.
	function slice(from, to, a)
	{
		if (from < 0)
		{
			from += length(a);
		}
		if (to < 0)
		{
			to += length(a);
		}
		return sliceLeft(from, sliceRight(to, a));
	}

	function sliceRight(to, a)
	{
		if (to === length(a))
		{
			return a;
		}

		// Handle leaf level.
		if (a.height === 0)
		{
			var newA = { ctor:'_Array', height:0 };
			newA.table = a.table.slice(0, to);
			return newA;
		}

		// Slice the right recursively.
		var right = getSlot(to, a);
		var sliced = sliceRight(to - (right > 0 ? a.lengths[right - 1] : 0), a.table[right]);

		// Maybe the a node is not even needed, as sliced contains the whole slice.
		if (right === 0)
		{
			return sliced;
		}

		// Create new node.
		var newA = {
			ctor: '_Array',
			height: a.height,
			table: a.table.slice(0, right),
			lengths: a.lengths.slice(0, right)
		};
		if (sliced.table.length > 0)
		{
			newA.table[right] = sliced;
			newA.lengths[right] = length(sliced) + (right > 0 ? newA.lengths[right - 1] : 0);
		}
		return newA;
	}

	function sliceLeft(from, a)
	{
		if (from === 0)
		{
			return a;
		}

		// Handle leaf level.
		if (a.height === 0)
		{
			var newA = { ctor:'_Array', height:0 };
			newA.table = a.table.slice(from, a.table.length + 1);
			return newA;
		}

		// Slice the left recursively.
		var left = getSlot(from, a);
		var sliced = sliceLeft(from - (left > 0 ? a.lengths[left - 1] : 0), a.table[left]);

		// Maybe the a node is not even needed, as sliced contains the whole slice.
		if (left === a.table.length - 1)
		{
			return sliced;
		}

		// Create new node.
		var newA = {
			ctor: '_Array',
			height: a.height,
			table: a.table.slice(left, a.table.length + 1),
			lengths: new Array(a.table.length - left)
		};
		newA.table[0] = sliced;
		var len = 0;
		for (var i = 0; i < newA.table.length; i++)
		{
			len += length(newA.table[i]);
			newA.lengths[i] = len;
		}

		return newA;
	}

	// Appends two trees.
	function append(a,b)
	{
		if (a.table.length === 0)
		{
			return b;
		}
		if (b.table.length === 0)
		{
			return a;
		}

		var c = append_(a, b);

		// Check if both nodes can be crunshed together.
		if (c[0].table.length + c[1].table.length <= M)
		{
			if (c[0].table.length === 0)
			{
				return c[1];
			}
			if (c[1].table.length === 0)
			{
				return c[0];
			}

			// Adjust .table and .lengths
			c[0].table = c[0].table.concat(c[1].table);
			if (c[0].height > 0)
			{
				var len = length(c[0]);
				for (var i = 0; i < c[1].lengths.length; i++)
				{
					c[1].lengths[i] += len;
				}
				c[0].lengths = c[0].lengths.concat(c[1].lengths);
			}

			return c[0];
		}

		if (c[0].height > 0)
		{
			var toRemove = calcToRemove(a, b);
			if (toRemove > E)
			{
				c = shuffle(c[0], c[1], toRemove);
			}
		}

		return siblise(c[0], c[1]);
	}

	// Returns an array of two nodes; right and left. One node _may_ be empty.
	function append_(a, b)
	{
		if (a.height === 0 && b.height === 0)
		{
			return [a, b];
		}

		if (a.height !== 1 || b.height !== 1)
		{
			if (a.height === b.height)
			{
				a = nodeCopy(a);
				b = nodeCopy(b);
				var appended = append_(botRight(a), botLeft(b));

				insertRight(a, appended[1]);
				insertLeft(b, appended[0]);
			}
			else if (a.height > b.height)
			{
				a = nodeCopy(a);
				var appended = append_(botRight(a), b);

				insertRight(a, appended[0]);
				b = parentise(appended[1], appended[1].height + 1);
			}
			else
			{
				b = nodeCopy(b);
				var appended = append_(a, botLeft(b));

				var left = appended[0].table.length === 0 ? 0 : 1;
				var right = left === 0 ? 1 : 0;
				insertLeft(b, appended[left]);
				a = parentise(appended[right], appended[right].height + 1);
			}
		}

		// Check if balancing is needed and return based on that.
		if (a.table.length === 0 || b.table.length === 0)
		{
			return [a, b];
		}

		var toRemove = calcToRemove(a, b);
		if (toRemove <= E)
		{
			return [a, b];
		}
		return shuffle(a, b, toRemove);
	}

	// Helperfunctions for append_. Replaces a child node at the side of the parent.
	function insertRight(parent, node)
	{
		var index = parent.table.length - 1;
		parent.table[index] = node;
		parent.lengths[index] = length(node);
		parent.lengths[index] += index > 0 ? parent.lengths[index - 1] : 0;
	}

	function insertLeft(parent, node)
	{
		if (node.table.length > 0)
		{
			parent.table[0] = node;
			parent.lengths[0] = length(node);

			var len = length(parent.table[0]);
			for (var i = 1; i < parent.lengths.length; i++)
			{
				len += length(parent.table[i]);
				parent.lengths[i] = len;
			}
		}
		else
		{
			parent.table.shift();
			for (var i = 1; i < parent.lengths.length; i++)
			{
				parent.lengths[i] = parent.lengths[i] - parent.lengths[0];
			}
			parent.lengths.shift();
		}
	}

	// Returns the extra search steps for E. Refer to the paper.
	function calcToRemove(a, b)
	{
		var subLengths = 0;
		for (var i = 0; i < a.table.length; i++)
		{
			subLengths += a.table[i].table.length;
		}
		for (var i = 0; i < b.table.length; i++)
		{
			subLengths += b.table[i].table.length;
		}

		var toRemove = a.table.length + b.table.length;
		return toRemove - (Math.floor((subLengths - 1) / M) + 1);
	}

	// get2, set2 and saveSlot are helpers for accessing elements over two arrays.
	function get2(a, b, index)
	{
		return index < a.length
			? a[index]
			: b[index - a.length];
	}

	function set2(a, b, index, value)
	{
		if (index < a.length)
		{
			a[index] = value;
		}
		else
		{
			b[index - a.length] = value;
		}
	}

	function saveSlot(a, b, index, slot)
	{
		set2(a.table, b.table, index, slot);

		var l = (index === 0 || index === a.lengths.length)
			? 0
			: get2(a.lengths, a.lengths, index - 1);

		set2(a.lengths, b.lengths, index, l + length(slot));
	}

	// Creates a node or leaf with a given length at their arrays for perfomance.
	// Is only used by shuffle.
	function createNode(h, length)
	{
		if (length < 0)
		{
			length = 0;
		}
		var a = {
			ctor: '_Array',
			height: h,
			table: new Array(length)
		};
		if (h > 0)
		{
			a.lengths = new Array(length);
		}
		return a;
	}

	// Returns an array of two balanced nodes.
	function shuffle(a, b, toRemove)
	{
		var newA = createNode(a.height, Math.min(M, a.table.length + b.table.length - toRemove));
		var newB = createNode(a.height, newA.table.length - (a.table.length + b.table.length - toRemove));

		// Skip the slots with size M. More precise: copy the slot references
		// to the new node
		var read = 0;
		while (get2(a.table, b.table, read).table.length % M === 0)
		{
			set2(newA.table, newB.table, read, get2(a.table, b.table, read));
			set2(newA.lengths, newB.lengths, read, get2(a.lengths, b.lengths, read));
			read++;
		}

		// Pulling items from left to right, caching in a slot before writing
		// it into the new nodes.
		var write = read;
		var slot = new createNode(a.height - 1, 0);
		var from = 0;

		// If the current slot is still containing data, then there will be at
		// least one more write, so we do not break this loop yet.
		while (read - write - (slot.table.length > 0 ? 1 : 0) < toRemove)
		{
			// Find out the max possible items for copying.
			var source = get2(a.table, b.table, read);
			var to = Math.min(M - slot.table.length, source.table.length);

			// Copy and adjust size table.
			slot.table = slot.table.concat(source.table.slice(from, to));
			if (slot.height > 0)
			{
				var len = slot.lengths.length;
				for (var i = len; i < len + to - from; i++)
				{
					slot.lengths[i] = length(slot.table[i]);
					slot.lengths[i] += (i > 0 ? slot.lengths[i - 1] : 0);
				}
			}

			from += to;

			// Only proceed to next slots[i] if the current one was
			// fully copied.
			if (source.table.length <= to)
			{
				read++; from = 0;
			}

			// Only create a new slot if the current one is filled up.
			if (slot.table.length === M)
			{
				saveSlot(newA, newB, write, slot);
				slot = createNode(a.height - 1, 0);
				write++;
			}
		}

		// Cleanup after the loop. Copy the last slot into the new nodes.
		if (slot.table.length > 0)
		{
			saveSlot(newA, newB, write, slot);
			write++;
		}

		// Shift the untouched slots to the left
		while (read < a.table.length + b.table.length )
		{
			saveSlot(newA, newB, write, get2(a.table, b.table, read));
			read++;
			write++;
		}

		return [newA, newB];
	}

	// Navigation functions
	function botRight(a)
	{
		return a.table[a.table.length - 1];
	}
	function botLeft(a)
	{
		return a.table[0];
	}

	// Copies a node for updating. Note that you should not use this if
	// only updating only one of "table" or "lengths" for performance reasons.
	function nodeCopy(a)
	{
		var newA = {
			ctor: '_Array',
			height: a.height,
			table: a.table.slice()
		};
		if (a.height > 0)
		{
			newA.lengths = a.lengths.slice();
		}
		return newA;
	}

	// Returns how many items are in the tree.
	function length(array)
	{
		if (array.height === 0)
		{
			return array.table.length;
		}
		else
		{
			return array.lengths[array.lengths.length - 1];
		}
	}

	// Calculates in which slot of "table" the item probably is, then
	// find the exact slot via forward searching in  "lengths". Returns the index.
	function getSlot(i, a)
	{
		var slot = i >> (5 * a.height);
		while (a.lengths[slot] <= i)
		{
			slot++;
		}
		return slot;
	}

	// Recursively creates a tree with a given height containing
	// only the given item.
	function create(item, h)
	{
		if (h === 0)
		{
			return {
				ctor: '_Array',
				height: 0,
				table: [item]
			};
		}
		return {
			ctor: '_Array',
			height: h,
			table: [create(item, h - 1)],
			lengths: [1]
		};
	}

	// Recursively creates a tree that contains the given tree.
	function parentise(tree, h)
	{
		if (h === tree.height)
		{
			return tree;
		}

		return {
			ctor: '_Array',
			height: h,
			table: [parentise(tree, h - 1)],
			lengths: [length(tree)]
		};
	}

	// Emphasizes blood brotherhood beneath two trees.
	function siblise(a, b)
	{
		return {
			ctor: '_Array',
			height: a.height + 1,
			table: [a, b],
			lengths: [length(a), length(a) + length(b)]
		};
	}

	function toJSArray(a)
	{
		var jsArray = new Array(length(a));
		toJSArray_(jsArray, 0, a);
		return jsArray;
	}

	function toJSArray_(jsArray, i, a)
	{
		for (var t = 0; t < a.table.length; t++)
		{
			if (a.height === 0)
			{
				jsArray[i + t] = a.table[t];
			}
			else
			{
				var inc = t === 0 ? 0 : a.lengths[t - 1];
				toJSArray_(jsArray, i + inc, a.table[t]);
			}
		}
	}

	function fromJSArray(jsArray)
	{
		if (jsArray.length === 0)
		{
			return empty;
		}
		var h = Math.floor(Math.log(jsArray.length) / Math.log(M));
		return fromJSArray_(jsArray, h, 0, jsArray.length);
	}

	function fromJSArray_(jsArray, h, from, to)
	{
		if (h === 0)
		{
			return {
				ctor: '_Array',
				height: 0,
				table: jsArray.slice(from, to)
			};
		}

		var step = Math.pow(M, h);
		var table = new Array(Math.ceil((to - from) / step));
		var lengths = new Array(table.length);
		for (var i = 0; i < table.length; i++)
		{
			table[i] = fromJSArray_(jsArray, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
			lengths[i] = length(table[i]) + (i > 0 ? lengths[i - 1] : 0);
		}
		return {
			ctor: '_Array',
			height: h,
			table: table,
			lengths: lengths
		};
	}

	Elm.Native.Array.values = {
		empty: empty,
		fromList: fromList,
		toList: toList,
		initialize: F2(initialize),
		append: F2(append),
		push: F2(push),
		slice: F3(slice),
		get: F2(get),
		set: F3(set),
		map: F2(map),
		indexedMap: F2(indexedMap),
		foldl: F3(foldl),
		foldr: F3(foldr),
		length: length,

		toJSArray: toJSArray,
		fromJSArray: fromJSArray
	};

	return localRuntime.Native.Array.values = Elm.Native.Array.values;
};

Elm.Array = Elm.Array || {};
Elm.Array.make = function (_elm) {
   "use strict";
   _elm.Array = _elm.Array || {};
   if (_elm.Array.values) return _elm.Array.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Array = Elm.Native.Array.make(_elm);
   var _op = {};
   var append = $Native$Array.append;
   var length = $Native$Array.length;
   var isEmpty = function (array) {    return _U.eq(length(array),0);};
   var slice = $Native$Array.slice;
   var set = $Native$Array.set;
   var get = F2(function (i,array) {
      return _U.cmp(0,i) < 1 && _U.cmp(i,$Native$Array.length(array)) < 0 ? $Maybe.Just(A2($Native$Array.get,i,array)) : $Maybe.Nothing;
   });
   var push = $Native$Array.push;
   var empty = $Native$Array.empty;
   var filter = F2(function (isOkay,arr) {
      var update = F2(function (x,xs) {    return isOkay(x) ? A2($Native$Array.push,x,xs) : xs;});
      return A3($Native$Array.foldl,update,$Native$Array.empty,arr);
   });
   var foldr = $Native$Array.foldr;
   var foldl = $Native$Array.foldl;
   var indexedMap = $Native$Array.indexedMap;
   var map = $Native$Array.map;
   var toIndexedList = function (array) {
      return A3($List.map2,
      F2(function (v0,v1) {    return {ctor: "_Tuple2",_0: v0,_1: v1};}),
      _U.range(0,$Native$Array.length(array) - 1),
      $Native$Array.toList(array));
   };
   var toList = $Native$Array.toList;
   var fromList = $Native$Array.fromList;
   var initialize = $Native$Array.initialize;
   var repeat = F2(function (n,e) {    return A2(initialize,n,$Basics.always(e));});
   var Array = {ctor: "Array"};
   return _elm.Array.values = {_op: _op
                              ,empty: empty
                              ,repeat: repeat
                              ,initialize: initialize
                              ,fromList: fromList
                              ,isEmpty: isEmpty
                              ,length: length
                              ,push: push
                              ,append: append
                              ,get: get
                              ,set: set
                              ,slice: slice
                              ,toList: toList
                              ,toIndexedList: toIndexedList
                              ,map: map
                              ,indexedMap: indexedMap
                              ,filter: filter
                              ,foldl: foldl
                              ,foldr: foldr};
};
Elm.Native.Char = {};
Elm.Native.Char.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Char = localRuntime.Native.Char || {};
	if (localRuntime.Native.Char.values)
	{
		return localRuntime.Native.Char.values;
	}

	var Utils = Elm.Native.Utils.make(localRuntime);

	return localRuntime.Native.Char.values = {
		fromCode: function(c) { return Utils.chr(String.fromCharCode(c)); },
		toCode: function(c) { return c.charCodeAt(0); },
		toUpper: function(c) { return Utils.chr(c.toUpperCase()); },
		toLower: function(c) { return Utils.chr(c.toLowerCase()); },
		toLocaleUpper: function(c) { return Utils.chr(c.toLocaleUpperCase()); },
		toLocaleLower: function(c) { return Utils.chr(c.toLocaleLowerCase()); }
	};
};

Elm.Char = Elm.Char || {};
Elm.Char.make = function (_elm) {
   "use strict";
   _elm.Char = _elm.Char || {};
   if (_elm.Char.values) return _elm.Char.values;
   var _U = Elm.Native.Utils.make(_elm),$Basics = Elm.Basics.make(_elm),$Native$Char = Elm.Native.Char.make(_elm);
   var _op = {};
   var fromCode = $Native$Char.fromCode;
   var toCode = $Native$Char.toCode;
   var toLocaleLower = $Native$Char.toLocaleLower;
   var toLocaleUpper = $Native$Char.toLocaleUpper;
   var toLower = $Native$Char.toLower;
   var toUpper = $Native$Char.toUpper;
   var isBetween = F3(function (low,high,$char) {    var code = toCode($char);return _U.cmp(code,toCode(low)) > -1 && _U.cmp(code,toCode(high)) < 1;});
   var isUpper = A2(isBetween,_U.chr("A"),_U.chr("Z"));
   var isLower = A2(isBetween,_U.chr("a"),_U.chr("z"));
   var isDigit = A2(isBetween,_U.chr("0"),_U.chr("9"));
   var isOctDigit = A2(isBetween,_U.chr("0"),_U.chr("7"));
   var isHexDigit = function ($char) {
      return isDigit($char) || (A3(isBetween,_U.chr("a"),_U.chr("f"),$char) || A3(isBetween,_U.chr("A"),_U.chr("F"),$char));
   };
   return _elm.Char.values = {_op: _op
                             ,isUpper: isUpper
                             ,isLower: isLower
                             ,isDigit: isDigit
                             ,isOctDigit: isOctDigit
                             ,isHexDigit: isHexDigit
                             ,toUpper: toUpper
                             ,toLower: toLower
                             ,toLocaleUpper: toLocaleUpper
                             ,toLocaleLower: toLocaleLower
                             ,toCode: toCode
                             ,fromCode: fromCode};
};
Elm.Native.String = {};

Elm.Native.String.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.String = localRuntime.Native.String || {};
	if (localRuntime.Native.String.values)
	{
		return localRuntime.Native.String.values;
	}
	if ('values' in Elm.Native.String)
	{
		return localRuntime.Native.String.values = Elm.Native.String.values;
	}


	var Char = Elm.Char.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);
	var Result = Elm.Result.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);

	function isEmpty(str)
	{
		return str.length === 0;
	}
	function cons(chr, str)
	{
		return chr + str;
	}
	function uncons(str)
	{
		var hd = str[0];
		if (hd)
		{
			return Maybe.Just(Utils.Tuple2(Utils.chr(hd), str.slice(1)));
		}
		return Maybe.Nothing;
	}
	function append(a, b)
	{
		return a + b;
	}
	function concat(strs)
	{
		return List.toArray(strs).join('');
	}
	function length(str)
	{
		return str.length;
	}
	function map(f, str)
	{
		var out = str.split('');
		for (var i = out.length; i--; )
		{
			out[i] = f(Utils.chr(out[i]));
		}
		return out.join('');
	}
	function filter(pred, str)
	{
		return str.split('').map(Utils.chr).filter(pred).join('');
	}
	function reverse(str)
	{
		return str.split('').reverse().join('');
	}
	function foldl(f, b, str)
	{
		var len = str.length;
		for (var i = 0; i < len; ++i)
		{
			b = A2(f, Utils.chr(str[i]), b);
		}
		return b;
	}
	function foldr(f, b, str)
	{
		for (var i = str.length; i--; )
		{
			b = A2(f, Utils.chr(str[i]), b);
		}
		return b;
	}
	function split(sep, str)
	{
		return List.fromArray(str.split(sep));
	}
	function join(sep, strs)
	{
		return List.toArray(strs).join(sep);
	}
	function repeat(n, str)
	{
		var result = '';
		while (n > 0)
		{
			if (n & 1)
			{
				result += str;
			}
			n >>= 1, str += str;
		}
		return result;
	}
	function slice(start, end, str)
	{
		return str.slice(start, end);
	}
	function left(n, str)
	{
		return n < 1 ? '' : str.slice(0, n);
	}
	function right(n, str)
	{
		return n < 1 ? '' : str.slice(-n);
	}
	function dropLeft(n, str)
	{
		return n < 1 ? str : str.slice(n);
	}
	function dropRight(n, str)
	{
		return n < 1 ? str : str.slice(0, -n);
	}
	function pad(n, chr, str)
	{
		var half = (n - str.length) / 2;
		return repeat(Math.ceil(half), chr) + str + repeat(half | 0, chr);
	}
	function padRight(n, chr, str)
	{
		return str + repeat(n - str.length, chr);
	}
	function padLeft(n, chr, str)
	{
		return repeat(n - str.length, chr) + str;
	}

	function trim(str)
	{
		return str.trim();
	}
	function trimLeft(str)
	{
		return str.replace(/^\s+/, '');
	}
	function trimRight(str)
	{
		return str.replace(/\s+$/, '');
	}

	function words(str)
	{
		return List.fromArray(str.trim().split(/\s+/g));
	}
	function lines(str)
	{
		return List.fromArray(str.split(/\r\n|\r|\n/g));
	}

	function toUpper(str)
	{
		return str.toUpperCase();
	}
	function toLower(str)
	{
		return str.toLowerCase();
	}

	function any(pred, str)
	{
		for (var i = str.length; i--; )
		{
			if (pred(Utils.chr(str[i])))
			{
				return true;
			}
		}
		return false;
	}
	function all(pred, str)
	{
		for (var i = str.length; i--; )
		{
			if (!pred(Utils.chr(str[i])))
			{
				return false;
			}
		}
		return true;
	}

	function contains(sub, str)
	{
		return str.indexOf(sub) > -1;
	}
	function startsWith(sub, str)
	{
		return str.indexOf(sub) === 0;
	}
	function endsWith(sub, str)
	{
		return str.length >= sub.length &&
			str.lastIndexOf(sub) === str.length - sub.length;
	}
	function indexes(sub, str)
	{
		var subLen = sub.length;
		var i = 0;
		var is = [];
		while ((i = str.indexOf(sub, i)) > -1)
		{
			is.push(i);
			i = i + subLen;
		}
		return List.fromArray(is);
	}

	function toInt(s)
	{
		var len = s.length;
		if (len === 0)
		{
			return Result.Err("could not convert string '" + s + "' to an Int" );
		}
		var start = 0;
		if (s[0] === '-')
		{
			if (len === 1)
			{
				return Result.Err("could not convert string '" + s + "' to an Int" );
			}
			start = 1;
		}
		for (var i = start; i < len; ++i)
		{
			if (!Char.isDigit(s[i]))
			{
				return Result.Err("could not convert string '" + s + "' to an Int" );
			}
		}
		return Result.Ok(parseInt(s, 10));
	}

	function toFloat(s)
	{
		var len = s.length;
		if (len === 0)
		{
			return Result.Err("could not convert string '" + s + "' to a Float" );
		}
		var start = 0;
		if (s[0] === '-')
		{
			if (len === 1)
			{
				return Result.Err("could not convert string '" + s + "' to a Float" );
			}
			start = 1;
		}
		var dotCount = 0;
		for (var i = start; i < len; ++i)
		{
			if (Char.isDigit(s[i]))
			{
				continue;
			}
			if (s[i] === '.')
			{
				dotCount += 1;
				if (dotCount <= 1)
				{
					continue;
				}
			}
			return Result.Err("could not convert string '" + s + "' to a Float" );
		}
		return Result.Ok(parseFloat(s));
	}

	function toList(str)
	{
		return List.fromArray(str.split('').map(Utils.chr));
	}
	function fromList(chars)
	{
		return List.toArray(chars).join('');
	}

	return Elm.Native.String.values = {
		isEmpty: isEmpty,
		cons: F2(cons),
		uncons: uncons,
		append: F2(append),
		concat: concat,
		length: length,
		map: F2(map),
		filter: F2(filter),
		reverse: reverse,
		foldl: F3(foldl),
		foldr: F3(foldr),

		split: F2(split),
		join: F2(join),
		repeat: F2(repeat),

		slice: F3(slice),
		left: F2(left),
		right: F2(right),
		dropLeft: F2(dropLeft),
		dropRight: F2(dropRight),

		pad: F3(pad),
		padLeft: F3(padLeft),
		padRight: F3(padRight),

		trim: trim,
		trimLeft: trimLeft,
		trimRight: trimRight,

		words: words,
		lines: lines,

		toUpper: toUpper,
		toLower: toLower,

		any: F2(any),
		all: F2(all),

		contains: F2(contains),
		startsWith: F2(startsWith),
		endsWith: F2(endsWith),
		indexes: F2(indexes),

		toInt: toInt,
		toFloat: toFloat,
		toList: toList,
		fromList: fromList
	};
};

Elm.String = Elm.String || {};
Elm.String.make = function (_elm) {
   "use strict";
   _elm.String = _elm.String || {};
   if (_elm.String.values) return _elm.String.values;
   var _U = Elm.Native.Utils.make(_elm),$Maybe = Elm.Maybe.make(_elm),$Native$String = Elm.Native.String.make(_elm),$Result = Elm.Result.make(_elm);
   var _op = {};
   var fromList = $Native$String.fromList;
   var toList = $Native$String.toList;
   var toFloat = $Native$String.toFloat;
   var toInt = $Native$String.toInt;
   var indices = $Native$String.indexes;
   var indexes = $Native$String.indexes;
   var endsWith = $Native$String.endsWith;
   var startsWith = $Native$String.startsWith;
   var contains = $Native$String.contains;
   var all = $Native$String.all;
   var any = $Native$String.any;
   var toLower = $Native$String.toLower;
   var toUpper = $Native$String.toUpper;
   var lines = $Native$String.lines;
   var words = $Native$String.words;
   var trimRight = $Native$String.trimRight;
   var trimLeft = $Native$String.trimLeft;
   var trim = $Native$String.trim;
   var padRight = $Native$String.padRight;
   var padLeft = $Native$String.padLeft;
   var pad = $Native$String.pad;
   var dropRight = $Native$String.dropRight;
   var dropLeft = $Native$String.dropLeft;
   var right = $Native$String.right;
   var left = $Native$String.left;
   var slice = $Native$String.slice;
   var repeat = $Native$String.repeat;
   var join = $Native$String.join;
   var split = $Native$String.split;
   var foldr = $Native$String.foldr;
   var foldl = $Native$String.foldl;
   var reverse = $Native$String.reverse;
   var filter = $Native$String.filter;
   var map = $Native$String.map;
   var length = $Native$String.length;
   var concat = $Native$String.concat;
   var append = $Native$String.append;
   var uncons = $Native$String.uncons;
   var cons = $Native$String.cons;
   var fromChar = function ($char) {    return A2(cons,$char,"");};
   var isEmpty = $Native$String.isEmpty;
   return _elm.String.values = {_op: _op
                               ,isEmpty: isEmpty
                               ,length: length
                               ,reverse: reverse
                               ,repeat: repeat
                               ,cons: cons
                               ,uncons: uncons
                               ,fromChar: fromChar
                               ,append: append
                               ,concat: concat
                               ,split: split
                               ,join: join
                               ,words: words
                               ,lines: lines
                               ,slice: slice
                               ,left: left
                               ,right: right
                               ,dropLeft: dropLeft
                               ,dropRight: dropRight
                               ,contains: contains
                               ,startsWith: startsWith
                               ,endsWith: endsWith
                               ,indexes: indexes
                               ,indices: indices
                               ,toInt: toInt
                               ,toFloat: toFloat
                               ,toList: toList
                               ,fromList: fromList
                               ,toUpper: toUpper
                               ,toLower: toLower
                               ,pad: pad
                               ,padLeft: padLeft
                               ,padRight: padRight
                               ,trim: trim
                               ,trimLeft: trimLeft
                               ,trimRight: trimRight
                               ,map: map
                               ,filter: filter
                               ,foldl: foldl
                               ,foldr: foldr
                               ,any: any
                               ,all: all};
};
Elm.Dict = Elm.Dict || {};
Elm.Dict.make = function (_elm) {
   "use strict";
   _elm.Dict = _elm.Dict || {};
   if (_elm.Dict.values) return _elm.Dict.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Debug = Elm.Native.Debug.make(_elm),
   $String = Elm.String.make(_elm);
   var _op = {};
   var foldr = F3(function (f,acc,t) {
      foldr: while (true) {
         var _p0 = t;
         if (_p0.ctor === "RBEmpty_elm_builtin") {
               return acc;
            } else {
               var _v1 = f,_v2 = A3(f,_p0._1,_p0._2,A3(foldr,f,acc,_p0._4)),_v3 = _p0._3;
               f = _v1;
               acc = _v2;
               t = _v3;
               continue foldr;
            }
      }
   });
   var keys = function (dict) {    return A3(foldr,F3(function (key,value,keyList) {    return A2($List._op["::"],key,keyList);}),_U.list([]),dict);};
   var values = function (dict) {    return A3(foldr,F3(function (key,value,valueList) {    return A2($List._op["::"],value,valueList);}),_U.list([]),dict);};
   var toList = function (dict) {
      return A3(foldr,F3(function (key,value,list) {    return A2($List._op["::"],{ctor: "_Tuple2",_0: key,_1: value},list);}),_U.list([]),dict);
   };
   var foldl = F3(function (f,acc,dict) {
      foldl: while (true) {
         var _p1 = dict;
         if (_p1.ctor === "RBEmpty_elm_builtin") {
               return acc;
            } else {
               var _v5 = f,_v6 = A3(f,_p1._1,_p1._2,A3(foldl,f,acc,_p1._3)),_v7 = _p1._4;
               f = _v5;
               acc = _v6;
               dict = _v7;
               continue foldl;
            }
      }
   });
   var reportRemBug = F4(function (msg,c,lgot,rgot) {
      return $Native$Debug.crash($String.concat(_U.list(["Internal red-black tree invariant violated, expected "
                                                        ,msg
                                                        ," and got "
                                                        ,$Basics.toString(c)
                                                        ,"/"
                                                        ,lgot
                                                        ,"/"
                                                        ,rgot
                                                        ,"\nPlease report this bug to <https://github.com/elm-lang/core/issues>"])));
   });
   var isBBlack = function (dict) {
      var _p2 = dict;
      _v8_2: do {
         if (_p2.ctor === "RBNode_elm_builtin") {
               if (_p2._0.ctor === "BBlack") {
                     return true;
                  } else {
                     break _v8_2;
                  }
            } else {
               if (_p2._0.ctor === "LBBlack") {
                     return true;
                  } else {
                     break _v8_2;
                  }
            }
      } while (false);
      return false;
   };
   var Same = {ctor: "Same"};
   var Remove = {ctor: "Remove"};
   var Insert = {ctor: "Insert"};
   var sizeHelp = F2(function (n,dict) {
      sizeHelp: while (true) {
         var _p3 = dict;
         if (_p3.ctor === "RBEmpty_elm_builtin") {
               return n;
            } else {
               var _v10 = A2(sizeHelp,n + 1,_p3._4),_v11 = _p3._3;
               n = _v10;
               dict = _v11;
               continue sizeHelp;
            }
      }
   });
   var size = function (dict) {    return A2(sizeHelp,0,dict);};
   var get = F2(function (targetKey,dict) {
      get: while (true) {
         var _p4 = dict;
         if (_p4.ctor === "RBEmpty_elm_builtin") {
               return $Maybe.Nothing;
            } else {
               var _p5 = A2($Basics.compare,targetKey,_p4._1);
               switch (_p5.ctor)
               {case "LT": var _v14 = targetKey,_v15 = _p4._3;
                    targetKey = _v14;
                    dict = _v15;
                    continue get;
                  case "EQ": return $Maybe.Just(_p4._2);
                  default: var _v16 = targetKey,_v17 = _p4._4;
                    targetKey = _v16;
                    dict = _v17;
                    continue get;}
            }
      }
   });
   var member = F2(function (key,dict) {    var _p6 = A2(get,key,dict);if (_p6.ctor === "Just") {    return true;} else {    return false;}});
   var maxWithDefault = F3(function (k,v,r) {
      maxWithDefault: while (true) {
         var _p7 = r;
         if (_p7.ctor === "RBEmpty_elm_builtin") {
               return {ctor: "_Tuple2",_0: k,_1: v};
            } else {
               var _v20 = _p7._1,_v21 = _p7._2,_v22 = _p7._4;
               k = _v20;
               v = _v21;
               r = _v22;
               continue maxWithDefault;
            }
      }
   });
   var RBEmpty_elm_builtin = function (a) {    return {ctor: "RBEmpty_elm_builtin",_0: a};};
   var RBNode_elm_builtin = F5(function (a,b,c,d,e) {    return {ctor: "RBNode_elm_builtin",_0: a,_1: b,_2: c,_3: d,_4: e};});
   var LBBlack = {ctor: "LBBlack"};
   var LBlack = {ctor: "LBlack"};
   var empty = RBEmpty_elm_builtin(LBlack);
   var isEmpty = function (dict) {    return _U.eq(dict,empty);};
   var map = F2(function (f,dict) {
      var _p8 = dict;
      if (_p8.ctor === "RBEmpty_elm_builtin") {
            return RBEmpty_elm_builtin(LBlack);
         } else {
            var _p9 = _p8._1;
            return A5(RBNode_elm_builtin,_p8._0,_p9,A2(f,_p9,_p8._2),A2(map,f,_p8._3),A2(map,f,_p8._4));
         }
   });
   var NBlack = {ctor: "NBlack"};
   var BBlack = {ctor: "BBlack"};
   var Black = {ctor: "Black"};
   var ensureBlackRoot = function (dict) {
      var _p10 = dict;
      if (_p10.ctor === "RBNode_elm_builtin" && _p10._0.ctor === "Red") {
            return A5(RBNode_elm_builtin,Black,_p10._1,_p10._2,_p10._3,_p10._4);
         } else {
            return dict;
         }
   };
   var blackish = function (t) {
      var _p11 = t;
      if (_p11.ctor === "RBNode_elm_builtin") {
            var _p12 = _p11._0;
            return _U.eq(_p12,Black) || _U.eq(_p12,BBlack);
         } else {
            return true;
         }
   };
   var blacken = function (t) {
      var _p13 = t;
      if (_p13.ctor === "RBEmpty_elm_builtin") {
            return RBEmpty_elm_builtin(LBlack);
         } else {
            return A5(RBNode_elm_builtin,Black,_p13._1,_p13._2,_p13._3,_p13._4);
         }
   };
   var Red = {ctor: "Red"};
   var moreBlack = function (color) {
      var _p14 = color;
      switch (_p14.ctor)
      {case "Black": return BBlack;
         case "Red": return Black;
         case "NBlack": return Red;
         default: return $Native$Debug.crash("Can\'t make a double black node more black!");}
   };
   var lessBlack = function (color) {
      var _p15 = color;
      switch (_p15.ctor)
      {case "BBlack": return Black;
         case "Black": return Red;
         case "Red": return NBlack;
         default: return $Native$Debug.crash("Can\'t make a negative black node less black!");}
   };
   var lessBlackTree = function (dict) {
      var _p16 = dict;
      if (_p16.ctor === "RBNode_elm_builtin") {
            return A5(RBNode_elm_builtin,lessBlack(_p16._0),_p16._1,_p16._2,_p16._3,_p16._4);
         } else {
            return RBEmpty_elm_builtin(LBlack);
         }
   };
   var balancedTree = function (col) {
      return function (xk) {
         return function (xv) {
            return function (yk) {
               return function (yv) {
                  return function (zk) {
                     return function (zv) {
                        return function (a) {
                           return function (b) {
                              return function (c) {
                                 return function (d) {
                                    return A5(RBNode_elm_builtin,
                                    lessBlack(col),
                                    yk,
                                    yv,
                                    A5(RBNode_elm_builtin,Black,xk,xv,a,b),
                                    A5(RBNode_elm_builtin,Black,zk,zv,c,d));
                                 };
                              };
                           };
                        };
                     };
                  };
               };
            };
         };
      };
   };
   var redden = function (t) {
      var _p17 = t;
      if (_p17.ctor === "RBEmpty_elm_builtin") {
            return $Native$Debug.crash("can\'t make a Leaf red");
         } else {
            return A5(RBNode_elm_builtin,Red,_p17._1,_p17._2,_p17._3,_p17._4);
         }
   };
   var balanceHelp = function (tree) {
      var _p18 = tree;
      _v31_6: do {
         _v31_5: do {
            _v31_4: do {
               _v31_3: do {
                  _v31_2: do {
                     _v31_1: do {
                        _v31_0: do {
                           if (_p18.ctor === "RBNode_elm_builtin") {
                                 if (_p18._3.ctor === "RBNode_elm_builtin") {
                                       if (_p18._4.ctor === "RBNode_elm_builtin") {
                                             switch (_p18._3._0.ctor)
                                             {case "Red": switch (_p18._4._0.ctor)
                                                  {case "Red": if (_p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Red") {
                                                             break _v31_0;
                                                          } else {
                                                             if (_p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Red") {
                                                                   break _v31_1;
                                                                } else {
                                                                   if (_p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Red") {
                                                                         break _v31_2;
                                                                      } else {
                                                                         if (_p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Red") {
                                                                               break _v31_3;
                                                                            } else {
                                                                               break _v31_6;
                                                                            }
                                                                      }
                                                                }
                                                          }
                                                     case "NBlack": if (_p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Red") {
                                                             break _v31_0;
                                                          } else {
                                                             if (_p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Red") {
                                                                   break _v31_1;
                                                                } else {
                                                                   if (_p18._0.ctor === "BBlack" && _p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Black" && _p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Black")
                                                                   {
                                                                         break _v31_4;
                                                                      } else {
                                                                         break _v31_6;
                                                                      }
                                                                }
                                                          }
                                                     default: if (_p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Red") {
                                                             break _v31_0;
                                                          } else {
                                                             if (_p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Red") {
                                                                   break _v31_1;
                                                                } else {
                                                                   break _v31_6;
                                                                }
                                                          }}
                                                case "NBlack": switch (_p18._4._0.ctor)
                                                  {case "Red": if (_p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Red") {
                                                             break _v31_2;
                                                          } else {
                                                             if (_p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Red") {
                                                                   break _v31_3;
                                                                } else {
                                                                   if (_p18._0.ctor === "BBlack" && _p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Black" && _p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Black")
                                                                   {
                                                                         break _v31_5;
                                                                      } else {
                                                                         break _v31_6;
                                                                      }
                                                                }
                                                          }
                                                     case "NBlack": if (_p18._0.ctor === "BBlack") {
                                                             if (_p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Black" && _p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Black")
                                                             {
                                                                   break _v31_4;
                                                                } else {
                                                                   if (_p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Black" && _p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Black")
                                                                   {
                                                                         break _v31_5;
                                                                      } else {
                                                                         break _v31_6;
                                                                      }
                                                                }
                                                          } else {
                                                             break _v31_6;
                                                          }
                                                     default:
                                                     if (_p18._0.ctor === "BBlack" && _p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Black" && _p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Black")
                                                       {
                                                             break _v31_5;
                                                          } else {
                                                             break _v31_6;
                                                          }}
                                                default: switch (_p18._4._0.ctor)
                                                  {case "Red": if (_p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Red") {
                                                             break _v31_2;
                                                          } else {
                                                             if (_p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Red") {
                                                                   break _v31_3;
                                                                } else {
                                                                   break _v31_6;
                                                                }
                                                          }
                                                     case "NBlack":
                                                     if (_p18._0.ctor === "BBlack" && _p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Black" && _p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Black")
                                                       {
                                                             break _v31_4;
                                                          } else {
                                                             break _v31_6;
                                                          }
                                                     default: break _v31_6;}}
                                          } else {
                                             switch (_p18._3._0.ctor)
                                             {case "Red": if (_p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Red") {
                                                        break _v31_0;
                                                     } else {
                                                        if (_p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Red") {
                                                              break _v31_1;
                                                           } else {
                                                              break _v31_6;
                                                           }
                                                     }
                                                case "NBlack":
                                                if (_p18._0.ctor === "BBlack" && _p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Black" && _p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Black")
                                                  {
                                                        break _v31_5;
                                                     } else {
                                                        break _v31_6;
                                                     }
                                                default: break _v31_6;}
                                          }
                                    } else {
                                       if (_p18._4.ctor === "RBNode_elm_builtin") {
                                             switch (_p18._4._0.ctor)
                                             {case "Red": if (_p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Red") {
                                                        break _v31_2;
                                                     } else {
                                                        if (_p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Red") {
                                                              break _v31_3;
                                                           } else {
                                                              break _v31_6;
                                                           }
                                                     }
                                                case "NBlack":
                                                if (_p18._0.ctor === "BBlack" && _p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Black" && _p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Black")
                                                  {
                                                        break _v31_4;
                                                     } else {
                                                        break _v31_6;
                                                     }
                                                default: break _v31_6;}
                                          } else {
                                             break _v31_6;
                                          }
                                    }
                              } else {
                                 break _v31_6;
                              }
                        } while (false);
                        return balancedTree(_p18._0)(_p18._3._3._1)(_p18._3._3._2)(_p18._3._1)(_p18._3._2)(_p18._1)(_p18._2)(_p18._3._3._3)(_p18._3._3._4)(_p18._3._4)(_p18._4);
                     } while (false);
                     return balancedTree(_p18._0)(_p18._3._1)(_p18._3._2)(_p18._3._4._1)(_p18._3._4._2)(_p18._1)(_p18._2)(_p18._3._3)(_p18._3._4._3)(_p18._3._4._4)(_p18._4);
                  } while (false);
                  return balancedTree(_p18._0)(_p18._1)(_p18._2)(_p18._4._3._1)(_p18._4._3._2)(_p18._4._1)(_p18._4._2)(_p18._3)(_p18._4._3._3)(_p18._4._3._4)(_p18._4._4);
               } while (false);
               return balancedTree(_p18._0)(_p18._1)(_p18._2)(_p18._4._1)(_p18._4._2)(_p18._4._4._1)(_p18._4._4._2)(_p18._3)(_p18._4._3)(_p18._4._4._3)(_p18._4._4._4);
            } while (false);
            return A5(RBNode_elm_builtin,
            Black,
            _p18._4._3._1,
            _p18._4._3._2,
            A5(RBNode_elm_builtin,Black,_p18._1,_p18._2,_p18._3,_p18._4._3._3),
            A5(balance,Black,_p18._4._1,_p18._4._2,_p18._4._3._4,redden(_p18._4._4)));
         } while (false);
         return A5(RBNode_elm_builtin,
         Black,
         _p18._3._4._1,
         _p18._3._4._2,
         A5(balance,Black,_p18._3._1,_p18._3._2,redden(_p18._3._3),_p18._3._4._3),
         A5(RBNode_elm_builtin,Black,_p18._1,_p18._2,_p18._3._4._4,_p18._4));
      } while (false);
      return tree;
   };
   var balance = F5(function (c,k,v,l,r) {    var tree = A5(RBNode_elm_builtin,c,k,v,l,r);return blackish(tree) ? balanceHelp(tree) : tree;});
   var bubble = F5(function (c,k,v,l,r) {
      return isBBlack(l) || isBBlack(r) ? A5(balance,moreBlack(c),k,v,lessBlackTree(l),lessBlackTree(r)) : A5(RBNode_elm_builtin,c,k,v,l,r);
   });
   var removeMax = F5(function (c,k,v,l,r) {
      var _p19 = r;
      if (_p19.ctor === "RBEmpty_elm_builtin") {
            return A3(rem,c,l,r);
         } else {
            return A5(bubble,c,k,v,l,A5(removeMax,_p19._0,_p19._1,_p19._2,_p19._3,_p19._4));
         }
   });
   var rem = F3(function (c,l,r) {
      var _p20 = {ctor: "_Tuple2",_0: l,_1: r};
      if (_p20._0.ctor === "RBEmpty_elm_builtin") {
            if (_p20._1.ctor === "RBEmpty_elm_builtin") {
                  var _p21 = c;
                  switch (_p21.ctor)
                  {case "Red": return RBEmpty_elm_builtin(LBlack);
                     case "Black": return RBEmpty_elm_builtin(LBBlack);
                     default: return $Native$Debug.crash("cannot have bblack or nblack nodes at this point");}
               } else {
                  var _p24 = _p20._1._0;
                  var _p23 = _p20._0._0;
                  var _p22 = {ctor: "_Tuple3",_0: c,_1: _p23,_2: _p24};
                  if (_p22.ctor === "_Tuple3" && _p22._0.ctor === "Black" && _p22._1.ctor === "LBlack" && _p22._2.ctor === "Red") {
                        return A5(RBNode_elm_builtin,Black,_p20._1._1,_p20._1._2,_p20._1._3,_p20._1._4);
                     } else {
                        return A4(reportRemBug,"Black/LBlack/Red",c,$Basics.toString(_p23),$Basics.toString(_p24));
                     }
               }
         } else {
            if (_p20._1.ctor === "RBEmpty_elm_builtin") {
                  var _p27 = _p20._1._0;
                  var _p26 = _p20._0._0;
                  var _p25 = {ctor: "_Tuple3",_0: c,_1: _p26,_2: _p27};
                  if (_p25.ctor === "_Tuple3" && _p25._0.ctor === "Black" && _p25._1.ctor === "Red" && _p25._2.ctor === "LBlack") {
                        return A5(RBNode_elm_builtin,Black,_p20._0._1,_p20._0._2,_p20._0._3,_p20._0._4);
                     } else {
                        return A4(reportRemBug,"Black/Red/LBlack",c,$Basics.toString(_p26),$Basics.toString(_p27));
                     }
               } else {
                  var _p31 = _p20._0._2;
                  var _p30 = _p20._0._4;
                  var _p29 = _p20._0._1;
                  var l$ = A5(removeMax,_p20._0._0,_p29,_p31,_p20._0._3,_p30);
                  var _p28 = A3(maxWithDefault,_p29,_p31,_p30);
                  var k = _p28._0;
                  var v = _p28._1;
                  return A5(bubble,c,k,v,l$,r);
               }
         }
   });
   var update = F3(function (k,alter,dict) {
      var up = function (dict) {
         var _p32 = dict;
         if (_p32.ctor === "RBEmpty_elm_builtin") {
               var _p33 = alter($Maybe.Nothing);
               if (_p33.ctor === "Nothing") {
                     return {ctor: "_Tuple2",_0: Same,_1: empty};
                  } else {
                     return {ctor: "_Tuple2",_0: Insert,_1: A5(RBNode_elm_builtin,Red,k,_p33._0,empty,empty)};
                  }
            } else {
               var _p44 = _p32._2;
               var _p43 = _p32._4;
               var _p42 = _p32._3;
               var _p41 = _p32._1;
               var _p40 = _p32._0;
               var _p34 = A2($Basics.compare,k,_p41);
               switch (_p34.ctor)
               {case "EQ": var _p35 = alter($Maybe.Just(_p44));
                    if (_p35.ctor === "Nothing") {
                          return {ctor: "_Tuple2",_0: Remove,_1: A3(rem,_p40,_p42,_p43)};
                       } else {
                          return {ctor: "_Tuple2",_0: Same,_1: A5(RBNode_elm_builtin,_p40,_p41,_p35._0,_p42,_p43)};
                       }
                  case "LT": var _p36 = up(_p42);
                    var flag = _p36._0;
                    var newLeft = _p36._1;
                    var _p37 = flag;
                    switch (_p37.ctor)
                    {case "Same": return {ctor: "_Tuple2",_0: Same,_1: A5(RBNode_elm_builtin,_p40,_p41,_p44,newLeft,_p43)};
                       case "Insert": return {ctor: "_Tuple2",_0: Insert,_1: A5(balance,_p40,_p41,_p44,newLeft,_p43)};
                       default: return {ctor: "_Tuple2",_0: Remove,_1: A5(bubble,_p40,_p41,_p44,newLeft,_p43)};}
                  default: var _p38 = up(_p43);
                    var flag = _p38._0;
                    var newRight = _p38._1;
                    var _p39 = flag;
                    switch (_p39.ctor)
                    {case "Same": return {ctor: "_Tuple2",_0: Same,_1: A5(RBNode_elm_builtin,_p40,_p41,_p44,_p42,newRight)};
                       case "Insert": return {ctor: "_Tuple2",_0: Insert,_1: A5(balance,_p40,_p41,_p44,_p42,newRight)};
                       default: return {ctor: "_Tuple2",_0: Remove,_1: A5(bubble,_p40,_p41,_p44,_p42,newRight)};}}
            }
      };
      var _p45 = up(dict);
      var flag = _p45._0;
      var updatedDict = _p45._1;
      var _p46 = flag;
      switch (_p46.ctor)
      {case "Same": return updatedDict;
         case "Insert": return ensureBlackRoot(updatedDict);
         default: return blacken(updatedDict);}
   });
   var insert = F3(function (key,value,dict) {    return A3(update,key,$Basics.always($Maybe.Just(value)),dict);});
   var singleton = F2(function (key,value) {    return A3(insert,key,value,empty);});
   var union = F2(function (t1,t2) {    return A3(foldl,insert,t2,t1);});
   var fromList = function (assocs) {
      return A3($List.foldl,F2(function (_p47,dict) {    var _p48 = _p47;return A3(insert,_p48._0,_p48._1,dict);}),empty,assocs);
   };
   var filter = F2(function (predicate,dictionary) {
      var add = F3(function (key,value,dict) {    return A2(predicate,key,value) ? A3(insert,key,value,dict) : dict;});
      return A3(foldl,add,empty,dictionary);
   });
   var intersect = F2(function (t1,t2) {    return A2(filter,F2(function (k,_p49) {    return A2(member,k,t2);}),t1);});
   var partition = F2(function (predicate,dict) {
      var add = F3(function (key,value,_p50) {
         var _p51 = _p50;
         var _p53 = _p51._1;
         var _p52 = _p51._0;
         return A2(predicate,key,value) ? {ctor: "_Tuple2",_0: A3(insert,key,value,_p52),_1: _p53} : {ctor: "_Tuple2",_0: _p52,_1: A3(insert,key,value,_p53)};
      });
      return A3(foldl,add,{ctor: "_Tuple2",_0: empty,_1: empty},dict);
   });
   var remove = F2(function (key,dict) {    return A3(update,key,$Basics.always($Maybe.Nothing),dict);});
   var diff = F2(function (t1,t2) {    return A3(foldl,F3(function (k,v,t) {    return A2(remove,k,t);}),t1,t2);});
   return _elm.Dict.values = {_op: _op
                             ,empty: empty
                             ,singleton: singleton
                             ,insert: insert
                             ,update: update
                             ,isEmpty: isEmpty
                             ,get: get
                             ,remove: remove
                             ,member: member
                             ,size: size
                             ,filter: filter
                             ,partition: partition
                             ,foldl: foldl
                             ,foldr: foldr
                             ,map: map
                             ,union: union
                             ,intersect: intersect
                             ,diff: diff
                             ,keys: keys
                             ,values: values
                             ,toList: toList
                             ,fromList: fromList};
};
Elm.Native.Json = {};

Elm.Native.Json.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Json = localRuntime.Native.Json || {};
	if (localRuntime.Native.Json.values) {
		return localRuntime.Native.Json.values;
	}

	var ElmArray = Elm.Native.Array.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);
	var Result = Elm.Result.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);


	function crash(expected, actual) {
		throw new Error(
			'expecting ' + expected + ' but got ' + JSON.stringify(actual)
		);
	}


	// PRIMITIVE VALUES

	function decodeNull(successValue) {
		return function(value) {
			if (value === null) {
				return successValue;
			}
			crash('null', value);
		};
	}


	function decodeString(value) {
		if (typeof value === 'string' || value instanceof String) {
			return value;
		}
		crash('a String', value);
	}


	function decodeFloat(value) {
		if (typeof value === 'number') {
			return value;
		}
		crash('a Float', value);
	}


	function decodeInt(value) {
		if (typeof value !== 'number') {
			crash('an Int', value);
		}

		if (value < 2147483647 && value > -2147483647 && (value | 0) === value) {
			return value;
		}

		if (isFinite(value) && !(value % 1)) {
			return value;
		}

		crash('an Int', value);
	}


	function decodeBool(value) {
		if (typeof value === 'boolean') {
			return value;
		}
		crash('a Bool', value);
	}


	// ARRAY

	function decodeArray(decoder) {
		return function(value) {
			if (value instanceof Array) {
				var len = value.length;
				var array = new Array(len);
				for (var i = len; i--; ) {
					array[i] = decoder(value[i]);
				}
				return ElmArray.fromJSArray(array);
			}
			crash('an Array', value);
		};
	}


	// LIST

	function decodeList(decoder) {
		return function(value) {
			if (value instanceof Array) {
				var len = value.length;
				var list = List.Nil;
				for (var i = len; i--; ) {
					list = List.Cons( decoder(value[i]), list );
				}
				return list;
			}
			crash('a List', value);
		};
	}


	// MAYBE

	function decodeMaybe(decoder) {
		return function(value) {
			try {
				return Maybe.Just(decoder(value));
			} catch(e) {
				return Maybe.Nothing;
			}
		};
	}


	// FIELDS

	function decodeField(field, decoder) {
		return function(value) {
			var subValue = value[field];
			if (subValue !== undefined) {
				return decoder(subValue);
			}
			crash("an object with field '" + field + "'", value);
		};
	}


	// OBJECTS

	function decodeKeyValuePairs(decoder) {
		return function(value) {
			var isObject =
				typeof value === 'object'
					&& value !== null
					&& !(value instanceof Array);

			if (isObject) {
				var keyValuePairs = List.Nil;
				for (var key in value)
				{
					var elmValue = decoder(value[key]);
					var pair = Utils.Tuple2(key, elmValue);
					keyValuePairs = List.Cons(pair, keyValuePairs);
				}
				return keyValuePairs;
			}

			crash('an object', value);
		};
	}

	function decodeObject1(f, d1) {
		return function(value) {
			return f(d1(value));
		};
	}

	function decodeObject2(f, d1, d2) {
		return function(value) {
			return A2( f, d1(value), d2(value) );
		};
	}

	function decodeObject3(f, d1, d2, d3) {
		return function(value) {
			return A3( f, d1(value), d2(value), d3(value) );
		};
	}

	function decodeObject4(f, d1, d2, d3, d4) {
		return function(value) {
			return A4( f, d1(value), d2(value), d3(value), d4(value) );
		};
	}

	function decodeObject5(f, d1, d2, d3, d4, d5) {
		return function(value) {
			return A5( f, d1(value), d2(value), d3(value), d4(value), d5(value) );
		};
	}

	function decodeObject6(f, d1, d2, d3, d4, d5, d6) {
		return function(value) {
			return A6( f,
				d1(value),
				d2(value),
				d3(value),
				d4(value),
				d5(value),
				d6(value)
			);
		};
	}

	function decodeObject7(f, d1, d2, d3, d4, d5, d6, d7) {
		return function(value) {
			return A7( f,
				d1(value),
				d2(value),
				d3(value),
				d4(value),
				d5(value),
				d6(value),
				d7(value)
			);
		};
	}

	function decodeObject8(f, d1, d2, d3, d4, d5, d6, d7, d8) {
		return function(value) {
			return A8( f,
				d1(value),
				d2(value),
				d3(value),
				d4(value),
				d5(value),
				d6(value),
				d7(value),
				d8(value)
			);
		};
	}


	// TUPLES

	function decodeTuple1(f, d1) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 1 ) {
				crash('a Tuple of length 1', value);
			}
			return f( d1(value[0]) );
		};
	}

	function decodeTuple2(f, d1, d2) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 2 ) {
				crash('a Tuple of length 2', value);
			}
			return A2( f, d1(value[0]), d2(value[1]) );
		};
	}

	function decodeTuple3(f, d1, d2, d3) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 3 ) {
				crash('a Tuple of length 3', value);
			}
			return A3( f, d1(value[0]), d2(value[1]), d3(value[2]) );
		};
	}


	function decodeTuple4(f, d1, d2, d3, d4) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 4 ) {
				crash('a Tuple of length 4', value);
			}
			return A4( f, d1(value[0]), d2(value[1]), d3(value[2]), d4(value[3]) );
		};
	}


	function decodeTuple5(f, d1, d2, d3, d4, d5) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 5 ) {
				crash('a Tuple of length 5', value);
			}
			return A5( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4])
			);
		};
	}


	function decodeTuple6(f, d1, d2, d3, d4, d5, d6) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 6 ) {
				crash('a Tuple of length 6', value);
			}
			return A6( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4]),
				d6(value[5])
			);
		};
	}

	function decodeTuple7(f, d1, d2, d3, d4, d5, d6, d7) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 7 ) {
				crash('a Tuple of length 7', value);
			}
			return A7( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4]),
				d6(value[5]),
				d7(value[6])
			);
		};
	}


	function decodeTuple8(f, d1, d2, d3, d4, d5, d6, d7, d8) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 8 ) {
				crash('a Tuple of length 8', value);
			}
			return A8( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4]),
				d6(value[5]),
				d7(value[6]),
				d8(value[7])
			);
		};
	}


	// CUSTOM DECODERS

	function decodeValue(value) {
		return value;
	}

	function runDecoderValue(decoder, value) {
		try {
			return Result.Ok(decoder(value));
		} catch(e) {
			return Result.Err(e.message);
		}
	}

	function customDecoder(decoder, callback) {
		return function(value) {
			var result = callback(decoder(value));
			if (result.ctor === 'Err') {
				throw new Error('custom decoder failed: ' + result._0);
			}
			return result._0;
		};
	}

	function andThen(decode, callback) {
		return function(value) {
			var result = decode(value);
			return callback(result)(value);
		};
	}

	function fail(msg) {
		return function(value) {
			throw new Error(msg);
		};
	}

	function succeed(successValue) {
		return function(value) {
			return successValue;
		};
	}


	// ONE OF MANY

	function oneOf(decoders) {
		return function(value) {
			var errors = [];
			var temp = decoders;
			while (temp.ctor !== '[]') {
				try {
					return temp._0(value);
				} catch(e) {
					errors.push(e.message);
				}
				temp = temp._1;
			}
			throw new Error('expecting one of the following:\n    ' + errors.join('\n    '));
		};
	}

	function get(decoder, value) {
		try {
			return Result.Ok(decoder(value));
		} catch(e) {
			return Result.Err(e.message);
		}
	}


	// ENCODE / DECODE

	function runDecoderString(decoder, string) {
		try {
			return Result.Ok(decoder(JSON.parse(string)));
		} catch(e) {
			return Result.Err(e.message);
		}
	}

	function encode(indentLevel, value) {
		return JSON.stringify(value, null, indentLevel);
	}

	function identity(value) {
		return value;
	}

	function encodeObject(keyValuePairs) {
		var obj = {};
		while (keyValuePairs.ctor !== '[]') {
			var pair = keyValuePairs._0;
			obj[pair._0] = pair._1;
			keyValuePairs = keyValuePairs._1;
		}
		return obj;
	}

	return localRuntime.Native.Json.values = {
		encode: F2(encode),
		runDecoderString: F2(runDecoderString),
		runDecoderValue: F2(runDecoderValue),

		get: F2(get),
		oneOf: oneOf,

		decodeNull: decodeNull,
		decodeInt: decodeInt,
		decodeFloat: decodeFloat,
		decodeString: decodeString,
		decodeBool: decodeBool,

		decodeMaybe: decodeMaybe,

		decodeList: decodeList,
		decodeArray: decodeArray,

		decodeField: F2(decodeField),

		decodeObject1: F2(decodeObject1),
		decodeObject2: F3(decodeObject2),
		decodeObject3: F4(decodeObject3),
		decodeObject4: F5(decodeObject4),
		decodeObject5: F6(decodeObject5),
		decodeObject6: F7(decodeObject6),
		decodeObject7: F8(decodeObject7),
		decodeObject8: F9(decodeObject8),
		decodeKeyValuePairs: decodeKeyValuePairs,

		decodeTuple1: F2(decodeTuple1),
		decodeTuple2: F3(decodeTuple2),
		decodeTuple3: F4(decodeTuple3),
		decodeTuple4: F5(decodeTuple4),
		decodeTuple5: F6(decodeTuple5),
		decodeTuple6: F7(decodeTuple6),
		decodeTuple7: F8(decodeTuple7),
		decodeTuple8: F9(decodeTuple8),

		andThen: F2(andThen),
		decodeValue: decodeValue,
		customDecoder: F2(customDecoder),
		fail: fail,
		succeed: succeed,

		identity: identity,
		encodeNull: null,
		encodeArray: ElmArray.toJSArray,
		encodeList: List.toArray,
		encodeObject: encodeObject

	};
};

Elm.Json = Elm.Json || {};
Elm.Json.Encode = Elm.Json.Encode || {};
Elm.Json.Encode.make = function (_elm) {
   "use strict";
   _elm.Json = _elm.Json || {};
   _elm.Json.Encode = _elm.Json.Encode || {};
   if (_elm.Json.Encode.values) return _elm.Json.Encode.values;
   var _U = Elm.Native.Utils.make(_elm),$Array = Elm.Array.make(_elm),$Native$Json = Elm.Native.Json.make(_elm);
   var _op = {};
   var list = $Native$Json.encodeList;
   var array = $Native$Json.encodeArray;
   var object = $Native$Json.encodeObject;
   var $null = $Native$Json.encodeNull;
   var bool = $Native$Json.identity;
   var $float = $Native$Json.identity;
   var $int = $Native$Json.identity;
   var string = $Native$Json.identity;
   var encode = $Native$Json.encode;
   var Value = {ctor: "Value"};
   return _elm.Json.Encode.values = {_op: _op
                                    ,encode: encode
                                    ,string: string
                                    ,$int: $int
                                    ,$float: $float
                                    ,bool: bool
                                    ,$null: $null
                                    ,list: list
                                    ,array: array
                                    ,object: object};
};
Elm.Json = Elm.Json || {};
Elm.Json.Decode = Elm.Json.Decode || {};
Elm.Json.Decode.make = function (_elm) {
   "use strict";
   _elm.Json = _elm.Json || {};
   _elm.Json.Decode = _elm.Json.Decode || {};
   if (_elm.Json.Decode.values) return _elm.Json.Decode.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Array = Elm.Array.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Json$Encode = Elm.Json.Encode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Json = Elm.Native.Json.make(_elm),
   $Result = Elm.Result.make(_elm);
   var _op = {};
   var tuple8 = $Native$Json.decodeTuple8;
   var tuple7 = $Native$Json.decodeTuple7;
   var tuple6 = $Native$Json.decodeTuple6;
   var tuple5 = $Native$Json.decodeTuple5;
   var tuple4 = $Native$Json.decodeTuple4;
   var tuple3 = $Native$Json.decodeTuple3;
   var tuple2 = $Native$Json.decodeTuple2;
   var tuple1 = $Native$Json.decodeTuple1;
   var succeed = $Native$Json.succeed;
   var fail = $Native$Json.fail;
   var andThen = $Native$Json.andThen;
   var customDecoder = $Native$Json.customDecoder;
   var decodeValue = $Native$Json.runDecoderValue;
   var value = $Native$Json.decodeValue;
   var maybe = $Native$Json.decodeMaybe;
   var $null = $Native$Json.decodeNull;
   var array = $Native$Json.decodeArray;
   var list = $Native$Json.decodeList;
   var bool = $Native$Json.decodeBool;
   var $int = $Native$Json.decodeInt;
   var $float = $Native$Json.decodeFloat;
   var string = $Native$Json.decodeString;
   var oneOf = $Native$Json.oneOf;
   var keyValuePairs = $Native$Json.decodeKeyValuePairs;
   var object8 = $Native$Json.decodeObject8;
   var object7 = $Native$Json.decodeObject7;
   var object6 = $Native$Json.decodeObject6;
   var object5 = $Native$Json.decodeObject5;
   var object4 = $Native$Json.decodeObject4;
   var object3 = $Native$Json.decodeObject3;
   var object2 = $Native$Json.decodeObject2;
   var object1 = $Native$Json.decodeObject1;
   _op[":="] = $Native$Json.decodeField;
   var at = F2(function (fields,decoder) {    return A3($List.foldr,F2(function (x,y) {    return A2(_op[":="],x,y);}),decoder,fields);});
   var decodeString = $Native$Json.runDecoderString;
   var map = $Native$Json.decodeObject1;
   var dict = function (decoder) {    return A2(map,$Dict.fromList,keyValuePairs(decoder));};
   var Decoder = {ctor: "Decoder"};
   return _elm.Json.Decode.values = {_op: _op
                                    ,decodeString: decodeString
                                    ,decodeValue: decodeValue
                                    ,string: string
                                    ,$int: $int
                                    ,$float: $float
                                    ,bool: bool
                                    ,$null: $null
                                    ,list: list
                                    ,array: array
                                    ,tuple1: tuple1
                                    ,tuple2: tuple2
                                    ,tuple3: tuple3
                                    ,tuple4: tuple4
                                    ,tuple5: tuple5
                                    ,tuple6: tuple6
                                    ,tuple7: tuple7
                                    ,tuple8: tuple8
                                    ,at: at
                                    ,object1: object1
                                    ,object2: object2
                                    ,object3: object3
                                    ,object4: object4
                                    ,object5: object5
                                    ,object6: object6
                                    ,object7: object7
                                    ,object8: object8
                                    ,keyValuePairs: keyValuePairs
                                    ,dict: dict
                                    ,maybe: maybe
                                    ,oneOf: oneOf
                                    ,map: map
                                    ,fail: fail
                                    ,succeed: succeed
                                    ,andThen: andThen
                                    ,value: value
                                    ,customDecoder: customDecoder};
};
Elm.Set = Elm.Set || {};
Elm.Set.make = function (_elm) {
   "use strict";
   _elm.Set = _elm.Set || {};
   if (_elm.Set.values) return _elm.Set.values;
   var _U = Elm.Native.Utils.make(_elm),$Basics = Elm.Basics.make(_elm),$Dict = Elm.Dict.make(_elm),$List = Elm.List.make(_elm);
   var _op = {};
   var foldr = F3(function (f,b,_p0) {    var _p1 = _p0;return A3($Dict.foldr,F3(function (k,_p2,b) {    return A2(f,k,b);}),b,_p1._0);});
   var foldl = F3(function (f,b,_p3) {    var _p4 = _p3;return A3($Dict.foldl,F3(function (k,_p5,b) {    return A2(f,k,b);}),b,_p4._0);});
   var toList = function (_p6) {    var _p7 = _p6;return $Dict.keys(_p7._0);};
   var size = function (_p8) {    var _p9 = _p8;return $Dict.size(_p9._0);};
   var member = F2(function (k,_p10) {    var _p11 = _p10;return A2($Dict.member,k,_p11._0);});
   var isEmpty = function (_p12) {    var _p13 = _p12;return $Dict.isEmpty(_p13._0);};
   var Set_elm_builtin = function (a) {    return {ctor: "Set_elm_builtin",_0: a};};
   var empty = Set_elm_builtin($Dict.empty);
   var singleton = function (k) {    return Set_elm_builtin(A2($Dict.singleton,k,{ctor: "_Tuple0"}));};
   var insert = F2(function (k,_p14) {    var _p15 = _p14;return Set_elm_builtin(A3($Dict.insert,k,{ctor: "_Tuple0"},_p15._0));});
   var fromList = function (xs) {    return A3($List.foldl,insert,empty,xs);};
   var map = F2(function (f,s) {    return fromList(A2($List.map,f,toList(s)));});
   var remove = F2(function (k,_p16) {    var _p17 = _p16;return Set_elm_builtin(A2($Dict.remove,k,_p17._0));});
   var union = F2(function (_p19,_p18) {    var _p20 = _p19;var _p21 = _p18;return Set_elm_builtin(A2($Dict.union,_p20._0,_p21._0));});
   var intersect = F2(function (_p23,_p22) {    var _p24 = _p23;var _p25 = _p22;return Set_elm_builtin(A2($Dict.intersect,_p24._0,_p25._0));});
   var diff = F2(function (_p27,_p26) {    var _p28 = _p27;var _p29 = _p26;return Set_elm_builtin(A2($Dict.diff,_p28._0,_p29._0));});
   var filter = F2(function (p,_p30) {    var _p31 = _p30;return Set_elm_builtin(A2($Dict.filter,F2(function (k,_p32) {    return p(k);}),_p31._0));});
   var partition = F2(function (p,_p33) {
      var _p34 = _p33;
      var _p35 = A2($Dict.partition,F2(function (k,_p36) {    return p(k);}),_p34._0);
      var p1 = _p35._0;
      var p2 = _p35._1;
      return {ctor: "_Tuple2",_0: Set_elm_builtin(p1),_1: Set_elm_builtin(p2)};
   });
   return _elm.Set.values = {_op: _op
                            ,empty: empty
                            ,singleton: singleton
                            ,insert: insert
                            ,remove: remove
                            ,isEmpty: isEmpty
                            ,member: member
                            ,size: size
                            ,foldl: foldl
                            ,foldr: foldr
                            ,map: map
                            ,filter: filter
                            ,partition: partition
                            ,union: union
                            ,intersect: intersect
                            ,diff: diff
                            ,toList: toList
                            ,fromList: fromList};
};
Elm.Native.Regex = {};
Elm.Native.Regex.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Regex = localRuntime.Native.Regex || {};
	if (localRuntime.Native.Regex.values)
	{
		return localRuntime.Native.Regex.values;
	}
	if ('values' in Elm.Native.Regex)
	{
		return localRuntime.Native.Regex.values = Elm.Native.Regex.values;
	}

	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);

	function escape(str)
	{
		return str.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
	}
	function caseInsensitive(re)
	{
		return new RegExp(re.source, 'gi');
	}
	function regex(raw)
	{
		return new RegExp(raw, 'g');
	}

	function contains(re, string)
	{
		return string.match(re) !== null;
	}

	function find(n, re, str)
	{
		n = n.ctor === 'All' ? Infinity : n._0;
		var out = [];
		var number = 0;
		var string = str;
		var lastIndex = re.lastIndex;
		var prevLastIndex = -1;
		var result;
		while (number++ < n && (result = re.exec(string)))
		{
			if (prevLastIndex === re.lastIndex) break;
			var i = result.length - 1;
			var subs = new Array(i);
			while (i > 0)
			{
				var submatch = result[i];
				subs[--i] = submatch === undefined
					? Maybe.Nothing
					: Maybe.Just(submatch);
			}
			out.push({
				match: result[0],
				submatches: List.fromArray(subs),
				index: result.index,
				number: number
			});
			prevLastIndex = re.lastIndex;
		}
		re.lastIndex = lastIndex;
		return List.fromArray(out);
	}

	function replace(n, re, replacer, string)
	{
		n = n.ctor === 'All' ? Infinity : n._0;
		var count = 0;
		function jsReplacer(match)
		{
			if (count++ >= n)
			{
				return match;
			}
			var i = arguments.length - 3;
			var submatches = new Array(i);
			while (i > 0)
			{
				var submatch = arguments[i];
				submatches[--i] = submatch === undefined
					? Maybe.Nothing
					: Maybe.Just(submatch);
			}
			return replacer({
				match: match,
				submatches: List.fromArray(submatches),
				index: arguments[i - 1],
				number: count
			});
		}
		return string.replace(re, jsReplacer);
	}

	function split(n, re, str)
	{
		n = n.ctor === 'All' ? Infinity : n._0;
		if (n === Infinity)
		{
			return List.fromArray(str.split(re));
		}
		var string = str;
		var result;
		var out = [];
		var start = re.lastIndex;
		while (n--)
		{
			if (!(result = re.exec(string))) break;
			out.push(string.slice(start, result.index));
			start = re.lastIndex;
		}
		out.push(string.slice(start));
		return List.fromArray(out);
	}

	return Elm.Native.Regex.values = {
		regex: regex,
		caseInsensitive: caseInsensitive,
		escape: escape,

		contains: F2(contains),
		find: F3(find),
		replace: F4(replace),
		split: F3(split)
	};
};

Elm.Regex = Elm.Regex || {};
Elm.Regex.make = function (_elm) {
   "use strict";
   _elm.Regex = _elm.Regex || {};
   if (_elm.Regex.values) return _elm.Regex.values;
   var _U = Elm.Native.Utils.make(_elm),$Maybe = Elm.Maybe.make(_elm),$Native$Regex = Elm.Native.Regex.make(_elm);
   var _op = {};
   var split = $Native$Regex.split;
   var replace = $Native$Regex.replace;
   var find = $Native$Regex.find;
   var AtMost = function (a) {    return {ctor: "AtMost",_0: a};};
   var All = {ctor: "All"};
   var Match = F4(function (a,b,c,d) {    return {match: a,submatches: b,index: c,number: d};});
   var contains = $Native$Regex.contains;
   var caseInsensitive = $Native$Regex.caseInsensitive;
   var regex = $Native$Regex.regex;
   var escape = $Native$Regex.escape;
   var Regex = {ctor: "Regex"};
   return _elm.Regex.values = {_op: _op
                              ,regex: regex
                              ,escape: escape
                              ,caseInsensitive: caseInsensitive
                              ,contains: contains
                              ,find: find
                              ,replace: replace
                              ,split: split
                              ,Match: Match
                              ,All: All
                              ,AtMost: AtMost};
};
Elm.Native.Effects = {};
Elm.Native.Effects.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Effects = localRuntime.Native.Effects || {};
	if (localRuntime.Native.Effects.values)
	{
		return localRuntime.Native.Effects.values;
	}

	var Task = Elm.Native.Task.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);
	var Signal = Elm.Signal.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);


	// polyfill so things will work even if rAF is not available for some reason
	var _requestAnimationFrame =
		typeof requestAnimationFrame !== 'undefined'
			? requestAnimationFrame
			: function(cb) { setTimeout(cb, 1000 / 60); }
			;


	// batchedSending and sendCallback implement a small state machine in order
	// to schedule only one send(time) call per animation frame.
	//
	// Invariants:
	// 1. In the NO_REQUEST state, there is never a scheduled sendCallback.
	// 2. In the PENDING_REQUEST and EXTRA_REQUEST states, there is always exactly
	//    one scheduled sendCallback.
	var NO_REQUEST = 0;
	var PENDING_REQUEST = 1;
	var EXTRA_REQUEST = 2;
	var state = NO_REQUEST;
	var messageArray = [];


	function batchedSending(address, tickMessages)
	{
		// insert ticks into the messageArray
		var foundAddress = false;

		for (var i = messageArray.length; i--; )
		{
			if (messageArray[i].address === address)
			{
				foundAddress = true;
				messageArray[i].tickMessages = A3(List.foldl, List.cons, messageArray[i].tickMessages, tickMessages);
				break;
			}
		}

		if (!foundAddress)
		{
			messageArray.push({ address: address, tickMessages: tickMessages });
		}

		// do the appropriate state transition
		switch (state)
		{
			case NO_REQUEST:
				_requestAnimationFrame(sendCallback);
				state = PENDING_REQUEST;
				break;
			case PENDING_REQUEST:
				state = PENDING_REQUEST;
				break;
			case EXTRA_REQUEST:
				state = PENDING_REQUEST;
				break;
		}
	}


	function sendCallback(time)
	{
		switch (state)
		{
			case NO_REQUEST:
				// This state should not be possible. How can there be no
				// request, yet somehow we are actively fulfilling a
				// request?
				throw new Error(
					'Unexpected send callback.\n' +
					'Please report this to <https://github.com/evancz/elm-effects/issues>.'
				);

			case PENDING_REQUEST:
				// At this point, we do not *know* that another frame is
				// needed, but we make an extra request to rAF just in
				// case. It's possible to drop a frame if rAF is called
				// too late, so we just do it preemptively.
				_requestAnimationFrame(sendCallback);
				state = EXTRA_REQUEST;

				// There's also stuff we definitely need to send.
				send(time);
				return;

			case EXTRA_REQUEST:
				// Turns out the extra request was not needed, so we will
				// stop calling rAF. No reason to call it all the time if
				// no one needs it.
				state = NO_REQUEST;
				return;
		}
	}


	function send(time)
	{
		for (var i = messageArray.length; i--; )
		{
			var messages = A3(
				List.foldl,
				F2( function(toAction, list) { return List.Cons(toAction(time), list); } ),
				List.Nil,
				messageArray[i].tickMessages
			);
			Task.perform( A2(Signal.send, messageArray[i].address, messages) );
		}
		messageArray = [];
	}


	function requestTickSending(address, tickMessages)
	{
		return Task.asyncFunction(function(callback) {
			batchedSending(address, tickMessages);
			callback(Task.succeed(Utils.Tuple0));
		});
	}


	return localRuntime.Native.Effects.values = {
		requestTickSending: F2(requestTickSending)
	};

};

Elm.Effects = Elm.Effects || {};
Elm.Effects.make = function (_elm) {
   "use strict";
   _elm.Effects = _elm.Effects || {};
   if (_elm.Effects.values) return _elm.Effects.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Effects = Elm.Native.Effects.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Task = Elm.Task.make(_elm),
   $Time = Elm.Time.make(_elm);
   var _op = {};
   var ignore = function (task) {    return A2($Task.map,$Basics.always({ctor: "_Tuple0"}),task);};
   var requestTickSending = $Native$Effects.requestTickSending;
   var toTaskHelp = F3(function (address,effect,_p0) {
      var _p1 = _p0;
      var _p5 = _p1._1;
      var _p4 = _p1;
      var _p3 = _p1._0;
      var _p2 = effect;
      switch (_p2.ctor)
      {case "Task": var reporter = A2($Task.andThen,_p2._0,function (answer) {    return A2($Signal.send,address,_U.list([answer]));});
           return {ctor: "_Tuple2",_0: A2($Task.andThen,_p3,$Basics.always(ignore($Task.spawn(reporter)))),_1: _p5};
         case "Tick": return {ctor: "_Tuple2",_0: _p3,_1: A2($List._op["::"],_p2._0,_p5)};
         case "None": return _p4;
         default: return A3($List.foldl,toTaskHelp(address),_p4,_p2._0);}
   });
   var toTask = F2(function (address,effect) {
      var _p6 = A3(toTaskHelp,address,effect,{ctor: "_Tuple2",_0: $Task.succeed({ctor: "_Tuple0"}),_1: _U.list([])});
      var combinedTask = _p6._0;
      var tickMessages = _p6._1;
      return $List.isEmpty(tickMessages) ? combinedTask : A2($Task.andThen,combinedTask,$Basics.always(A2(requestTickSending,address,tickMessages)));
   });
   var Never = function (a) {    return {ctor: "Never",_0: a};};
   var Batch = function (a) {    return {ctor: "Batch",_0: a};};
   var batch = Batch;
   var None = {ctor: "None"};
   var none = None;
   var Tick = function (a) {    return {ctor: "Tick",_0: a};};
   var tick = Tick;
   var Task = function (a) {    return {ctor: "Task",_0: a};};
   var task = Task;
   var map = F2(function (func,effect) {
      var _p7 = effect;
      switch (_p7.ctor)
      {case "Task": return Task(A2($Task.map,func,_p7._0));
         case "Tick": return Tick(function (_p8) {    return func(_p7._0(_p8));});
         case "None": return None;
         default: return Batch(A2($List.map,map(func),_p7._0));}
   });
   return _elm.Effects.values = {_op: _op,none: none,task: task,tick: tick,map: map,batch: batch,toTask: toTask};
};
(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){

},{}],2:[function(require,module,exports){
(function (global){
var topLevel = typeof global !== 'undefined' ? global :
    typeof window !== 'undefined' ? window : {}
var minDoc = require('min-document');

if (typeof document !== 'undefined') {
    module.exports = document;
} else {
    var doccy = topLevel['__GLOBAL_DOCUMENT_CACHE@4'];

    if (!doccy) {
        doccy = topLevel['__GLOBAL_DOCUMENT_CACHE@4'] = minDoc;
    }

    module.exports = doccy;
}

}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"min-document":1}],3:[function(require,module,exports){
"use strict";

module.exports = function isObject(x) {
	return typeof x === "object" && x !== null;
};

},{}],4:[function(require,module,exports){
var nativeIsArray = Array.isArray
var toString = Object.prototype.toString

module.exports = nativeIsArray || isArray

function isArray(obj) {
    return toString.call(obj) === "[object Array]"
}

},{}],5:[function(require,module,exports){
var isObject = require("is-object")
var isHook = require("../vnode/is-vhook.js")

module.exports = applyProperties

function applyProperties(node, props, previous) {
    for (var propName in props) {
        var propValue = props[propName]

        if (propValue === undefined) {
            removeProperty(node, propName, propValue, previous);
        } else if (isHook(propValue)) {
            removeProperty(node, propName, propValue, previous)
            if (propValue.hook) {
                propValue.hook(node,
                    propName,
                    previous ? previous[propName] : undefined)
            }
        } else {
            if (isObject(propValue)) {
                patchObject(node, props, previous, propName, propValue);
            } else {
                node[propName] = propValue
            }
        }
    }
}

function removeProperty(node, propName, propValue, previous) {
    if (previous) {
        var previousValue = previous[propName]

        if (!isHook(previousValue)) {
            if (propName === "attributes") {
                for (var attrName in previousValue) {
                    node.removeAttribute(attrName)
                }
            } else if (propName === "style") {
                for (var i in previousValue) {
                    node.style[i] = ""
                }
            } else if (typeof previousValue === "string") {
                node[propName] = ""
            } else {
                node[propName] = null
            }
        } else if (previousValue.unhook) {
            previousValue.unhook(node, propName, propValue)
        }
    }
}

function patchObject(node, props, previous, propName, propValue) {
    var previousValue = previous ? previous[propName] : undefined

    // Set attributes
    if (propName === "attributes") {
        for (var attrName in propValue) {
            var attrValue = propValue[attrName]

            if (attrValue === undefined) {
                node.removeAttribute(attrName)
            } else {
                node.setAttribute(attrName, attrValue)
            }
        }

        return
    }

    if(previousValue && isObject(previousValue) &&
        getPrototype(previousValue) !== getPrototype(propValue)) {
        node[propName] = propValue
        return
    }

    if (!isObject(node[propName])) {
        node[propName] = {}
    }

    var replacer = propName === "style" ? "" : undefined

    for (var k in propValue) {
        var value = propValue[k]
        node[propName][k] = (value === undefined) ? replacer : value
    }
}

function getPrototype(value) {
    if (Object.getPrototypeOf) {
        return Object.getPrototypeOf(value)
    } else if (value.__proto__) {
        return value.__proto__
    } else if (value.constructor) {
        return value.constructor.prototype
    }
}

},{"../vnode/is-vhook.js":13,"is-object":3}],6:[function(require,module,exports){
var document = require("global/document")

var applyProperties = require("./apply-properties")

var isVNode = require("../vnode/is-vnode.js")
var isVText = require("../vnode/is-vtext.js")
var isWidget = require("../vnode/is-widget.js")
var handleThunk = require("../vnode/handle-thunk.js")

module.exports = createElement

function createElement(vnode, opts) {
    var doc = opts ? opts.document || document : document
    var warn = opts ? opts.warn : null

    vnode = handleThunk(vnode).a

    if (isWidget(vnode)) {
        return vnode.init()
    } else if (isVText(vnode)) {
        return doc.createTextNode(vnode.text)
    } else if (!isVNode(vnode)) {
        if (warn) {
            warn("Item is not a valid virtual dom node", vnode)
        }
        return null
    }

    var node = (vnode.namespace === null) ?
        doc.createElement(vnode.tagName) :
        doc.createElementNS(vnode.namespace, vnode.tagName)

    var props = vnode.properties
    applyProperties(node, props)

    var children = vnode.children

    for (var i = 0; i < children.length; i++) {
        var childNode = createElement(children[i], opts)
        if (childNode) {
            node.appendChild(childNode)
        }
    }

    return node
}

},{"../vnode/handle-thunk.js":11,"../vnode/is-vnode.js":14,"../vnode/is-vtext.js":15,"../vnode/is-widget.js":16,"./apply-properties":5,"global/document":2}],7:[function(require,module,exports){
// Maps a virtual DOM tree onto a real DOM tree in an efficient manner.
// We don't want to read all of the DOM nodes in the tree so we use
// the in-order tree indexing to eliminate recursion down certain branches.
// We only recurse into a DOM node if we know that it contains a child of
// interest.

var noChild = {}

module.exports = domIndex

function domIndex(rootNode, tree, indices, nodes) {
    if (!indices || indices.length === 0) {
        return {}
    } else {
        indices.sort(ascending)
        return recurse(rootNode, tree, indices, nodes, 0)
    }
}

function recurse(rootNode, tree, indices, nodes, rootIndex) {
    nodes = nodes || {}


    if (rootNode) {
        if (indexInRange(indices, rootIndex, rootIndex)) {
            nodes[rootIndex] = rootNode
        }

        var vChildren = tree.children

        if (vChildren) {

            var childNodes = rootNode.childNodes

            for (var i = 0; i < tree.children.length; i++) {
                rootIndex += 1

                var vChild = vChildren[i] || noChild
                var nextIndex = rootIndex + (vChild.count || 0)

                // skip recursion down the tree if there are no nodes down here
                if (indexInRange(indices, rootIndex, nextIndex)) {
                    recurse(childNodes[i], vChild, indices, nodes, rootIndex)
                }

                rootIndex = nextIndex
            }
        }
    }

    return nodes
}

// Binary search for an index in the interval [left, right]
function indexInRange(indices, left, right) {
    if (indices.length === 0) {
        return false
    }

    var minIndex = 0
    var maxIndex = indices.length - 1
    var currentIndex
    var currentItem

    while (minIndex <= maxIndex) {
        currentIndex = ((maxIndex + minIndex) / 2) >> 0
        currentItem = indices[currentIndex]

        if (minIndex === maxIndex) {
            return currentItem >= left && currentItem <= right
        } else if (currentItem < left) {
            minIndex = currentIndex + 1
        } else  if (currentItem > right) {
            maxIndex = currentIndex - 1
        } else {
            return true
        }
    }

    return false;
}

function ascending(a, b) {
    return a > b ? 1 : -1
}

},{}],8:[function(require,module,exports){
var applyProperties = require("./apply-properties")

var isWidget = require("../vnode/is-widget.js")
var VPatch = require("../vnode/vpatch.js")

var render = require("./create-element")
var updateWidget = require("./update-widget")

module.exports = applyPatch

function applyPatch(vpatch, domNode, renderOptions) {
    var type = vpatch.type
    var vNode = vpatch.vNode
    var patch = vpatch.patch

    switch (type) {
        case VPatch.REMOVE:
            return removeNode(domNode, vNode)
        case VPatch.INSERT:
            return insertNode(domNode, patch, renderOptions)
        case VPatch.VTEXT:
            return stringPatch(domNode, vNode, patch, renderOptions)
        case VPatch.WIDGET:
            return widgetPatch(domNode, vNode, patch, renderOptions)
        case VPatch.VNODE:
            return vNodePatch(domNode, vNode, patch, renderOptions)
        case VPatch.ORDER:
            reorderChildren(domNode, patch)
            return domNode
        case VPatch.PROPS:
            applyProperties(domNode, patch, vNode.properties)
            return domNode
        case VPatch.THUNK:
            return replaceRoot(domNode,
                renderOptions.patch(domNode, patch, renderOptions))
        default:
            return domNode
    }
}

function removeNode(domNode, vNode) {
    var parentNode = domNode.parentNode

    if (parentNode) {
        parentNode.removeChild(domNode)
    }

    destroyWidget(domNode, vNode);

    return null
}

function insertNode(parentNode, vNode, renderOptions) {
    var newNode = render(vNode, renderOptions)

    if (parentNode) {
        parentNode.appendChild(newNode)
    }

    return parentNode
}

function stringPatch(domNode, leftVNode, vText, renderOptions) {
    var newNode

    if (domNode.nodeType === 3) {
        domNode.replaceData(0, domNode.length, vText.text)
        newNode = domNode
    } else {
        var parentNode = domNode.parentNode
        newNode = render(vText, renderOptions)

        if (parentNode && newNode !== domNode) {
            parentNode.replaceChild(newNode, domNode)
        }
    }

    return newNode
}

function widgetPatch(domNode, leftVNode, widget, renderOptions) {
    var updating = updateWidget(leftVNode, widget)
    var newNode

    if (updating) {
        newNode = widget.update(leftVNode, domNode) || domNode
    } else {
        newNode = render(widget, renderOptions)
    }

    var parentNode = domNode.parentNode

    if (parentNode && newNode !== domNode) {
        parentNode.replaceChild(newNode, domNode)
    }

    if (!updating) {
        destroyWidget(domNode, leftVNode)
    }

    return newNode
}

function vNodePatch(domNode, leftVNode, vNode, renderOptions) {
    var parentNode = domNode.parentNode
    var newNode = render(vNode, renderOptions)

    if (parentNode && newNode !== domNode) {
        parentNode.replaceChild(newNode, domNode)
    }

    return newNode
}

function destroyWidget(domNode, w) {
    if (typeof w.destroy === "function" && isWidget(w)) {
        w.destroy(domNode)
    }
}

function reorderChildren(domNode, moves) {
    var childNodes = domNode.childNodes
    var keyMap = {}
    var node
    var remove
    var insert

    for (var i = 0; i < moves.removes.length; i++) {
        remove = moves.removes[i]
        node = childNodes[remove.from]
        if (remove.key) {
            keyMap[remove.key] = node
        }
        domNode.removeChild(node)
    }

    var length = childNodes.length
    for (var j = 0; j < moves.inserts.length; j++) {
        insert = moves.inserts[j]
        node = keyMap[insert.key]
        // this is the weirdest bug i've ever seen in webkit
        domNode.insertBefore(node, insert.to >= length++ ? null : childNodes[insert.to])
    }
}

function replaceRoot(oldRoot, newRoot) {
    if (oldRoot && newRoot && oldRoot !== newRoot && oldRoot.parentNode) {
        oldRoot.parentNode.replaceChild(newRoot, oldRoot)
    }

    return newRoot;
}

},{"../vnode/is-widget.js":16,"../vnode/vpatch.js":19,"./apply-properties":5,"./create-element":6,"./update-widget":10}],9:[function(require,module,exports){
var document = require("global/document")
var isArray = require("x-is-array")

var domIndex = require("./dom-index")
var patchOp = require("./patch-op")
module.exports = patch

function patch(rootNode, patches) {
    return patchRecursive(rootNode, patches)
}

function patchRecursive(rootNode, patches, renderOptions) {
    var indices = patchIndices(patches)

    if (indices.length === 0) {
        return rootNode
    }

    var index = domIndex(rootNode, patches.a, indices)
    var ownerDocument = rootNode.ownerDocument

    if (!renderOptions) {
        renderOptions = { patch: patchRecursive }
        if (ownerDocument !== document) {
            renderOptions.document = ownerDocument
        }
    }

    for (var i = 0; i < indices.length; i++) {
        var nodeIndex = indices[i]
        rootNode = applyPatch(rootNode,
            index[nodeIndex],
            patches[nodeIndex],
            renderOptions)
    }

    return rootNode
}

function applyPatch(rootNode, domNode, patchList, renderOptions) {
    if (!domNode) {
        return rootNode
    }

    var newNode

    if (isArray(patchList)) {
        for (var i = 0; i < patchList.length; i++) {
            newNode = patchOp(patchList[i], domNode, renderOptions)

            if (domNode === rootNode) {
                rootNode = newNode
            }
        }
    } else {
        newNode = patchOp(patchList, domNode, renderOptions)

        if (domNode === rootNode) {
            rootNode = newNode
        }
    }

    return rootNode
}

function patchIndices(patches) {
    var indices = []

    for (var key in patches) {
        if (key !== "a") {
            indices.push(Number(key))
        }
    }

    return indices
}

},{"./dom-index":7,"./patch-op":8,"global/document":2,"x-is-array":4}],10:[function(require,module,exports){
var isWidget = require("../vnode/is-widget.js")

module.exports = updateWidget

function updateWidget(a, b) {
    if (isWidget(a) && isWidget(b)) {
        if ("name" in a && "name" in b) {
            return a.id === b.id
        } else {
            return a.init === b.init
        }
    }

    return false
}

},{"../vnode/is-widget.js":16}],11:[function(require,module,exports){
var isVNode = require("./is-vnode")
var isVText = require("./is-vtext")
var isWidget = require("./is-widget")
var isThunk = require("./is-thunk")

module.exports = handleThunk

function handleThunk(a, b) {
    var renderedA = a
    var renderedB = b

    if (isThunk(b)) {
        renderedB = renderThunk(b, a)
    }

    if (isThunk(a)) {
        renderedA = renderThunk(a, null)
    }

    return {
        a: renderedA,
        b: renderedB
    }
}

function renderThunk(thunk, previous) {
    var renderedThunk = thunk.vnode

    if (!renderedThunk) {
        renderedThunk = thunk.vnode = thunk.render(previous)
    }

    if (!(isVNode(renderedThunk) ||
            isVText(renderedThunk) ||
            isWidget(renderedThunk))) {
        throw new Error("thunk did not return a valid node");
    }

    return renderedThunk
}

},{"./is-thunk":12,"./is-vnode":14,"./is-vtext":15,"./is-widget":16}],12:[function(require,module,exports){
module.exports = isThunk

function isThunk(t) {
    return t && t.type === "Thunk"
}

},{}],13:[function(require,module,exports){
module.exports = isHook

function isHook(hook) {
    return hook &&
      (typeof hook.hook === "function" && !hook.hasOwnProperty("hook") ||
       typeof hook.unhook === "function" && !hook.hasOwnProperty("unhook"))
}

},{}],14:[function(require,module,exports){
var version = require("./version")

module.exports = isVirtualNode

function isVirtualNode(x) {
    return x && x.type === "VirtualNode" && x.version === version
}

},{"./version":17}],15:[function(require,module,exports){
var version = require("./version")

module.exports = isVirtualText

function isVirtualText(x) {
    return x && x.type === "VirtualText" && x.version === version
}

},{"./version":17}],16:[function(require,module,exports){
module.exports = isWidget

function isWidget(w) {
    return w && w.type === "Widget"
}

},{}],17:[function(require,module,exports){
module.exports = "2"

},{}],18:[function(require,module,exports){
var version = require("./version")
var isVNode = require("./is-vnode")
var isWidget = require("./is-widget")
var isThunk = require("./is-thunk")
var isVHook = require("./is-vhook")

module.exports = VirtualNode

var noProperties = {}
var noChildren = []

function VirtualNode(tagName, properties, children, key, namespace) {
    this.tagName = tagName
    this.properties = properties || noProperties
    this.children = children || noChildren
    this.key = key != null ? String(key) : undefined
    this.namespace = (typeof namespace === "string") ? namespace : null

    var count = (children && children.length) || 0
    var descendants = 0
    var hasWidgets = false
    var hasThunks = false
    var descendantHooks = false
    var hooks

    for (var propName in properties) {
        if (properties.hasOwnProperty(propName)) {
            var property = properties[propName]
            if (isVHook(property) && property.unhook) {
                if (!hooks) {
                    hooks = {}
                }

                hooks[propName] = property
            }
        }
    }

    for (var i = 0; i < count; i++) {
        var child = children[i]
        if (isVNode(child)) {
            descendants += child.count || 0

            if (!hasWidgets && child.hasWidgets) {
                hasWidgets = true
            }

            if (!hasThunks && child.hasThunks) {
                hasThunks = true
            }

            if (!descendantHooks && (child.hooks || child.descendantHooks)) {
                descendantHooks = true
            }
        } else if (!hasWidgets && isWidget(child)) {
            if (typeof child.destroy === "function") {
                hasWidgets = true
            }
        } else if (!hasThunks && isThunk(child)) {
            hasThunks = true;
        }
    }

    this.count = count + descendants
    this.hasWidgets = hasWidgets
    this.hasThunks = hasThunks
    this.hooks = hooks
    this.descendantHooks = descendantHooks
}

VirtualNode.prototype.version = version
VirtualNode.prototype.type = "VirtualNode"

},{"./is-thunk":12,"./is-vhook":13,"./is-vnode":14,"./is-widget":16,"./version":17}],19:[function(require,module,exports){
var version = require("./version")

VirtualPatch.NONE = 0
VirtualPatch.VTEXT = 1
VirtualPatch.VNODE = 2
VirtualPatch.WIDGET = 3
VirtualPatch.PROPS = 4
VirtualPatch.ORDER = 5
VirtualPatch.INSERT = 6
VirtualPatch.REMOVE = 7
VirtualPatch.THUNK = 8

module.exports = VirtualPatch

function VirtualPatch(type, vNode, patch) {
    this.type = Number(type)
    this.vNode = vNode
    this.patch = patch
}

VirtualPatch.prototype.version = version
VirtualPatch.prototype.type = "VirtualPatch"

},{"./version":17}],20:[function(require,module,exports){
var version = require("./version")

module.exports = VirtualText

function VirtualText(text) {
    this.text = String(text)
}

VirtualText.prototype.version = version
VirtualText.prototype.type = "VirtualText"

},{"./version":17}],21:[function(require,module,exports){
var isObject = require("is-object")
var isHook = require("../vnode/is-vhook")

module.exports = diffProps

function diffProps(a, b) {
    var diff

    for (var aKey in a) {
        if (!(aKey in b)) {
            diff = diff || {}
            diff[aKey] = undefined
        }

        var aValue = a[aKey]
        var bValue = b[aKey]

        if (aValue === bValue) {
            continue
        } else if (isObject(aValue) && isObject(bValue)) {
            if (getPrototype(bValue) !== getPrototype(aValue)) {
                diff = diff || {}
                diff[aKey] = bValue
            } else if (isHook(bValue)) {
                 diff = diff || {}
                 diff[aKey] = bValue
            } else {
                var objectDiff = diffProps(aValue, bValue)
                if (objectDiff) {
                    diff = diff || {}
                    diff[aKey] = objectDiff
                }
            }
        } else {
            diff = diff || {}
            diff[aKey] = bValue
        }
    }

    for (var bKey in b) {
        if (!(bKey in a)) {
            diff = diff || {}
            diff[bKey] = b[bKey]
        }
    }

    return diff
}

function getPrototype(value) {
  if (Object.getPrototypeOf) {
    return Object.getPrototypeOf(value)
  } else if (value.__proto__) {
    return value.__proto__
  } else if (value.constructor) {
    return value.constructor.prototype
  }
}

},{"../vnode/is-vhook":13,"is-object":3}],22:[function(require,module,exports){
var isArray = require("x-is-array")

var VPatch = require("../vnode/vpatch")
var isVNode = require("../vnode/is-vnode")
var isVText = require("../vnode/is-vtext")
var isWidget = require("../vnode/is-widget")
var isThunk = require("../vnode/is-thunk")
var handleThunk = require("../vnode/handle-thunk")

var diffProps = require("./diff-props")

module.exports = diff

function diff(a, b) {
    var patch = { a: a }
    walk(a, b, patch, 0)
    return patch
}

function walk(a, b, patch, index) {
    if (a === b) {
        return
    }

    var apply = patch[index]
    var applyClear = false

    if (isThunk(a) || isThunk(b)) {
        thunks(a, b, patch, index)
    } else if (b == null) {

        // If a is a widget we will add a remove patch for it
        // Otherwise any child widgets/hooks must be destroyed.
        // This prevents adding two remove patches for a widget.
        if (!isWidget(a)) {
            clearState(a, patch, index)
            apply = patch[index]
        }

        apply = appendPatch(apply, new VPatch(VPatch.REMOVE, a, b))
    } else if (isVNode(b)) {
        if (isVNode(a)) {
            if (a.tagName === b.tagName &&
                a.namespace === b.namespace &&
                a.key === b.key) {
                var propsPatch = diffProps(a.properties, b.properties)
                if (propsPatch) {
                    apply = appendPatch(apply,
                        new VPatch(VPatch.PROPS, a, propsPatch))
                }
                apply = diffChildren(a, b, patch, apply, index)
            } else {
                apply = appendPatch(apply, new VPatch(VPatch.VNODE, a, b))
                applyClear = true
            }
        } else {
            apply = appendPatch(apply, new VPatch(VPatch.VNODE, a, b))
            applyClear = true
        }
    } else if (isVText(b)) {
        if (!isVText(a)) {
            apply = appendPatch(apply, new VPatch(VPatch.VTEXT, a, b))
            applyClear = true
        } else if (a.text !== b.text) {
            apply = appendPatch(apply, new VPatch(VPatch.VTEXT, a, b))
        }
    } else if (isWidget(b)) {
        if (!isWidget(a)) {
            applyClear = true
        }

        apply = appendPatch(apply, new VPatch(VPatch.WIDGET, a, b))
    }

    if (apply) {
        patch[index] = apply
    }

    if (applyClear) {
        clearState(a, patch, index)
    }
}

function diffChildren(a, b, patch, apply, index) {
    var aChildren = a.children
    var orderedSet = reorder(aChildren, b.children)
    var bChildren = orderedSet.children

    var aLen = aChildren.length
    var bLen = bChildren.length
    var len = aLen > bLen ? aLen : bLen

    for (var i = 0; i < len; i++) {
        var leftNode = aChildren[i]
        var rightNode = bChildren[i]
        index += 1

        if (!leftNode) {
            if (rightNode) {
                // Excess nodes in b need to be added
                apply = appendPatch(apply,
                    new VPatch(VPatch.INSERT, null, rightNode))
            }
        } else {
            walk(leftNode, rightNode, patch, index)
        }

        if (isVNode(leftNode) && leftNode.count) {
            index += leftNode.count
        }
    }

    if (orderedSet.moves) {
        // Reorder nodes last
        apply = appendPatch(apply, new VPatch(
            VPatch.ORDER,
            a,
            orderedSet.moves
        ))
    }

    return apply
}

function clearState(vNode, patch, index) {
    // TODO: Make this a single walk, not two
    unhook(vNode, patch, index)
    destroyWidgets(vNode, patch, index)
}

// Patch records for all destroyed widgets must be added because we need
// a DOM node reference for the destroy function
function destroyWidgets(vNode, patch, index) {
    if (isWidget(vNode)) {
        if (typeof vNode.destroy === "function") {
            patch[index] = appendPatch(
                patch[index],
                new VPatch(VPatch.REMOVE, vNode, null)
            )
        }
    } else if (isVNode(vNode) && (vNode.hasWidgets || vNode.hasThunks)) {
        var children = vNode.children
        var len = children.length
        for (var i = 0; i < len; i++) {
            var child = children[i]
            index += 1

            destroyWidgets(child, patch, index)

            if (isVNode(child) && child.count) {
                index += child.count
            }
        }
    } else if (isThunk(vNode)) {
        thunks(vNode, null, patch, index)
    }
}

// Create a sub-patch for thunks
function thunks(a, b, patch, index) {
    var nodes = handleThunk(a, b)
    var thunkPatch = diff(nodes.a, nodes.b)
    if (hasPatches(thunkPatch)) {
        patch[index] = new VPatch(VPatch.THUNK, null, thunkPatch)
    }
}

function hasPatches(patch) {
    for (var index in patch) {
        if (index !== "a") {
            return true
        }
    }

    return false
}

// Execute hooks when two nodes are identical
function unhook(vNode, patch, index) {
    if (isVNode(vNode)) {
        if (vNode.hooks) {
            patch[index] = appendPatch(
                patch[index],
                new VPatch(
                    VPatch.PROPS,
                    vNode,
                    undefinedKeys(vNode.hooks)
                )
            )
        }

        if (vNode.descendantHooks || vNode.hasThunks) {
            var children = vNode.children
            var len = children.length
            for (var i = 0; i < len; i++) {
                var child = children[i]
                index += 1

                unhook(child, patch, index)

                if (isVNode(child) && child.count) {
                    index += child.count
                }
            }
        }
    } else if (isThunk(vNode)) {
        thunks(vNode, null, patch, index)
    }
}

function undefinedKeys(obj) {
    var result = {}

    for (var key in obj) {
        result[key] = undefined
    }

    return result
}

// List diff, naive left to right reordering
function reorder(aChildren, bChildren) {
    // O(M) time, O(M) memory
    var bChildIndex = keyIndex(bChildren)
    var bKeys = bChildIndex.keys
    var bFree = bChildIndex.free

    if (bFree.length === bChildren.length) {
        return {
            children: bChildren,
            moves: null
        }
    }

    // O(N) time, O(N) memory
    var aChildIndex = keyIndex(aChildren)
    var aKeys = aChildIndex.keys
    var aFree = aChildIndex.free

    if (aFree.length === aChildren.length) {
        return {
            children: bChildren,
            moves: null
        }
    }

    // O(MAX(N, M)) memory
    var newChildren = []

    var freeIndex = 0
    var freeCount = bFree.length
    var deletedItems = 0

    // Iterate through a and match a node in b
    // O(N) time,
    for (var i = 0 ; i < aChildren.length; i++) {
        var aItem = aChildren[i]
        var itemIndex

        if (aItem.key) {
            if (bKeys.hasOwnProperty(aItem.key)) {
                // Match up the old keys
                itemIndex = bKeys[aItem.key]
                newChildren.push(bChildren[itemIndex])

            } else {
                // Remove old keyed items
                itemIndex = i - deletedItems++
                newChildren.push(null)
            }
        } else {
            // Match the item in a with the next free item in b
            if (freeIndex < freeCount) {
                itemIndex = bFree[freeIndex++]
                newChildren.push(bChildren[itemIndex])
            } else {
                // There are no free items in b to match with
                // the free items in a, so the extra free nodes
                // are deleted.
                itemIndex = i - deletedItems++
                newChildren.push(null)
            }
        }
    }

    var lastFreeIndex = freeIndex >= bFree.length ?
        bChildren.length :
        bFree[freeIndex]

    // Iterate through b and append any new keys
    // O(M) time
    for (var j = 0; j < bChildren.length; j++) {
        var newItem = bChildren[j]

        if (newItem.key) {
            if (!aKeys.hasOwnProperty(newItem.key)) {
                // Add any new keyed items
                // We are adding new items to the end and then sorting them
                // in place. In future we should insert new items in place.
                newChildren.push(newItem)
            }
        } else if (j >= lastFreeIndex) {
            // Add any leftover non-keyed items
            newChildren.push(newItem)
        }
    }

    var simulate = newChildren.slice()
    var simulateIndex = 0
    var removes = []
    var inserts = []
    var simulateItem

    for (var k = 0; k < bChildren.length;) {
        var wantedItem = bChildren[k]
        simulateItem = simulate[simulateIndex]

        // remove items
        while (simulateItem === null && simulate.length) {
            removes.push(remove(simulate, simulateIndex, null))
            simulateItem = simulate[simulateIndex]
        }

        if (!simulateItem || simulateItem.key !== wantedItem.key) {
            // if we need a key in this position...
            if (wantedItem.key) {
                if (simulateItem && simulateItem.key) {
                    // if an insert doesn't put this key in place, it needs to move
                    if (bKeys[simulateItem.key] !== k + 1) {
                        removes.push(remove(simulate, simulateIndex, simulateItem.key))
                        simulateItem = simulate[simulateIndex]
                        // if the remove didn't put the wanted item in place, we need to insert it
                        if (!simulateItem || simulateItem.key !== wantedItem.key) {
                            inserts.push({key: wantedItem.key, to: k})
                        }
                        // items are matching, so skip ahead
                        else {
                            simulateIndex++
                        }
                    }
                    else {
                        inserts.push({key: wantedItem.key, to: k})
                    }
                }
                else {
                    inserts.push({key: wantedItem.key, to: k})
                }
                k++
            }
            // a key in simulate has no matching wanted key, remove it
            else if (simulateItem && simulateItem.key) {
                removes.push(remove(simulate, simulateIndex, simulateItem.key))
            }
        }
        else {
            simulateIndex++
            k++
        }
    }

    // remove all the remaining nodes from simulate
    while(simulateIndex < simulate.length) {
        simulateItem = simulate[simulateIndex]
        removes.push(remove(simulate, simulateIndex, simulateItem && simulateItem.key))
    }

    // If the only moves we have are deletes then we can just
    // let the delete patch remove these items.
    if (removes.length === deletedItems && !inserts.length) {
        return {
            children: newChildren,
            moves: null
        }
    }

    return {
        children: newChildren,
        moves: {
            removes: removes,
            inserts: inserts
        }
    }
}

function remove(arr, index, key) {
    arr.splice(index, 1)

    return {
        from: index,
        key: key
    }
}

function keyIndex(children) {
    var keys = {}
    var free = []
    var length = children.length

    for (var i = 0; i < length; i++) {
        var child = children[i]

        if (child.key) {
            keys[child.key] = i
        } else {
            free.push(i)
        }
    }

    return {
        keys: keys,     // A hash of key name to index
        free: free,     // An array of unkeyed item indices
    }
}

function appendPatch(apply, patch) {
    if (apply) {
        if (isArray(apply)) {
            apply.push(patch)
        } else {
            apply = [apply, patch]
        }

        return apply
    } else {
        return patch
    }
}

},{"../vnode/handle-thunk":11,"../vnode/is-thunk":12,"../vnode/is-vnode":14,"../vnode/is-vtext":15,"../vnode/is-widget":16,"../vnode/vpatch":19,"./diff-props":21,"x-is-array":4}],23:[function(require,module,exports){
var VNode = require('virtual-dom/vnode/vnode');
var VText = require('virtual-dom/vnode/vtext');
var diff = require('virtual-dom/vtree/diff');
var patch = require('virtual-dom/vdom/patch');
var createElement = require('virtual-dom/vdom/create-element');
var isHook = require("virtual-dom/vnode/is-vhook");


Elm.Native.VirtualDom = {};
Elm.Native.VirtualDom.make = function(elm)
{
	elm.Native = elm.Native || {};
	elm.Native.VirtualDom = elm.Native.VirtualDom || {};
	if (elm.Native.VirtualDom.values)
	{
		return elm.Native.VirtualDom.values;
	}

	var Element = Elm.Native.Graphics.Element.make(elm);
	var Json = Elm.Native.Json.make(elm);
	var List = Elm.Native.List.make(elm);
	var Signal = Elm.Native.Signal.make(elm);
	var Utils = Elm.Native.Utils.make(elm);

	var ATTRIBUTE_KEY = 'UniqueNameThatOthersAreVeryUnlikelyToUse';



	// VIRTUAL DOM NODES


	function text(string)
	{
		return new VText(string);
	}

	function node(name)
	{
		return F2(function(propertyList, contents) {
			return makeNode(name, propertyList, contents);
		});
	}


	// BUILD VIRTUAL DOME NODES


	function makeNode(name, propertyList, contents)
	{
		var props = listToProperties(propertyList);

		var key, namespace;
		// support keys
		if (props.key !== undefined)
		{
			key = props.key;
			props.key = undefined;
		}

		// support namespace
		if (props.namespace !== undefined)
		{
			namespace = props.namespace;
			props.namespace = undefined;
		}

		// ensure that setting text of an input does not move the cursor
		var useSoftSet =
			(name === 'input' || name === 'textarea')
			&& props.value !== undefined
			&& !isHook(props.value);

		if (useSoftSet)
		{
			props.value = SoftSetHook(props.value);
		}

		return new VNode(name, props, List.toArray(contents), key, namespace);
	}

	function listToProperties(list)
	{
		var object = {};
		while (list.ctor !== '[]')
		{
			var entry = list._0;
			if (entry.key === ATTRIBUTE_KEY)
			{
				object.attributes = object.attributes || {};
				object.attributes[entry.value.attrKey] = entry.value.attrValue;
			}
			else
			{
				object[entry.key] = entry.value;
			}
			list = list._1;
		}
		return object;
	}



	// PROPERTIES AND ATTRIBUTES


	function property(key, value)
	{
		return {
			key: key,
			value: value
		};
	}

	function attribute(key, value)
	{
		return {
			key: ATTRIBUTE_KEY,
			value: {
				attrKey: key,
				attrValue: value
			}
		};
	}



	// NAMESPACED ATTRIBUTES


	function attributeNS(namespace, key, value)
	{
		return {
			key: key,
			value: new AttributeHook(namespace, key, value)
		};
	}

	function AttributeHook(namespace, key, value)
	{
		if (!(this instanceof AttributeHook))
		{
			return new AttributeHook(namespace, key, value);
		}

		this.namespace = namespace;
		this.key = key;
		this.value = value;
	}

	AttributeHook.prototype.hook = function (node, prop, prev)
	{
		if (prev
			&& prev.type === 'AttributeHook'
			&& prev.value === this.value
			&& prev.namespace === this.namespace)
		{
			return;
		}

		node.setAttributeNS(this.namespace, prop, this.value);
	};

	AttributeHook.prototype.unhook = function (node, prop, next)
	{
		if (next
			&& next.type === 'AttributeHook'
			&& next.namespace === this.namespace)
		{
			return;
		}

		node.removeAttributeNS(this.namespace, this.key);
	};

	AttributeHook.prototype.type = 'AttributeHook';



	// EVENTS


	function on(name, options, decoder, createMessage)
	{
		function eventHandler(event)
		{
			var value = A2(Json.runDecoderValue, decoder, event);
			if (value.ctor === 'Ok')
			{
				if (options.stopPropagation)
				{
					event.stopPropagation();
				}
				if (options.preventDefault)
				{
					event.preventDefault();
				}
				Signal.sendMessage(createMessage(value._0));
			}
		}
		return property('on' + name, eventHandler);
	}

	function SoftSetHook(value)
	{
		if (!(this instanceof SoftSetHook))
		{
			return new SoftSetHook(value);
		}

		this.value = value;
	}

	SoftSetHook.prototype.hook = function (node, propertyName)
	{
		if (node[propertyName] !== this.value)
		{
			node[propertyName] = this.value;
		}
	};



	// INTEGRATION WITH ELEMENTS


	function ElementWidget(element)
	{
		this.element = element;
	}

	ElementWidget.prototype.type = "Widget";

	ElementWidget.prototype.init = function init()
	{
		return Element.render(this.element);
	};

	ElementWidget.prototype.update = function update(previous, node)
	{
		return Element.update(node, previous.element, this.element);
	};

	function fromElement(element)
	{
		return new ElementWidget(element);
	}

	function toElement(width, height, html)
	{
		return A3(Element.newElement, width, height, {
			ctor: 'Custom',
			type: 'evancz/elm-html',
			render: render,
			update: update,
			model: html
		});
	}



	// RENDER AND UPDATE


	function render(model)
	{
		var element = Element.createNode('div');
		element.appendChild(createElement(model));
		return element;
	}

	function update(node, oldModel, newModel)
	{
		updateAndReplace(node.firstChild, oldModel, newModel);
		return node;
	}

	function updateAndReplace(node, oldModel, newModel)
	{
		var patches = diff(oldModel, newModel);
		var newNode = patch(node, patches);
		return newNode;
	}



	// LAZINESS


	function lazyRef(fn, a)
	{
		function thunk()
		{
			return fn(a);
		}
		return new Thunk(fn, [a], thunk);
	}

	function lazyRef2(fn, a, b)
	{
		function thunk()
		{
			return A2(fn, a, b);
		}
		return new Thunk(fn, [a,b], thunk);
	}

	function lazyRef3(fn, a, b, c)
	{
		function thunk()
		{
			return A3(fn, a, b, c);
		}
		return new Thunk(fn, [a,b,c], thunk);
	}

	function Thunk(fn, args, thunk)
	{
		/* public (used by VirtualDom.js) */
		this.vnode = null;
		this.key = undefined;

		/* private */
		this.fn = fn;
		this.args = args;
		this.thunk = thunk;
	}

	Thunk.prototype.type = "Thunk";
	Thunk.prototype.render = renderThunk;

	function shouldUpdate(current, previous)
	{
		if (current.fn !== previous.fn)
		{
			return true;
		}

		// if it's the same function, we know the number of args must match
		var cargs = current.args;
		var pargs = previous.args;

		for (var i = cargs.length; i--; )
		{
			if (cargs[i] !== pargs[i])
			{
				return true;
			}
		}

		return false;
	}

	function renderThunk(previous)
	{
		if (previous == null || shouldUpdate(this, previous))
		{
			return this.thunk();
		}
		else
		{
			return previous.vnode;
		}
	}


	return elm.Native.VirtualDom.values = Elm.Native.VirtualDom.values = {
		node: node,
		text: text,
		on: F4(on),

		property: F2(property),
		attribute: F2(attribute),
		attributeNS: F3(attributeNS),

		lazy: F2(lazyRef),
		lazy2: F3(lazyRef2),
		lazy3: F4(lazyRef3),

		toElement: F3(toElement),
		fromElement: fromElement,

		render: createElement,
		updateAndReplace: updateAndReplace
	};
};

},{"virtual-dom/vdom/create-element":6,"virtual-dom/vdom/patch":9,"virtual-dom/vnode/is-vhook":13,"virtual-dom/vnode/vnode":18,"virtual-dom/vnode/vtext":20,"virtual-dom/vtree/diff":22}]},{},[23]);

Elm.VirtualDom = Elm.VirtualDom || {};
Elm.VirtualDom.make = function (_elm) {
   "use strict";
   _elm.VirtualDom = _elm.VirtualDom || {};
   if (_elm.VirtualDom.values) return _elm.VirtualDom.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$VirtualDom = Elm.Native.VirtualDom.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var lazy3 = $Native$VirtualDom.lazy3;
   var lazy2 = $Native$VirtualDom.lazy2;
   var lazy = $Native$VirtualDom.lazy;
   var defaultOptions = {stopPropagation: false,preventDefault: false};
   var Options = F2(function (a,b) {    return {stopPropagation: a,preventDefault: b};});
   var onWithOptions = $Native$VirtualDom.on;
   var on = F3(function (eventName,decoder,toMessage) {    return A4($Native$VirtualDom.on,eventName,defaultOptions,decoder,toMessage);});
   var attributeNS = $Native$VirtualDom.attributeNS;
   var attribute = $Native$VirtualDom.attribute;
   var property = $Native$VirtualDom.property;
   var Property = {ctor: "Property"};
   var fromElement = $Native$VirtualDom.fromElement;
   var toElement = $Native$VirtualDom.toElement;
   var text = $Native$VirtualDom.text;
   var node = $Native$VirtualDom.node;
   var Node = {ctor: "Node"};
   return _elm.VirtualDom.values = {_op: _op
                                   ,text: text
                                   ,node: node
                                   ,toElement: toElement
                                   ,fromElement: fromElement
                                   ,property: property
                                   ,attribute: attribute
                                   ,attributeNS: attributeNS
                                   ,on: on
                                   ,onWithOptions: onWithOptions
                                   ,defaultOptions: defaultOptions
                                   ,lazy: lazy
                                   ,lazy2: lazy2
                                   ,lazy3: lazy3
                                   ,Options: Options};
};
Elm.Html = Elm.Html || {};
Elm.Html.make = function (_elm) {
   "use strict";
   _elm.Html = _elm.Html || {};
   if (_elm.Html.values) return _elm.Html.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $VirtualDom = Elm.VirtualDom.make(_elm);
   var _op = {};
   var fromElement = $VirtualDom.fromElement;
   var toElement = $VirtualDom.toElement;
   var text = $VirtualDom.text;
   var node = $VirtualDom.node;
   var body = node("body");
   var section = node("section");
   var nav = node("nav");
   var article = node("article");
   var aside = node("aside");
   var h1 = node("h1");
   var h2 = node("h2");
   var h3 = node("h3");
   var h4 = node("h4");
   var h5 = node("h5");
   var h6 = node("h6");
   var header = node("header");
   var footer = node("footer");
   var address = node("address");
   var main$ = node("main");
   var p = node("p");
   var hr = node("hr");
   var pre = node("pre");
   var blockquote = node("blockquote");
   var ol = node("ol");
   var ul = node("ul");
   var li = node("li");
   var dl = node("dl");
   var dt = node("dt");
   var dd = node("dd");
   var figure = node("figure");
   var figcaption = node("figcaption");
   var div = node("div");
   var a = node("a");
   var em = node("em");
   var strong = node("strong");
   var small = node("small");
   var s = node("s");
   var cite = node("cite");
   var q = node("q");
   var dfn = node("dfn");
   var abbr = node("abbr");
   var time = node("time");
   var code = node("code");
   var $var = node("var");
   var samp = node("samp");
   var kbd = node("kbd");
   var sub = node("sub");
   var sup = node("sup");
   var i = node("i");
   var b = node("b");
   var u = node("u");
   var mark = node("mark");
   var ruby = node("ruby");
   var rt = node("rt");
   var rp = node("rp");
   var bdi = node("bdi");
   var bdo = node("bdo");
   var span = node("span");
   var br = node("br");
   var wbr = node("wbr");
   var ins = node("ins");
   var del = node("del");
   var img = node("img");
   var iframe = node("iframe");
   var embed = node("embed");
   var object = node("object");
   var param = node("param");
   var video = node("video");
   var audio = node("audio");
   var source = node("source");
   var track = node("track");
   var canvas = node("canvas");
   var svg = node("svg");
   var math = node("math");
   var table = node("table");
   var caption = node("caption");
   var colgroup = node("colgroup");
   var col = node("col");
   var tbody = node("tbody");
   var thead = node("thead");
   var tfoot = node("tfoot");
   var tr = node("tr");
   var td = node("td");
   var th = node("th");
   var form = node("form");
   var fieldset = node("fieldset");
   var legend = node("legend");
   var label = node("label");
   var input = node("input");
   var button = node("button");
   var select = node("select");
   var datalist = node("datalist");
   var optgroup = node("optgroup");
   var option = node("option");
   var textarea = node("textarea");
   var keygen = node("keygen");
   var output = node("output");
   var progress = node("progress");
   var meter = node("meter");
   var details = node("details");
   var summary = node("summary");
   var menuitem = node("menuitem");
   var menu = node("menu");
   return _elm.Html.values = {_op: _op
                             ,node: node
                             ,text: text
                             ,toElement: toElement
                             ,fromElement: fromElement
                             ,body: body
                             ,section: section
                             ,nav: nav
                             ,article: article
                             ,aside: aside
                             ,h1: h1
                             ,h2: h2
                             ,h3: h3
                             ,h4: h4
                             ,h5: h5
                             ,h6: h6
                             ,header: header
                             ,footer: footer
                             ,address: address
                             ,main$: main$
                             ,p: p
                             ,hr: hr
                             ,pre: pre
                             ,blockquote: blockquote
                             ,ol: ol
                             ,ul: ul
                             ,li: li
                             ,dl: dl
                             ,dt: dt
                             ,dd: dd
                             ,figure: figure
                             ,figcaption: figcaption
                             ,div: div
                             ,a: a
                             ,em: em
                             ,strong: strong
                             ,small: small
                             ,s: s
                             ,cite: cite
                             ,q: q
                             ,dfn: dfn
                             ,abbr: abbr
                             ,time: time
                             ,code: code
                             ,$var: $var
                             ,samp: samp
                             ,kbd: kbd
                             ,sub: sub
                             ,sup: sup
                             ,i: i
                             ,b: b
                             ,u: u
                             ,mark: mark
                             ,ruby: ruby
                             ,rt: rt
                             ,rp: rp
                             ,bdi: bdi
                             ,bdo: bdo
                             ,span: span
                             ,br: br
                             ,wbr: wbr
                             ,ins: ins
                             ,del: del
                             ,img: img
                             ,iframe: iframe
                             ,embed: embed
                             ,object: object
                             ,param: param
                             ,video: video
                             ,audio: audio
                             ,source: source
                             ,track: track
                             ,canvas: canvas
                             ,svg: svg
                             ,math: math
                             ,table: table
                             ,caption: caption
                             ,colgroup: colgroup
                             ,col: col
                             ,tbody: tbody
                             ,thead: thead
                             ,tfoot: tfoot
                             ,tr: tr
                             ,td: td
                             ,th: th
                             ,form: form
                             ,fieldset: fieldset
                             ,legend: legend
                             ,label: label
                             ,input: input
                             ,button: button
                             ,select: select
                             ,datalist: datalist
                             ,optgroup: optgroup
                             ,option: option
                             ,textarea: textarea
                             ,keygen: keygen
                             ,output: output
                             ,progress: progress
                             ,meter: meter
                             ,details: details
                             ,summary: summary
                             ,menuitem: menuitem
                             ,menu: menu};
};
Elm.Html = Elm.Html || {};
Elm.Html.Attributes = Elm.Html.Attributes || {};
Elm.Html.Attributes.make = function (_elm) {
   "use strict";
   _elm.Html = _elm.Html || {};
   _elm.Html.Attributes = _elm.Html.Attributes || {};
   if (_elm.Html.Attributes.values) return _elm.Html.Attributes.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Json$Encode = Elm.Json.Encode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm),
   $VirtualDom = Elm.VirtualDom.make(_elm);
   var _op = {};
   var attribute = $VirtualDom.attribute;
   var contextmenu = function (value) {    return A2(attribute,"contextmenu",value);};
   var property = $VirtualDom.property;
   var stringProperty = F2(function (name,string) {    return A2(property,name,$Json$Encode.string(string));});
   var $class = function (name) {    return A2(stringProperty,"className",name);};
   var id = function (name) {    return A2(stringProperty,"id",name);};
   var title = function (name) {    return A2(stringProperty,"title",name);};
   var accesskey = function ($char) {    return A2(stringProperty,"accessKey",$String.fromChar($char));};
   var dir = function (value) {    return A2(stringProperty,"dir",value);};
   var draggable = function (value) {    return A2(stringProperty,"draggable",value);};
   var dropzone = function (value) {    return A2(stringProperty,"dropzone",value);};
   var itemprop = function (value) {    return A2(stringProperty,"itemprop",value);};
   var lang = function (value) {    return A2(stringProperty,"lang",value);};
   var tabindex = function (n) {    return A2(stringProperty,"tabIndex",$Basics.toString(n));};
   var charset = function (value) {    return A2(stringProperty,"charset",value);};
   var content = function (value) {    return A2(stringProperty,"content",value);};
   var httpEquiv = function (value) {    return A2(stringProperty,"httpEquiv",value);};
   var language = function (value) {    return A2(stringProperty,"language",value);};
   var src = function (value) {    return A2(stringProperty,"src",value);};
   var height = function (value) {    return A2(stringProperty,"height",$Basics.toString(value));};
   var width = function (value) {    return A2(stringProperty,"width",$Basics.toString(value));};
   var alt = function (value) {    return A2(stringProperty,"alt",value);};
   var preload = function (value) {    return A2(stringProperty,"preload",value);};
   var poster = function (value) {    return A2(stringProperty,"poster",value);};
   var kind = function (value) {    return A2(stringProperty,"kind",value);};
   var srclang = function (value) {    return A2(stringProperty,"srclang",value);};
   var sandbox = function (value) {    return A2(stringProperty,"sandbox",value);};
   var srcdoc = function (value) {    return A2(stringProperty,"srcdoc",value);};
   var type$ = function (value) {    return A2(stringProperty,"type",value);};
   var value = function (value) {    return A2(stringProperty,"value",value);};
   var placeholder = function (value) {    return A2(stringProperty,"placeholder",value);};
   var accept = function (value) {    return A2(stringProperty,"accept",value);};
   var acceptCharset = function (value) {    return A2(stringProperty,"acceptCharset",value);};
   var action = function (value) {    return A2(stringProperty,"action",value);};
   var autocomplete = function (bool) {    return A2(stringProperty,"autocomplete",bool ? "on" : "off");};
   var autosave = function (value) {    return A2(stringProperty,"autosave",value);};
   var enctype = function (value) {    return A2(stringProperty,"enctype",value);};
   var formaction = function (value) {    return A2(stringProperty,"formAction",value);};
   var list = function (value) {    return A2(stringProperty,"list",value);};
   var minlength = function (n) {    return A2(stringProperty,"minLength",$Basics.toString(n));};
   var maxlength = function (n) {    return A2(stringProperty,"maxLength",$Basics.toString(n));};
   var method = function (value) {    return A2(stringProperty,"method",value);};
   var name = function (value) {    return A2(stringProperty,"name",value);};
   var pattern = function (value) {    return A2(stringProperty,"pattern",value);};
   var size = function (n) {    return A2(stringProperty,"size",$Basics.toString(n));};
   var $for = function (value) {    return A2(stringProperty,"htmlFor",value);};
   var form = function (value) {    return A2(stringProperty,"form",value);};
   var max = function (value) {    return A2(stringProperty,"max",value);};
   var min = function (value) {    return A2(stringProperty,"min",value);};
   var step = function (n) {    return A2(stringProperty,"step",n);};
   var cols = function (n) {    return A2(stringProperty,"cols",$Basics.toString(n));};
   var rows = function (n) {    return A2(stringProperty,"rows",$Basics.toString(n));};
   var wrap = function (value) {    return A2(stringProperty,"wrap",value);};
   var usemap = function (value) {    return A2(stringProperty,"useMap",value);};
   var shape = function (value) {    return A2(stringProperty,"shape",value);};
   var coords = function (value) {    return A2(stringProperty,"coords",value);};
   var challenge = function (value) {    return A2(stringProperty,"challenge",value);};
   var keytype = function (value) {    return A2(stringProperty,"keytype",value);};
   var align = function (value) {    return A2(stringProperty,"align",value);};
   var cite = function (value) {    return A2(stringProperty,"cite",value);};
   var href = function (value) {    return A2(stringProperty,"href",value);};
   var target = function (value) {    return A2(stringProperty,"target",value);};
   var downloadAs = function (value) {    return A2(stringProperty,"download",value);};
   var hreflang = function (value) {    return A2(stringProperty,"hreflang",value);};
   var media = function (value) {    return A2(stringProperty,"media",value);};
   var ping = function (value) {    return A2(stringProperty,"ping",value);};
   var rel = function (value) {    return A2(stringProperty,"rel",value);};
   var datetime = function (value) {    return A2(stringProperty,"datetime",value);};
   var pubdate = function (value) {    return A2(stringProperty,"pubdate",value);};
   var start = function (n) {    return A2(stringProperty,"start",$Basics.toString(n));};
   var colspan = function (n) {    return A2(stringProperty,"colSpan",$Basics.toString(n));};
   var headers = function (value) {    return A2(stringProperty,"headers",value);};
   var rowspan = function (n) {    return A2(stringProperty,"rowSpan",$Basics.toString(n));};
   var scope = function (value) {    return A2(stringProperty,"scope",value);};
   var manifest = function (value) {    return A2(stringProperty,"manifest",value);};
   var boolProperty = F2(function (name,bool) {    return A2(property,name,$Json$Encode.bool(bool));});
   var hidden = function (bool) {    return A2(boolProperty,"hidden",bool);};
   var contenteditable = function (bool) {    return A2(boolProperty,"contentEditable",bool);};
   var spellcheck = function (bool) {    return A2(boolProperty,"spellcheck",bool);};
   var async = function (bool) {    return A2(boolProperty,"async",bool);};
   var defer = function (bool) {    return A2(boolProperty,"defer",bool);};
   var scoped = function (bool) {    return A2(boolProperty,"scoped",bool);};
   var autoplay = function (bool) {    return A2(boolProperty,"autoplay",bool);};
   var controls = function (bool) {    return A2(boolProperty,"controls",bool);};
   var loop = function (bool) {    return A2(boolProperty,"loop",bool);};
   var $default = function (bool) {    return A2(boolProperty,"default",bool);};
   var seamless = function (bool) {    return A2(boolProperty,"seamless",bool);};
   var checked = function (bool) {    return A2(boolProperty,"checked",bool);};
   var selected = function (bool) {    return A2(boolProperty,"selected",bool);};
   var autofocus = function (bool) {    return A2(boolProperty,"autofocus",bool);};
   var disabled = function (bool) {    return A2(boolProperty,"disabled",bool);};
   var multiple = function (bool) {    return A2(boolProperty,"multiple",bool);};
   var novalidate = function (bool) {    return A2(boolProperty,"noValidate",bool);};
   var readonly = function (bool) {    return A2(boolProperty,"readOnly",bool);};
   var required = function (bool) {    return A2(boolProperty,"required",bool);};
   var ismap = function (value) {    return A2(boolProperty,"isMap",value);};
   var download = function (bool) {    return A2(boolProperty,"download",bool);};
   var reversed = function (bool) {    return A2(boolProperty,"reversed",bool);};
   var classList = function (list) {    return $class(A2($String.join," ",A2($List.map,$Basics.fst,A2($List.filter,$Basics.snd,list))));};
   var style = function (props) {
      return A2(property,
      "style",
      $Json$Encode.object(A2($List.map,function (_p0) {    var _p1 = _p0;return {ctor: "_Tuple2",_0: _p1._0,_1: $Json$Encode.string(_p1._1)};},props)));
   };
   var key = function (k) {    return A2(stringProperty,"key",k);};
   return _elm.Html.Attributes.values = {_op: _op
                                        ,key: key
                                        ,style: style
                                        ,$class: $class
                                        ,classList: classList
                                        ,id: id
                                        ,title: title
                                        ,hidden: hidden
                                        ,type$: type$
                                        ,value: value
                                        ,checked: checked
                                        ,placeholder: placeholder
                                        ,selected: selected
                                        ,accept: accept
                                        ,acceptCharset: acceptCharset
                                        ,action: action
                                        ,autocomplete: autocomplete
                                        ,autofocus: autofocus
                                        ,autosave: autosave
                                        ,disabled: disabled
                                        ,enctype: enctype
                                        ,formaction: formaction
                                        ,list: list
                                        ,maxlength: maxlength
                                        ,minlength: minlength
                                        ,method: method
                                        ,multiple: multiple
                                        ,name: name
                                        ,novalidate: novalidate
                                        ,pattern: pattern
                                        ,readonly: readonly
                                        ,required: required
                                        ,size: size
                                        ,$for: $for
                                        ,form: form
                                        ,max: max
                                        ,min: min
                                        ,step: step
                                        ,cols: cols
                                        ,rows: rows
                                        ,wrap: wrap
                                        ,href: href
                                        ,target: target
                                        ,download: download
                                        ,downloadAs: downloadAs
                                        ,hreflang: hreflang
                                        ,media: media
                                        ,ping: ping
                                        ,rel: rel
                                        ,ismap: ismap
                                        ,usemap: usemap
                                        ,shape: shape
                                        ,coords: coords
                                        ,src: src
                                        ,height: height
                                        ,width: width
                                        ,alt: alt
                                        ,autoplay: autoplay
                                        ,controls: controls
                                        ,loop: loop
                                        ,preload: preload
                                        ,poster: poster
                                        ,$default: $default
                                        ,kind: kind
                                        ,srclang: srclang
                                        ,sandbox: sandbox
                                        ,seamless: seamless
                                        ,srcdoc: srcdoc
                                        ,reversed: reversed
                                        ,start: start
                                        ,align: align
                                        ,colspan: colspan
                                        ,rowspan: rowspan
                                        ,headers: headers
                                        ,scope: scope
                                        ,async: async
                                        ,charset: charset
                                        ,content: content
                                        ,defer: defer
                                        ,httpEquiv: httpEquiv
                                        ,language: language
                                        ,scoped: scoped
                                        ,accesskey: accesskey
                                        ,contenteditable: contenteditable
                                        ,contextmenu: contextmenu
                                        ,dir: dir
                                        ,draggable: draggable
                                        ,dropzone: dropzone
                                        ,itemprop: itemprop
                                        ,lang: lang
                                        ,spellcheck: spellcheck
                                        ,tabindex: tabindex
                                        ,challenge: challenge
                                        ,keytype: keytype
                                        ,cite: cite
                                        ,datetime: datetime
                                        ,pubdate: pubdate
                                        ,manifest: manifest
                                        ,property: property
                                        ,attribute: attribute};
};
Elm.Html = Elm.Html || {};
Elm.Html.Events = Elm.Html.Events || {};
Elm.Html.Events.make = function (_elm) {
   "use strict";
   _elm.Html = _elm.Html || {};
   _elm.Html.Events = _elm.Html.Events || {};
   if (_elm.Html.Events.values) return _elm.Html.Events.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $VirtualDom = Elm.VirtualDom.make(_elm);
   var _op = {};
   var keyCode = A2($Json$Decode._op[":="],"keyCode",$Json$Decode.$int);
   var targetChecked = A2($Json$Decode.at,_U.list(["target","checked"]),$Json$Decode.bool);
   var targetValue = A2($Json$Decode.at,_U.list(["target","value"]),$Json$Decode.string);
   var defaultOptions = $VirtualDom.defaultOptions;
   var Options = F2(function (a,b) {    return {stopPropagation: a,preventDefault: b};});
   var onWithOptions = $VirtualDom.onWithOptions;
   var on = $VirtualDom.on;
   var messageOn = F3(function (name,addr,msg) {    return A3(on,name,$Json$Decode.value,function (_p0) {    return A2($Signal.message,addr,msg);});});
   var onClick = messageOn("click");
   var onDoubleClick = messageOn("dblclick");
   var onMouseMove = messageOn("mousemove");
   var onMouseDown = messageOn("mousedown");
   var onMouseUp = messageOn("mouseup");
   var onMouseEnter = messageOn("mouseenter");
   var onMouseLeave = messageOn("mouseleave");
   var onMouseOver = messageOn("mouseover");
   var onMouseOut = messageOn("mouseout");
   var onBlur = messageOn("blur");
   var onFocus = messageOn("focus");
   var onSubmit = messageOn("submit");
   var onKey = F3(function (name,addr,handler) {    return A3(on,name,keyCode,function (code) {    return A2($Signal.message,addr,handler(code));});});
   var onKeyUp = onKey("keyup");
   var onKeyDown = onKey("keydown");
   var onKeyPress = onKey("keypress");
   return _elm.Html.Events.values = {_op: _op
                                    ,onBlur: onBlur
                                    ,onFocus: onFocus
                                    ,onSubmit: onSubmit
                                    ,onKeyUp: onKeyUp
                                    ,onKeyDown: onKeyDown
                                    ,onKeyPress: onKeyPress
                                    ,onClick: onClick
                                    ,onDoubleClick: onDoubleClick
                                    ,onMouseMove: onMouseMove
                                    ,onMouseDown: onMouseDown
                                    ,onMouseUp: onMouseUp
                                    ,onMouseEnter: onMouseEnter
                                    ,onMouseLeave: onMouseLeave
                                    ,onMouseOver: onMouseOver
                                    ,onMouseOut: onMouseOut
                                    ,on: on
                                    ,onWithOptions: onWithOptions
                                    ,defaultOptions: defaultOptions
                                    ,targetValue: targetValue
                                    ,targetChecked: targetChecked
                                    ,keyCode: keyCode
                                    ,Options: Options};
};
Elm.Component = Elm.Component || {};
Elm.Component.CatalogSidebar = Elm.Component.CatalogSidebar || {};
Elm.Component.CatalogSidebar.make = function (_elm) {
   "use strict";
   _elm.Component = _elm.Component || {};
   _elm.Component.CatalogSidebar = _elm.Component.CatalogSidebar || {};
   if (_elm.Component.CatalogSidebar.values) return _elm.Component.CatalogSidebar.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Effects = Elm.Effects.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Html$Attributes = Elm.Html.Attributes.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var pkgBlockItem = function (_p0) {
      var _p1 = _p0;
      var _p2 = _p1._1;
      return A2($Html.li,
      _U.list([]),
      _U.list([A2($Html.a,
      _U.list([$Html$Attributes.href(A2($Basics._op["++"],
      "/packages/",
      A2($Basics._op["++"],_p1._0,A2($Basics._op["++"],"/",A2($Basics._op["++"],_p2,"/latest")))))]),
      _U.list([$Html.text(_p2)]))]));
   };
   var pkgBlock = F2(function (title,pkgs) {
      return A2($Html.li,_U.list([]),_U.list([$Html.text(title),A2($Html.ul,_U.list([]),A2($List.map,pkgBlockItem,pkgs))]));
   });
   _op["=>"] = F2(function (v0,v1) {    return {ctor: "_Tuple2",_0: v0,_1: v1};});
   var generalPackages = _U.list([A2(_op["=>"],"elm-lang","core")]);
   var renderingPackages = _U.list([A2(_op["=>"],"evancz","elm-html")
                                   ,A2(_op["=>"],"evancz","elm-svg")
                                   ,A2(_op["=>"],"evancz","elm-markdown")
                                   ,A2(_op["=>"],"elm-community","elm-webgl")]);
   var effectsPackages = _U.list([A2(_op["=>"],"evancz","elm-http"),A2(_op["=>"],"evancz","start-app"),A2(_op["=>"],"evancz","elm-effects")]);
   var view = F2(function (addr,model) {
      return A2($Html.div,
      _U.list([$Html$Attributes.$class("catalog-sidebar")]),
      _U.list([A2($Html.h2,_U.list([]),_U.list([$Html.text("Resources")]))
              ,A2($Html.ul,
              _U.list([]),
              _U.list([A2($Html.li,
                      _U.list([]),
                      _U.list([A2($Html.a,
                      _U.list([$Html$Attributes.href("https://github.com/elm-lang/elm-package/blob/master/README.md")]),
                      _U.list([$Html.text("Using Packages")]))]))
                      ,A2($Html.li,
                      _U.list([]),
                      _U.list([A2($Html.a,_U.list([$Html$Attributes.href("/help/design-guidelines")]),_U.list([$Html.text("API Design Guidelines")]))]))
                      ,A2($Html.li,
                      _U.list([]),
                      _U.list([A2($Html.a,_U.list([$Html$Attributes.href("/help/documentation-format")]),_U.list([$Html.text("Write great docs")]))]))
                      ,A2($Html.li,
                      _U.list([]),
                      _U.list([A2($Html.a,_U.list([$Html$Attributes.href("/help/docs-preview")]),_U.list([$Html.text("Preview your docs")]))]))
                      ,A2($Html.li,_U.list([]),_U.list([A2($Html.a,_U.list([$Html$Attributes.href("/search")]),_U.list([$Html.text("API Search")]))]))
                      ,A2($Html.li,
                      _U.list([]),
                      _U.list([A2($Html.a,_U.list([$Html$Attributes.href("http://elm-lang.org")]),_U.list([$Html.text("Elm Website")]))]))]))
              ,A2($Html.h2,_U.list([]),_U.list([$Html.text("Popular Packages")]))
              ,A2($Html.ul,
              _U.list([]),
              _U.list([A2(pkgBlock,"General",generalPackages),A2(pkgBlock,"Rendering",renderingPackages),A2(pkgBlock,"Effects",effectsPackages)]))]));
   });
   var update = F2(function (action,model) {    var _p3 = action;return {ctor: "_Tuple2",_0: model,_1: $Effects.none};});
   var Act = {ctor: "Act"};
   var init = {ctor: "_Tuple2",_0: {ctor: "_Tuple0"},_1: $Effects.none};
   return _elm.Component.CatalogSidebar.values = {_op: _op
                                                 ,init: init
                                                 ,Act: Act
                                                 ,update: update
                                                 ,view: view
                                                 ,pkgBlock: pkgBlock
                                                 ,pkgBlockItem: pkgBlockItem
                                                 ,generalPackages: generalPackages
                                                 ,renderingPackages: renderingPackages
                                                 ,effectsPackages: effectsPackages};
};
Elm.Docs = Elm.Docs || {};
Elm.Docs.Version = Elm.Docs.Version || {};
Elm.Docs.Version.make = function (_elm) {
   "use strict";
   _elm.Docs = _elm.Docs || {};
   _elm.Docs.Version = _elm.Docs.Version || {};
   if (_elm.Docs.Version.values) return _elm.Docs.Version.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm);
   var _op = {};
   var insert = F2(function (y,list) {
      var _p0 = list;
      if (_p0.ctor === "[]") {
            return _U.list([y]);
         } else {
            var _p1 = _p0._0;
            return _U.cmp(y,_p1) > 0 ? A2($List._op["::"],_p1,A2(insert,y,_p0._1)) : A2($List._op["::"],y,list);
         }
   });
   var MinorPatch = F2(function (a,b) {    return {latest: a,others: b};});
   var toDictHelp = F2(function (_p2,dict) {
      var _p3 = _p2;
      var current = {ctor: "_Tuple2",_0: _p3._1,_1: _p3._2};
      var update = function (maybeMinorPatch) {
         var _p4 = maybeMinorPatch;
         if (_p4.ctor === "Nothing") {
               return $Maybe.Just(A2(MinorPatch,current,_U.list([])));
            } else {
               var _p5 = _p4._0.latest;
               return $Maybe.Just(A2(MinorPatch,A2($Basics.max,_p5,current),A2(insert,A2($Basics.min,_p5,current),_p4._0.others)));
            }
      };
      return A3($Dict.update,_p3._0,update,dict);
   });
   var toDict = function (versions) {    return A3($List.foldl,toDictHelp,$Dict.empty,versions);};
   var toLatest = F2(function (major,_p6) {
      var _p7 = _p6;
      var _p8 = _p7.latest;
      var minor = _p8._0;
      var patch = _p8._1;
      return {ctor: "_Tuple3",_0: major,_1: minor,_2: patch};
   });
   var filterInteresting = function (versions) {    return A2($List.map,$Basics.uncurry(toLatest),$Dict.toList(toDict(versions)));};
   var vsnToString = function (_p9) {
      var _p10 = _p9;
      return A2($Basics._op["++"],
      $Basics.toString(_p10._0),
      A2($Basics._op["++"],".",A2($Basics._op["++"],$Basics.toString(_p10._1),A2($Basics._op["++"],".",$Basics.toString(_p10._2)))));
   };
   var all = function (list) {
      var _p11 = list;
      if (_p11.ctor === "[]") {
            return $Result.Ok(_U.list([]));
         } else {
            return A3($Result.map2,F2(function (x,y) {    return A2($List._op["::"],x,y);}),_p11._0,all(_p11._1));
         }
   };
   var fromString = function (str) {
      var _p12 = all(A2($List.map,$String.toInt,A2($String.split,".",str)));
      if (_p12.ctor === "Ok" && _p12._0.ctor === "::" && _p12._0._1.ctor === "::" && _p12._0._1._1.ctor === "::" && _p12._0._1._1._1.ctor === "[]") {
            return $Result.Ok({ctor: "_Tuple3",_0: _p12._0._0,_1: _p12._0._1._0,_2: _p12._0._1._1._0});
         } else {
            return $Result.Err(A2($Basics._op["++"],str," is not a valid Elm version"));
         }
   };
   var fromStringList = function (versions) {    return all(A2($List.map,fromString,versions));};
   var realMax = F2(function (rawVsn,allRawVsns) {
      var _p13 = A3($Result.map2,F2(function (v0,v1) {    return {ctor: "_Tuple2",_0: v0,_1: v1};}),fromString(rawVsn),fromStringList(allRawVsns));
      if (_p13.ctor === "Ok" && _p13._0.ctor === "_Tuple2") {
            var _p14 = _p13._0._0;
            var maxVersion = A3($List.foldl,$Basics.max,_p14,_p13._0._1);
            return _U.eq(_p14,maxVersion) ? $Maybe.Nothing : $Maybe.Just(vsnToString(maxVersion));
         } else {
            return $Maybe.Nothing;
         }
   });
   var decoder = A2($Json$Decode.customDecoder,$Json$Decode.string,fromString);
   return _elm.Docs.Version.values = {_op: _op
                                     ,decoder: decoder
                                     ,filterInteresting: filterInteresting
                                     ,realMax: realMax
                                     ,toDict: toDict
                                     ,fromStringList: fromStringList
                                     ,vsnToString: vsnToString
                                     ,MinorPatch: MinorPatch};
};
Elm.Native.Http = {};
Elm.Native.Http.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Http = localRuntime.Native.Http || {};
	if (localRuntime.Native.Http.values)
	{
		return localRuntime.Native.Http.values;
	}

	var Dict = Elm.Dict.make(localRuntime);
	var List = Elm.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);
	var Task = Elm.Native.Task.make(localRuntime);


	function send(settings, request)
	{
		return Task.asyncFunction(function(callback) {
			var req = new XMLHttpRequest();

			// start
			if (settings.onStart.ctor === 'Just')
			{
				req.addEventListener('loadStart', function() {
					var task = settings.onStart._0;
					Task.spawn(task);
				});
			}

			// progress
			if (settings.onProgress.ctor === 'Just')
			{
				req.addEventListener('progress', function(event) {
					var progress = !event.lengthComputable
						? Maybe.Nothing
						: Maybe.Just({
							_: {},
							loaded: event.loaded,
							total: event.total
						});
					var task = settings.onProgress._0(progress);
					Task.spawn(task);
				});
			}

			// end
			req.addEventListener('error', function() {
				return callback(Task.fail({ ctor: 'RawNetworkError' }));
			});

			req.addEventListener('timeout', function() {
				return callback(Task.fail({ ctor: 'RawTimeout' }));
			});

			req.addEventListener('load', function() {
				return callback(Task.succeed(toResponse(req)));
			});

			req.open(request.verb, request.url, true);

			// set all the headers
			function setHeader(pair) {
				req.setRequestHeader(pair._0, pair._1);
			}
			A2(List.map, setHeader, request.headers);

			// set the timeout
			req.timeout = settings.timeout;

			// enable this withCredentials thing
			req.withCredentials = settings.withCredentials;

			// ask for a specific MIME type for the response
			if (settings.desiredResponseType.ctor === 'Just')
			{
				req.overrideMimeType(settings.desiredResponseType._0);
			}

			// actuall send the request
			if(request.body.ctor === "BodyFormData")
			{
				req.send(request.body.formData)
			}
			else
			{
				req.send(request.body._0);
			}
		});
	}


	// deal with responses

	function toResponse(req)
	{
		var tag = req.responseType === 'blob' ? 'Blob' : 'Text'
		var response = tag === 'Blob' ? req.response : req.responseText;
		return {
			_: {},
			status: req.status,
			statusText: req.statusText,
			headers: parseHeaders(req.getAllResponseHeaders()),
			url: req.responseURL,
			value: { ctor: tag, _0: response }
		};
	}


	function parseHeaders(rawHeaders)
	{
		var headers = Dict.empty;

		if (!rawHeaders)
		{
			return headers;
		}

		var headerPairs = rawHeaders.split('\u000d\u000a');
		for (var i = headerPairs.length; i--; )
		{
			var headerPair = headerPairs[i];
			var index = headerPair.indexOf('\u003a\u0020');
			if (index > 0)
			{
				var key = headerPair.substring(0, index);
				var value = headerPair.substring(index + 2);

				headers = A3(Dict.update, key, function(oldValue) {
					if (oldValue.ctor === 'Just')
					{
						return Maybe.Just(value + ', ' + oldValue._0);
					}
					return Maybe.Just(value);
				}, headers);
			}
		}

		return headers;
	}


	function multipart(dataList)
	{
		var formData = new FormData();

		while (dataList.ctor !== '[]')
		{
			var data = dataList._0;
			if (data.ctor === 'StringData')
			{
				formData.append(data._0, data._1);
			}
			else
			{
				var fileName = data._1.ctor === 'Nothing'
					? undefined
					: data._1._0;
				formData.append(data._0, data._2, fileName);
			}
			dataList = dataList._1;
		}

		return { ctor: 'BodyFormData', formData: formData };
	}


	function uriEncode(string)
	{
		return encodeURIComponent(string);
	}

	function uriDecode(string)
	{
		return decodeURIComponent(string);
	}

	return localRuntime.Native.Http.values = {
		send: F2(send),
		multipart: multipart,
		uriEncode: uriEncode,
		uriDecode: uriDecode
	};
};

Elm.Http = Elm.Http || {};
Elm.Http.make = function (_elm) {
   "use strict";
   _elm.Http = _elm.Http || {};
   if (_elm.Http.values) return _elm.Http.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Http = Elm.Native.Http.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm),
   $Task = Elm.Task.make(_elm),
   $Time = Elm.Time.make(_elm);
   var _op = {};
   var send = $Native$Http.send;
   var BadResponse = F2(function (a,b) {    return {ctor: "BadResponse",_0: a,_1: b};});
   var UnexpectedPayload = function (a) {    return {ctor: "UnexpectedPayload",_0: a};};
   var handleResponse = F2(function (handle,response) {
      if (_U.cmp(200,response.status) < 1 && _U.cmp(response.status,300) < 0) {
            var _p0 = response.value;
            if (_p0.ctor === "Text") {
                  return handle(_p0._0);
               } else {
                  return $Task.fail(UnexpectedPayload("Response body is a blob, expecting a string."));
               }
         } else return $Task.fail(A2(BadResponse,response.status,response.statusText));
   });
   var NetworkError = {ctor: "NetworkError"};
   var Timeout = {ctor: "Timeout"};
   var promoteError = function (rawError) {    var _p1 = rawError;if (_p1.ctor === "RawTimeout") {    return Timeout;} else {    return NetworkError;}};
   var fromJson = F2(function (decoder,response) {
      var decode = function (str) {
         var _p2 = A2($Json$Decode.decodeString,decoder,str);
         if (_p2.ctor === "Ok") {
               return $Task.succeed(_p2._0);
            } else {
               return $Task.fail(UnexpectedPayload(_p2._0));
            }
      };
      return A2($Task.andThen,A2($Task.mapError,promoteError,response),handleResponse(decode));
   });
   var RawNetworkError = {ctor: "RawNetworkError"};
   var RawTimeout = {ctor: "RawTimeout"};
   var Blob = function (a) {    return {ctor: "Blob",_0: a};};
   var Text = function (a) {    return {ctor: "Text",_0: a};};
   var Response = F5(function (a,b,c,d,e) {    return {status: a,statusText: b,headers: c,url: d,value: e};});
   var defaultSettings = {timeout: 0,onStart: $Maybe.Nothing,onProgress: $Maybe.Nothing,desiredResponseType: $Maybe.Nothing,withCredentials: false};
   var post = F3(function (decoder,url,body) {
      var request = {verb: "POST",headers: _U.list([]),url: url,body: body};
      return A2(fromJson,decoder,A2(send,defaultSettings,request));
   });
   var Settings = F5(function (a,b,c,d,e) {    return {timeout: a,onStart: b,onProgress: c,desiredResponseType: d,withCredentials: e};});
   var multipart = $Native$Http.multipart;
   var FileData = F3(function (a,b,c) {    return {ctor: "FileData",_0: a,_1: b,_2: c};});
   var BlobData = F3(function (a,b,c) {    return {ctor: "BlobData",_0: a,_1: b,_2: c};});
   var blobData = BlobData;
   var StringData = F2(function (a,b) {    return {ctor: "StringData",_0: a,_1: b};});
   var stringData = StringData;
   var BodyBlob = function (a) {    return {ctor: "BodyBlob",_0: a};};
   var BodyFormData = {ctor: "BodyFormData"};
   var ArrayBuffer = {ctor: "ArrayBuffer"};
   var BodyString = function (a) {    return {ctor: "BodyString",_0: a};};
   var string = BodyString;
   var Empty = {ctor: "Empty"};
   var empty = Empty;
   var getString = function (url) {
      var request = {verb: "GET",headers: _U.list([]),url: url,body: empty};
      return A2($Task.andThen,A2($Task.mapError,promoteError,A2(send,defaultSettings,request)),handleResponse($Task.succeed));
   };
   var get = F2(function (decoder,url) {
      var request = {verb: "GET",headers: _U.list([]),url: url,body: empty};
      return A2(fromJson,decoder,A2(send,defaultSettings,request));
   });
   var Request = F4(function (a,b,c,d) {    return {verb: a,headers: b,url: c,body: d};});
   var uriDecode = $Native$Http.uriDecode;
   var uriEncode = $Native$Http.uriEncode;
   var queryEscape = function (string) {    return A2($String.join,"+",A2($String.split,"%20",uriEncode(string)));};
   var queryPair = function (_p3) {    var _p4 = _p3;return A2($Basics._op["++"],queryEscape(_p4._0),A2($Basics._op["++"],"=",queryEscape(_p4._1)));};
   var url = F2(function (baseUrl,args) {
      var _p5 = args;
      if (_p5.ctor === "[]") {
            return baseUrl;
         } else {
            return A2($Basics._op["++"],baseUrl,A2($Basics._op["++"],"?",A2($String.join,"&",A2($List.map,queryPair,args))));
         }
   });
   var TODO_implement_file_in_another_library = {ctor: "TODO_implement_file_in_another_library"};
   var TODO_implement_blob_in_another_library = {ctor: "TODO_implement_blob_in_another_library"};
   return _elm.Http.values = {_op: _op
                             ,getString: getString
                             ,get: get
                             ,post: post
                             ,send: send
                             ,url: url
                             ,uriEncode: uriEncode
                             ,uriDecode: uriDecode
                             ,empty: empty
                             ,string: string
                             ,multipart: multipart
                             ,stringData: stringData
                             ,defaultSettings: defaultSettings
                             ,fromJson: fromJson
                             ,Request: Request
                             ,Settings: Settings
                             ,Response: Response
                             ,Text: Text
                             ,Blob: Blob
                             ,Timeout: Timeout
                             ,NetworkError: NetworkError
                             ,UnexpectedPayload: UnexpectedPayload
                             ,BadResponse: BadResponse
                             ,RawTimeout: RawTimeout
                             ,RawNetworkError: RawNetworkError};
};
Elm.Docs = Elm.Docs || {};
Elm.Docs.Name = Elm.Docs.Name || {};
Elm.Docs.Name.make = function (_elm) {
   "use strict";
   _elm.Docs = _elm.Docs || {};
   _elm.Docs.Name = _elm.Docs.Name || {};
   if (_elm.Docs.Name.values) return _elm.Docs.Name.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Html$Attributes = Elm.Html.Attributes.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Set = Elm.Set.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm);
   var _op = {};
   var pathTo = function (_p0) {
      var _p1 = _p0;
      return A2($Basics._op["++"],
      A2($String.map,function (c) {    return _U.eq(c,_U.chr(".")) ? _U.chr("-") : c;},_p1.home),
      A2($Basics._op["++"],"#",_p1.name));
   };
   var basePathTo = F2(function (basePath,_p2) {    var _p3 = _p2;return A2($Basics._op["++"],basePath,A2($Basics._op["++"],"/",pathTo(_p3)));});
   var toBaseLink = F3(function (basePath,dict,_p4) {
      var _p5 = _p4;
      var _p7 = _p5.name;
      var _p6 = A2($Maybe.map,$Set.member(_p7),A2($Dict.get,_p5.home,dict));
      if (_p6.ctor === "Just" && _p6._0 === true) {
            var link = A2(basePathTo,basePath,_p5);
            return A2($Html.a,_U.list([$Html$Attributes.target("_blank"),$Html$Attributes.href(link)]),_U.list([$Html.text(_p7)]));
         } else {
            return $Html.text(_p7);
         }
   });
   var toLink = F2(function (dict,_p8) {
      var _p9 = _p8;
      var _p11 = _p9.name;
      var _p10 = A2($Maybe.map,$Set.member(_p11),A2($Dict.get,_p9.home,dict));
      if (_p10.ctor === "Just" && _p10._0 === true) {
            var link = pathTo(_p9);
            return A2($Html.a,_U.list([$Html$Attributes.href(link)]),_U.list([$Html.text(_p11)]));
         } else {
            return $Html.text(_p11);
         }
   });
   var Canonical = F2(function (a,b) {    return {home: a,name: b};});
   return _elm.Docs.Name.values = {_op: _op,Canonical: Canonical,pathTo: pathTo,basePathTo: basePathTo,toLink: toLink,toBaseLink: toBaseLink};
};
Elm.Utils = Elm.Utils || {};
Elm.Utils.Code = Elm.Utils.Code || {};
Elm.Utils.Code.make = function (_elm) {
   "use strict";
   _elm.Utils = _elm.Utils || {};
   _elm.Utils.Code = _elm.Utils.Code || {};
   if (_elm.Utils.Code.values) return _elm.Utils.Code.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Html$Attributes = Elm.Html.Attributes.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var equals = A2($Html.span,_U.list([]),_U.list([$Html.text("=")]));
   var colon = A2($Html.span,_U.list([]),_U.list([$Html.text(":")]));
   var arrow = A2($Html.span,_U.list([]),_U.list([$Html.text("->")]));
   var space = $Html.text(" ");
   var padded = function (html) {    return _U.list([space,html,space]);};
   var addParens = function (list) {    return A2($List._op["::"],$Html.text("("),A2($Basics._op["++"],list,_U.list([$Html.text(")")])));};
   var keyword = function (kw) {    return A2($Html.span,_U.list([$Html$Attributes.$class("hljs-keyword")]),_U.list([$Html.text(kw)]));};
   return _elm.Utils.Code.values = {_op: _op,keyword: keyword,addParens: addParens,space: space,padded: padded,arrow: arrow,colon: colon,equals: equals};
};
Elm.Docs = Elm.Docs || {};
Elm.Docs.Type = Elm.Docs.Type || {};
Elm.Docs.Type.make = function (_elm) {
   "use strict";
   _elm.Docs = _elm.Docs || {};
   _elm.Docs.Type = _elm.Docs.Type || {};
   if (_elm.Docs.Type.values) return _elm.Docs.Type.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Char = Elm.Char.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Docs$Name = Elm.Docs.Name.make(_elm),
   $Html = Elm.Html.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm),
   $Utils$Code = Elm.Utils.Code.make(_elm);
   var _op = {};
   var defaultMapping = A3($Dict.insert,"appendable","appendable",A3($Dict.insert,"comparable","comparable",A3($Dict.insert,"number","number",$Dict.empty)));
   var nextMappingValue = function (mapping) {
      var base = $Dict.size(mapping) - $Dict.size(defaultMapping);
      var code = A2($Basics._op["%"],base,26) + $Char.toCode(_U.chr("a"));
      var string = $String.fromChar($Char.fromCode(code));
      var times = (base / 26 | 0) + 1;
      return A2($String.repeat,times,string);
   };
   var updateMapping = F2(function (tipe,mapping) {
      var updateMappingFor = function (name) {    return A2($Dict.member,name,mapping) ? mapping : A3($Dict.insert,name,nextMappingValue(mapping),mapping);};
      var _p0 = tipe;
      switch (_p0.ctor)
      {case "Function": return A3($List.foldl,updateMapping,mapping,A2($List.append,_p0._0,_U.list([_p0._1])));
         case "Var": return updateMappingFor(_p0._0);
         case "Apply": return A3($List.foldl,updateMapping,mapping,_p0._1);
         case "Tuple": return A3($List.foldl,updateMapping,mapping,_p0._0);
         default: return A3($List.foldl,updateMapping,mapping,A2($List.map,function (_p1) {    var _p2 = _p1;return _p2._1;},_p0._0));}
   });
   var compareNamesWithPenalty = F3(function (penalty,nameA,nameB) {    return _U.eq(nameA,nameB) ? 0 : A2($String.contains,nameA,nameB) ? 1 : penalty;});
   var compareVarsWithPenalty = F3(function (penalty,nameA,nameB) {    return _U.eq(nameA,nameB) ? 0 : penalty;});
   var distance = F2(function (needle,hay) {
      var defaultPenalty = 10;
      var argsLengthPenalty = F2(function (needleList,hayList) {
         var argsLengthDiff = $List.length(needleList) - $List.length(hayList);
         return _U.cmp(argsLengthDiff,0) > 0 ? argsLengthDiff * defaultPenalty : _U.cmp(argsLengthDiff,0) < 0 ? argsLengthDiff * -1 : 0;
      });
      var _p3 = {ctor: "_Tuple2",_0: needle,_1: hay};
      _v2_4: do {
         if (_p3.ctor === "_Tuple2") {
               switch (_p3._0.ctor)
               {case "Function": if (_p3._1.ctor === "Function") {
                          var _p5 = _p3._0._0;
                          var _p4 = _p3._1._0;
                          return $List.sum(A3($List.map2,
                          distance,
                          A2($List.append,_p5,_U.list([_p3._0._1])),
                          A2($List.append,_p4,_U.list([_p3._1._1])))) + A2(argsLengthPenalty,_p5,_p4);
                       } else {
                          break _v2_4;
                       }
                  case "Var": if (_p3._1.ctor === "Var") {
                          return A3(compareVarsWithPenalty,defaultPenalty,_p3._0._0,_p3._1._0);
                       } else {
                          break _v2_4;
                       }
                  case "Apply": if (_p3._1.ctor === "Apply") {
                          var _p9 = _p3._0._0;
                          var _p8 = _p3._1._0;
                          var _p7 = _p3._0._1;
                          var _p6 = _p3._1._1;
                          return A3(compareNamesWithPenalty,2,_p9.home,_p8.home) + A3(compareNamesWithPenalty,2,_p9.name,_p8.name) + $List.sum(A3($List.map2,
                          distance,
                          _p7,
                          _p6)) + A2(argsLengthPenalty,_p7,_p6);
                       } else {
                          break _v2_4;
                       }
                  case "Tuple": if (_p3._1.ctor === "Tuple") {
                          return $List.sum(A3($List.map2,distance,_p3._0._0,_p3._1._0));
                       } else {
                          break _v2_4;
                       }
                  default: break _v2_4;}
            } else {
               break _v2_4;
            }
      } while (false);
      return 100;
   });
   var Other = {ctor: "Other"};
   var App = {ctor: "App"};
   var Func = {ctor: "Func"};
   var toHtml = F3(function (nameDict,context,tipe) {
      var go = F2(function (ctx,t) {    return A3(toHtml,nameDict,ctx,t);});
      var _p10 = tipe;
      switch (_p10.ctor)
      {case "Function": var argsHtml = A2($List.concatMap,
           function (arg) {
              return A2($Basics._op["++"],A2(go,Func,arg),$Utils$Code.padded($Utils$Code.arrow));
           },
           _p10._0);
           var maybeAddParens = function () {
              var _p11 = context;
              switch (_p11.ctor)
              {case "Func": return $Utils$Code.addParens;
                 case "App": return $Utils$Code.addParens;
                 default: return $Basics.identity;}
           }();
           return maybeAddParens(A2($Basics._op["++"],argsHtml,A2(go,Func,_p10._1)));
         case "Var": return _U.list([$Html.text(_p10._0)]);
         case "Apply": if (_p10._1.ctor === "[]") {
                 return _U.list([A2($Docs$Name.toLink,nameDict,_p10._0)]);
              } else {
                 var argsHtml = A2($List.concatMap,function (arg) {    return A2($List._op["::"],$Utils$Code.space,A2(go,App,arg));},_p10._1);
                 var maybeAddParens = function () {
                    var _p12 = context;
                    switch (_p12.ctor)
                    {case "Func": return $Basics.identity;
                       case "App": return $Utils$Code.addParens;
                       default: return $Basics.identity;}
                 }();
                 return maybeAddParens(A2($List._op["::"],A2($Docs$Name.toLink,nameDict,_p10._0),argsHtml));
              }
         case "Tuple": return $Utils$Code.addParens($List.concat(A2($List.intersperse,_U.list([$Html.text(", ")]),A2($List.map,go(Other),_p10._0))));
         default: var fieldsHtml = $List.concat(A2($List.intersperse,_U.list([$Html.text(", ")]),A2($List.map,fieldToHtml(nameDict),_p10._0)));
           var recordInsides = function () {
              var _p13 = _p10._1;
              if (_p13.ctor === "Nothing") {
                    return fieldsHtml;
                 } else {
                    return A2($List._op["::"],$Html.text(_p13._0),A2($List._op["::"],$Html.text(" | "),fieldsHtml));
                 }
           }();
           return A2($List._op["::"],$Html.text("{ "),A2($Basics._op["++"],recordInsides,_U.list([$Html.text(" }")])));}
   });
   var fieldToHtml = F2(function (nameDict,_p14) {
      var _p15 = _p14;
      return A2($List._op["::"],
      $Html.text(_p15._0),
      A2($List._op["::"],$Utils$Code.space,A2($List._op["::"],$Utils$Code.colon,A2($List._op["::"],$Utils$Code.space,A3(toHtml,nameDict,Other,_p15._1)))));
   });
   var toHtmlWithBasePath = F4(function (basePath,nameDict,context,tipe) {
      var go = F2(function (ctx,t) {    return A4(toHtmlWithBasePath,basePath,nameDict,ctx,t);});
      var _p16 = tipe;
      switch (_p16.ctor)
      {case "Function": var argsHtml = A2($List.concatMap,
           function (arg) {
              return A2($Basics._op["++"],A2(go,Func,arg),$Utils$Code.padded($Utils$Code.arrow));
           },
           _p16._0);
           var maybeAddParens = function () {
              var _p17 = context;
              switch (_p17.ctor)
              {case "Func": return $Utils$Code.addParens;
                 case "App": return $Utils$Code.addParens;
                 default: return $Basics.identity;}
           }();
           return maybeAddParens(A2($Basics._op["++"],argsHtml,A2(go,Func,_p16._1)));
         case "Var": return _U.list([$Html.text(_p16._0)]);
         case "Apply": if (_p16._1.ctor === "[]") {
                 return _U.list([A3($Docs$Name.toBaseLink,basePath,nameDict,_p16._0)]);
              } else {
                 var argsHtml = A2($List.concatMap,function (arg) {    return A2($List._op["::"],$Utils$Code.space,A2(go,App,arg));},_p16._1);
                 var maybeAddParens = function () {
                    var _p18 = context;
                    switch (_p18.ctor)
                    {case "Func": return $Basics.identity;
                       case "App": return $Utils$Code.addParens;
                       default: return $Basics.identity;}
                 }();
                 return maybeAddParens(A2($List._op["::"],A3($Docs$Name.toBaseLink,basePath,nameDict,_p16._0),argsHtml));
              }
         case "Tuple": return $Utils$Code.addParens($List.concat(A2($List.intersperse,_U.list([$Html.text(", ")]),A2($List.map,go(Other),_p16._0))));
         default: var fieldsHtml = $List.concat(A2($List.intersperse,_U.list([$Html.text(", ")]),A2($List.map,fieldToHtml(nameDict),_p16._0)));
           var recordInsides = function () {
              var _p19 = _p16._1;
              if (_p19.ctor === "Nothing") {
                    return fieldsHtml;
                 } else {
                    return A2($List._op["::"],$Html.text(_p19._0),A2($List._op["::"],$Html.text(" | "),fieldsHtml));
                 }
           }();
           return A2($List._op["::"],$Html.text("{ "),A2($Basics._op["++"],recordInsides,_U.list([$Html.text(" }")])));}
   });
   var length = F2(function (context,tipe) {
      var _p20 = tipe;
      switch (_p20.ctor)
      {case "Function": var argLengths = A2($List.map,function (t) {    return 4 + A2(length,Func,t);},_p20._0);
           var parens = function () {    var _p21 = context;switch (_p21.ctor) {case "Func": return 2;case "App": return 2;default: return 0;}}();
           return parens + $List.sum(argLengths) + A2(length,Func,_p20._1);
         case "Var": return $String.length(_p20._0);
         case "Apply": if (_p20._1.ctor === "[]") {
                 return $String.length(_p20._0.name);
              } else {
                 var argsLength = $List.sum(A2($List.map,function (t) {    return 1 + A2(length,App,t);},_p20._1));
                 var parens = function () {    var _p22 = context;switch (_p22.ctor) {case "Func": return 0;case "App": return 2;default: return 0;}}();
                 return parens + $String.length(_p20._0.name) + argsLength;
              }
         case "Tuple": return $List.sum(A2($List.map,function (t) {    return 2 + A2(length,Other,t);},_p20._0));
         default: var extLength = function () {
              var _p23 = _p20._1;
              if (_p23.ctor === "Nothing") {
                    return 0;
                 } else {
                    return 2 + $String.length(_p23._0);
                 }
           }();
           var fieldLength = function (_p24) {    var _p25 = _p24;return $String.length(_p25._0) + 3 + A2(length,Other,_p25._1);};
           var recordLength = 2 + $List.sum(A2($List.map,function (ft) {    return 2 + fieldLength(ft);},_p20._0));
           return recordLength + extLength;}
   });
   var Tag = F2(function (a,b) {    return {tag: a,args: b};});
   var Record = F2(function (a,b) {    return {ctor: "Record",_0: a,_1: b};});
   var Tuple = function (a) {    return {ctor: "Tuple",_0: a};};
   var Apply = F2(function (a,b) {    return {ctor: "Apply",_0: a,_1: b};});
   var Var = function (a) {    return {ctor: "Var",_0: a};};
   var Function = F2(function (a,b) {    return {ctor: "Function",_0: a,_1: b};});
   var normalizeWithMapping = F2(function (mapping,tipe) {
      var normalize$ = normalizeWithMapping(mapping);
      var _p26 = tipe;
      switch (_p26.ctor)
      {case "Function": return A2(Function,A2($List.map,normalize$,_p26._0),normalize$(_p26._1));
         case "Var": var _p28 = _p26._0;
           var name$ = function () {    var _p27 = A2($Dict.get,_p28,mapping);if (_p27.ctor === "Just") {    return _p27._0;} else {    return _p28;}}();
           return Var(name$);
         case "Apply": return A2(Apply,_p26._0,A2($List.map,normalize$,_p26._1));
         case "Tuple": return Tuple(A2($List.map,normalize$,_p26._0));
         default: return A2(Record,
           A2($List.map,function (_p29) {    var _p30 = _p29;return {ctor: "_Tuple2",_0: _p30._0,_1: normalize$(_p30._1)};},_p26._0),
           _p26._1);}
   });
   var normalize = function (tipe) {    return A2(normalizeWithMapping,A2(updateMapping,tipe,defaultMapping),tipe);};
   return _elm.Docs.Type.values = {_op: _op
                                  ,Function: Function
                                  ,Var: Var
                                  ,Apply: Apply
                                  ,Tuple: Tuple
                                  ,Record: Record
                                  ,Tag: Tag
                                  ,Func: Func
                                  ,App: App
                                  ,Other: Other
                                  ,toHtml: toHtml
                                  ,toHtmlWithBasePath: toHtmlWithBasePath
                                  ,fieldToHtml: fieldToHtml
                                  ,length: length
                                  ,distance: distance
                                  ,compareVarsWithPenalty: compareVarsWithPenalty
                                  ,compareNamesWithPenalty: compareNamesWithPenalty
                                  ,defaultMapping: defaultMapping
                                  ,nextMappingValue: nextMappingValue
                                  ,updateMapping: updateMapping
                                  ,normalize: normalize
                                  ,normalizeWithMapping: normalizeWithMapping};
};

// setup
Elm.Native = Elm.Native || {};
Elm.Native.Markdown = Elm.Native.Markdown || {};

// definition
Elm.Native.Markdown.make = function(localRuntime) {
	'use strict';

	// attempt to short-circuit
	if ('values' in Elm.Native.Markdown)
	{
		return Elm.Native.Markdown.values;
	}

	var Element = Elm.Native.Graphics.Element.make(localRuntime);

	var marked = (function() {
		// catch the `marked` object regardless of the outer environment.
		// (ex. a CommonJS module compatible environment.)
		// note that this depends on marked's implementation of environment detection.
		var module = {};
		var exports = module.exports = {};

		/**
		 * marked - a markdown parser
		 * Copyright (c) 2011-2014, Christopher Jeffrey. (MIT Licensed)
		 * https://github.com/chjj/marked
		 */
		(function(){var block={newline:/^\n+/,code:/^( {4}[^\n]+\n*)+/,fences:noop,hr:/^( *[-*_]){3,} *(?:\n+|$)/,heading:/^ *(#{1,6}) *([^\n]+?) *#* *(?:\n+|$)/,nptable:noop,lheading:/^([^\n]+)\n *(=|-){2,} *(?:\n+|$)/,blockquote:/^( *>[^\n]+(\n(?!def)[^\n]+)*\n*)+/,list:/^( *)(bull) [\s\S]+?(?:hr|def|\n{2,}(?! )(?!\1bull )\n*|\s*$)/,html:/^ *(?:comment|closed|closing) *(?:\n{2,}|\s*$)/,def:/^ *\[([^\]]+)\]: *<?([^\s>]+)>?(?: +["(]([^\n]+)[")])? *(?:\n+|$)/,table:noop,paragraph:/^((?:[^\n]+\n?(?!hr|heading|lheading|blockquote|tag|def))+)\n*/,text:/^[^\n]+/};block.bullet=/(?:[*+-]|\d+\.)/;block.item=/^( *)(bull) [^\n]*(?:\n(?!\1bull )[^\n]*)*/;block.item=replace(block.item,"gm")(/bull/g,block.bullet)();block.list=replace(block.list)(/bull/g,block.bullet)("hr","\\n+(?=\\1?(?:[-*_] *){3,}(?:\\n+|$))")("def","\\n+(?="+block.def.source+")")();block.blockquote=replace(block.blockquote)("def",block.def)();block._tag="(?!(?:"+"a|em|strong|small|s|cite|q|dfn|abbr|data|time|code"+"|var|samp|kbd|sub|sup|i|b|u|mark|ruby|rt|rp|bdi|bdo"+"|span|br|wbr|ins|del|img)\\b)\\w+(?!:/|[^\\w\\s@]*@)\\b";block.html=replace(block.html)("comment",/<!--[\s\S]*?-->/)("closed",/<(tag)[\s\S]+?<\/\1>/)("closing",/<tag(?:"[^"]*"|'[^']*'|[^'">])*?>/)(/tag/g,block._tag)();block.paragraph=replace(block.paragraph)("hr",block.hr)("heading",block.heading)("lheading",block.lheading)("blockquote",block.blockquote)("tag","<"+block._tag)("def",block.def)();block.normal=merge({},block);block.gfm=merge({},block.normal,{fences:/^ *(`{3,}|~{3,}) *(\S+)? *\n([\s\S]+?)\s*\1 *(?:\n+|$)/,paragraph:/^/});block.gfm.paragraph=replace(block.paragraph)("(?!","(?!"+block.gfm.fences.source.replace("\\1","\\2")+"|"+block.list.source.replace("\\1","\\3")+"|")();block.tables=merge({},block.gfm,{nptable:/^ *(\S.*\|.*)\n *([-:]+ *\|[-| :]*)\n((?:.*\|.*(?:\n|$))*)\n*/,table:/^ *\|(.+)\n *\|( *[-:]+[-| :]*)\n((?: *\|.*(?:\n|$))*)\n*/});function Lexer(options){this.tokens=[];this.tokens.links={};this.options=options||marked.defaults;this.rules=block.normal;if(this.options.gfm){if(this.options.tables){this.rules=block.tables}else{this.rules=block.gfm}}}Lexer.rules=block;Lexer.lex=function(src,options){var lexer=new Lexer(options);return lexer.lex(src)};Lexer.prototype.lex=function(src){src=src.replace(/\r\n|\r/g,"\n").replace(/\t/g,"    ").replace(/\u00a0/g," ").replace(/\u2424/g,"\n");return this.token(src,true)};Lexer.prototype.token=function(src,top,bq){var src=src.replace(/^ +$/gm,""),next,loose,cap,bull,b,item,space,i,l;while(src){if(cap=this.rules.newline.exec(src)){src=src.substring(cap[0].length);if(cap[0].length>1){this.tokens.push({type:"space"})}}if(cap=this.rules.code.exec(src)){src=src.substring(cap[0].length);cap=cap[0].replace(/^ {4}/gm,"");this.tokens.push({type:"code",text:!this.options.pedantic?cap.replace(/\n+$/,""):cap});continue}if(cap=this.rules.fences.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"code",lang:cap[2],text:cap[3]});continue}if(cap=this.rules.heading.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"heading",depth:cap[1].length,text:cap[2]});continue}if(top&&(cap=this.rules.nptable.exec(src))){src=src.substring(cap[0].length);item={type:"table",header:cap[1].replace(/^ *| *\| *$/g,"").split(/ *\| */),align:cap[2].replace(/^ *|\| *$/g,"").split(/ *\| */),cells:cap[3].replace(/\n$/,"").split("\n")};for(i=0;i<item.align.length;i++){if(/^ *-+: *$/.test(item.align[i])){item.align[i]="right"}else if(/^ *:-+: *$/.test(item.align[i])){item.align[i]="center"}else if(/^ *:-+ *$/.test(item.align[i])){item.align[i]="left"}else{item.align[i]=null}}for(i=0;i<item.cells.length;i++){item.cells[i]=item.cells[i].split(/ *\| */)}this.tokens.push(item);continue}if(cap=this.rules.lheading.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"heading",depth:cap[2]==="="?1:2,text:cap[1]});continue}if(cap=this.rules.hr.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"hr"});continue}if(cap=this.rules.blockquote.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"blockquote_start"});cap=cap[0].replace(/^ *> ?/gm,"");this.token(cap,top,true);this.tokens.push({type:"blockquote_end"});continue}if(cap=this.rules.list.exec(src)){src=src.substring(cap[0].length);bull=cap[2];this.tokens.push({type:"list_start",ordered:bull.length>1});cap=cap[0].match(this.rules.item);next=false;l=cap.length;i=0;for(;i<l;i++){item=cap[i];space=item.length;item=item.replace(/^ *([*+-]|\d+\.) +/,"");if(~item.indexOf("\n ")){space-=item.length;item=!this.options.pedantic?item.replace(new RegExp("^ {1,"+space+"}","gm"),""):item.replace(/^ {1,4}/gm,"")}if(this.options.smartLists&&i!==l-1){b=block.bullet.exec(cap[i+1])[0];if(bull!==b&&!(bull.length>1&&b.length>1)){src=cap.slice(i+1).join("\n")+src;i=l-1}}loose=next||/\n\n(?!\s*$)/.test(item);if(i!==l-1){next=item.charAt(item.length-1)==="\n";if(!loose)loose=next}this.tokens.push({type:loose?"loose_item_start":"list_item_start"});this.token(item,false,bq);this.tokens.push({type:"list_item_end"})}this.tokens.push({type:"list_end"});continue}if(cap=this.rules.html.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:this.options.sanitize?"paragraph":"html",pre:cap[1]==="pre"||cap[1]==="script"||cap[1]==="style",text:cap[0]});continue}if(!bq&&top&&(cap=this.rules.def.exec(src))){src=src.substring(cap[0].length);this.tokens.links[cap[1].toLowerCase()]={href:cap[2],title:cap[3]};continue}if(top&&(cap=this.rules.table.exec(src))){src=src.substring(cap[0].length);item={type:"table",header:cap[1].replace(/^ *| *\| *$/g,"").split(/ *\| */),align:cap[2].replace(/^ *|\| *$/g,"").split(/ *\| */),cells:cap[3].replace(/(?: *\| *)?\n$/,"").split("\n")};for(i=0;i<item.align.length;i++){if(/^ *-+: *$/.test(item.align[i])){item.align[i]="right"}else if(/^ *:-+: *$/.test(item.align[i])){item.align[i]="center"}else if(/^ *:-+ *$/.test(item.align[i])){item.align[i]="left"}else{item.align[i]=null}}for(i=0;i<item.cells.length;i++){item.cells[i]=item.cells[i].replace(/^ *\| *| *\| *$/g,"").split(/ *\| */)}this.tokens.push(item);continue}if(top&&(cap=this.rules.paragraph.exec(src))){src=src.substring(cap[0].length);this.tokens.push({type:"paragraph",text:cap[1].charAt(cap[1].length-1)==="\n"?cap[1].slice(0,-1):cap[1]});continue}if(cap=this.rules.text.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"text",text:cap[0]});continue}if(src){throw new Error("Infinite loop on byte: "+src.charCodeAt(0))}}return this.tokens};var inline={escape:/^\\([\\`*{}\[\]()#+\-.!_>])/,autolink:/^<([^ >]+(@|:\/)[^ >]+)>/,url:noop,tag:/^<!--[\s\S]*?-->|^<\/?\w+(?:"[^"]*"|'[^']*'|[^'">])*?>/,link:/^!?\[(inside)\]\(href\)/,reflink:/^!?\[(inside)\]\s*\[([^\]]*)\]/,nolink:/^!?\[((?:\[[^\]]*\]|[^\[\]])*)\]/,strong:/^__([\s\S]+?)__(?!_)|^\*\*([\s\S]+?)\*\*(?!\*)/,em:/^\b_((?:__|[\s\S])+?)_\b|^\*((?:\*\*|[\s\S])+?)\*(?!\*)/,code:/^(`+)\s*([\s\S]*?[^`])\s*\1(?!`)/,br:/^ {2,}\n(?!\s*$)/,del:noop,text:/^[\s\S]+?(?=[\\<!\[_*`]| {2,}\n|$)/};inline._inside=/(?:\[[^\]]*\]|[^\[\]]|\](?=[^\[]*\]))*/;inline._href=/\s*<?([\s\S]*?)>?(?:\s+['"]([\s\S]*?)['"])?\s*/;inline.link=replace(inline.link)("inside",inline._inside)("href",inline._href)();inline.reflink=replace(inline.reflink)("inside",inline._inside)();inline.normal=merge({},inline);inline.pedantic=merge({},inline.normal,{strong:/^__(?=\S)([\s\S]*?\S)__(?!_)|^\*\*(?=\S)([\s\S]*?\S)\*\*(?!\*)/,em:/^_(?=\S)([\s\S]*?\S)_(?!_)|^\*(?=\S)([\s\S]*?\S)\*(?!\*)/});inline.gfm=merge({},inline.normal,{escape:replace(inline.escape)("])","~|])")(),url:/^(https?:\/\/[^\s<]+[^<.,:;"')\]\s])/,del:/^~~(?=\S)([\s\S]*?\S)~~/,text:replace(inline.text)("]|","~]|")("|","|https?://|")()});inline.breaks=merge({},inline.gfm,{br:replace(inline.br)("{2,}","*")(),text:replace(inline.gfm.text)("{2,}","*")()});function InlineLexer(links,options){this.options=options||marked.defaults;this.links=links;this.rules=inline.normal;this.renderer=this.options.renderer||new Renderer;this.renderer.options=this.options;if(!this.links){throw new Error("Tokens array requires a `links` property.")}if(this.options.gfm){if(this.options.breaks){this.rules=inline.breaks}else{this.rules=inline.gfm}}else if(this.options.pedantic){this.rules=inline.pedantic}}InlineLexer.rules=inline;InlineLexer.output=function(src,links,options){var inline=new InlineLexer(links,options);return inline.output(src)};InlineLexer.prototype.output=function(src){var out="",link,text,href,cap;while(src){if(cap=this.rules.escape.exec(src)){src=src.substring(cap[0].length);out+=cap[1];continue}if(cap=this.rules.autolink.exec(src)){src=src.substring(cap[0].length);if(cap[2]==="@"){text=cap[1].charAt(6)===":"?this.mangle(cap[1].substring(7)):this.mangle(cap[1]);href=this.mangle("mailto:")+text}else{text=escape(cap[1]);href=text}out+=this.renderer.link(href,null,text);continue}if(!this.inLink&&(cap=this.rules.url.exec(src))){src=src.substring(cap[0].length);text=escape(cap[1]);href=text;out+=this.renderer.link(href,null,text);continue}if(cap=this.rules.tag.exec(src)){if(!this.inLink&&/^<a /i.test(cap[0])){this.inLink=true}else if(this.inLink&&/^<\/a>/i.test(cap[0])){this.inLink=false}src=src.substring(cap[0].length);out+=this.options.sanitize?escape(cap[0]):cap[0];continue}if(cap=this.rules.link.exec(src)){src=src.substring(cap[0].length);this.inLink=true;out+=this.outputLink(cap,{href:cap[2],title:cap[3]});this.inLink=false;continue}if((cap=this.rules.reflink.exec(src))||(cap=this.rules.nolink.exec(src))){src=src.substring(cap[0].length);link=(cap[2]||cap[1]).replace(/\s+/g," ");link=this.links[link.toLowerCase()];if(!link||!link.href){out+=cap[0].charAt(0);src=cap[0].substring(1)+src;continue}this.inLink=true;out+=this.outputLink(cap,link);this.inLink=false;continue}if(cap=this.rules.strong.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.strong(this.output(cap[2]||cap[1]));continue}if(cap=this.rules.em.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.em(this.output(cap[2]||cap[1]));continue}if(cap=this.rules.code.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.codespan(escape(cap[2],true));continue}if(cap=this.rules.br.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.br();continue}if(cap=this.rules.del.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.del(this.output(cap[1]));continue}if(cap=this.rules.text.exec(src)){src=src.substring(cap[0].length);out+=escape(this.smartypants(cap[0]));continue}if(src){throw new Error("Infinite loop on byte: "+src.charCodeAt(0))}}return out};InlineLexer.prototype.outputLink=function(cap,link){var href=escape(link.href),title=link.title?escape(link.title):null;return cap[0].charAt(0)!=="!"?this.renderer.link(href,title,this.output(cap[1])):this.renderer.image(href,title,escape(cap[1]))};InlineLexer.prototype.smartypants=function(text){if(!this.options.smartypants)return text;return text.replace(/--/g,"—").replace(/(^|[-\u2014/(\[{"\s])'/g,"$1‘").replace(/'/g,"’").replace(/(^|[-\u2014/(\[{\u2018\s])"/g,"$1“").replace(/"/g,"”").replace(/\.{3}/g,"…")};InlineLexer.prototype.mangle=function(text){var out="",l=text.length,i=0,ch;for(;i<l;i++){ch=text.charCodeAt(i);if(Math.random()>.5){ch="x"+ch.toString(16)}out+="&#"+ch+";"}return out};function Renderer(options){this.options=options||{}}Renderer.prototype.code=function(code,lang,escaped){if(this.options.highlight){var out=this.options.highlight(code,lang);if(out!=null&&out!==code){escaped=true;code=out}}if(!lang){return"<pre><code>"+(escaped?code:escape(code,true))+"\n</code></pre>"}return'<pre><code class="'+this.options.langPrefix+escape(lang,true)+'">'+(escaped?code:escape(code,true))+"\n</code></pre>\n"};Renderer.prototype.blockquote=function(quote){return"<blockquote>\n"+quote+"</blockquote>\n"};Renderer.prototype.html=function(html){return html};Renderer.prototype.heading=function(text,level,raw){return"<h"+level+' id="'+this.options.headerPrefix+raw.toLowerCase().replace(/[^\w]+/g,"-")+'">'+text+"</h"+level+">\n"};Renderer.prototype.hr=function(){return this.options.xhtml?"<hr/>\n":"<hr>\n"};Renderer.prototype.list=function(body,ordered){var type=ordered?"ol":"ul";return"<"+type+">\n"+body+"</"+type+">\n"};Renderer.prototype.listitem=function(text){return"<li>"+text+"</li>\n"};Renderer.prototype.paragraph=function(text){return"<p>"+text+"</p>\n"};Renderer.prototype.table=function(header,body){return"<table>\n"+"<thead>\n"+header+"</thead>\n"+"<tbody>\n"+body+"</tbody>\n"+"</table>\n"};Renderer.prototype.tablerow=function(content){return"<tr>\n"+content+"</tr>\n"};Renderer.prototype.tablecell=function(content,flags){var type=flags.header?"th":"td";var tag=flags.align?"<"+type+' style="text-align:'+flags.align+'">':"<"+type+">";return tag+content+"</"+type+">\n"};Renderer.prototype.strong=function(text){return"<strong>"+text+"</strong>"};Renderer.prototype.em=function(text){return"<em>"+text+"</em>"};Renderer.prototype.codespan=function(text){return"<code>"+text+"</code>"};Renderer.prototype.br=function(){return this.options.xhtml?"<br/>":"<br>"};Renderer.prototype.del=function(text){return"<del>"+text+"</del>"};Renderer.prototype.link=function(href,title,text){if(this.options.sanitize){try{var prot=decodeURIComponent(unescape(href)).replace(/[^\w:]/g,"").toLowerCase()}catch(e){return""}if(prot.indexOf("javascript:")===0){return""}}var out='<a href="'+href+'"';if(title){out+=' title="'+title+'"'}out+=">"+text+"</a>";return out};Renderer.prototype.image=function(href,title,text){var out='<img src="'+href+'" alt="'+text+'"';if(title){out+=' title="'+title+'"'}out+=this.options.xhtml?"/>":">";return out};function Parser(options){this.tokens=[];this.token=null;this.options=options||marked.defaults;this.options.renderer=this.options.renderer||new Renderer;this.renderer=this.options.renderer;this.renderer.options=this.options}Parser.parse=function(src,options,renderer){var parser=new Parser(options,renderer);return parser.parse(src)};Parser.prototype.parse=function(src){this.inline=new InlineLexer(src.links,this.options,this.renderer);this.tokens=src.reverse();var out="";while(this.next()){out+=this.tok()}return out};Parser.prototype.next=function(){return this.token=this.tokens.pop()};Parser.prototype.peek=function(){return this.tokens[this.tokens.length-1]||0};Parser.prototype.parseText=function(){var body=this.token.text;while(this.peek().type==="text"){body+="\n"+this.next().text}return this.inline.output(body)};Parser.prototype.tok=function(){switch(this.token.type){case"space":{return""}case"hr":{return this.renderer.hr()}case"heading":{return this.renderer.heading(this.inline.output(this.token.text),this.token.depth,this.token.text)}case"code":{return this.renderer.code(this.token.text,this.token.lang,this.token.escaped)}case"table":{var header="",body="",i,row,cell,flags,j;cell="";for(i=0;i<this.token.header.length;i++){flags={header:true,align:this.token.align[i]};cell+=this.renderer.tablecell(this.inline.output(this.token.header[i]),{header:true,align:this.token.align[i]})}header+=this.renderer.tablerow(cell);for(i=0;i<this.token.cells.length;i++){row=this.token.cells[i];cell="";for(j=0;j<row.length;j++){cell+=this.renderer.tablecell(this.inline.output(row[j]),{header:false,align:this.token.align[j]})}body+=this.renderer.tablerow(cell)}return this.renderer.table(header,body)}case"blockquote_start":{var body="";while(this.next().type!=="blockquote_end"){body+=this.tok()}return this.renderer.blockquote(body)}case"list_start":{var body="",ordered=this.token.ordered;while(this.next().type!=="list_end"){body+=this.tok()}return this.renderer.list(body,ordered)}case"list_item_start":{var body="";while(this.next().type!=="list_item_end"){body+=this.token.type==="text"?this.parseText():this.tok()}return this.renderer.listitem(body)}case"loose_item_start":{var body="";while(this.next().type!=="list_item_end"){body+=this.tok()}return this.renderer.listitem(body)}case"html":{var html=!this.token.pre&&!this.options.pedantic?this.inline.output(this.token.text):this.token.text;return this.renderer.html(html)}case"paragraph":{return this.renderer.paragraph(this.inline.output(this.token.text))}case"text":{return this.renderer.paragraph(this.parseText())}}};function escape(html,encode){return html.replace(!encode?/&(?!#?\w+;)/g:/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;").replace(/"/g,"&quot;").replace(/'/g,"&#39;")}function unescape(html){return html.replace(/&([#\w]+);/g,function(_,n){n=n.toLowerCase();if(n==="colon")return":";if(n.charAt(0)==="#"){return n.charAt(1)==="x"?String.fromCharCode(parseInt(n.substring(2),16)):String.fromCharCode(+n.substring(1))}return""})}function replace(regex,opt){regex=regex.source;opt=opt||"";return function self(name,val){if(!name)return new RegExp(regex,opt);val=val.source||val;val=val.replace(/(^|[^\[])\^/g,"$1");regex=regex.replace(name,val);return self}}function noop(){}noop.exec=noop;function merge(obj){var i=1,target,key;for(;i<arguments.length;i++){target=arguments[i];for(key in target){if(Object.prototype.hasOwnProperty.call(target,key)){obj[key]=target[key]}}}return obj}function marked(src,opt,callback){if(callback||typeof opt==="function"){if(!callback){callback=opt;opt=null}opt=merge({},marked.defaults,opt||{});var highlight=opt.highlight,tokens,pending,i=0;try{tokens=Lexer.lex(src,opt)}catch(e){return callback(e)}pending=tokens.length;var done=function(err){if(err){opt.highlight=highlight;return callback(err)}var out;try{out=Parser.parse(tokens,opt)}catch(e){err=e}opt.highlight=highlight;return err?callback(err):callback(null,out)};if(!highlight||highlight.length<3){return done()}delete opt.highlight;if(!pending)return done();for(;i<tokens.length;i++){(function(token){if(token.type!=="code"){return--pending||done()}return highlight(token.text,token.lang,function(err,code){if(err)return done(err);if(code==null||code===token.text){return--pending||done()}token.text=code;token.escaped=true;--pending||done()})})(tokens[i])}return}try{if(opt)opt=merge({},marked.defaults,opt);return Parser.parse(Lexer.lex(src,opt),opt)}catch(e){e.message+="\nPlease report this to https://github.com/chjj/marked.";if((opt||marked.defaults).silent){return"<p>An error occured:</p><pre>"+escape(e.message+"",true)+"</pre>"}throw e}}marked.options=marked.setOptions=function(opt){merge(marked.defaults,opt);return marked};marked.defaults={gfm:true,tables:true,breaks:false,pedantic:false,sanitize:false,smartLists:false,silent:false,highlight:null,langPrefix:"lang-",smartypants:false,headerPrefix:"",renderer:new Renderer,xhtml:false};marked.Parser=Parser;marked.parser=Parser.parse;marked.Renderer=Renderer;marked.Lexer=Lexer;marked.lexer=Lexer.lex;marked.InlineLexer=InlineLexer;marked.inlineLexer=InlineLexer.output;marked.parse=marked;if(typeof module!=="undefined"&&typeof exports==="object"){module.exports=marked}else if(typeof define==="function"&&define.amd){define(function(){return marked})}else{this.marked=marked}}).call(function(){return this||(typeof window!=="undefined"?window:global)}());

		return module.exports;
	})();

	function formatOptions(options) {
		var toHighlight = function (code, lang) {
			if (!lang && options.defaultHighlighting.ctor === 'Just')
			{
				lang = options.defaultHighlighting._0;
			}
			if (typeof hljs !== 'undefined'
				&& lang
				&& hljs.listLanguages().indexOf(lang) >= 0)
			{
				return hljs.highlight(lang, code, true).value;
			}
			return code;
		};
		var gfm = options.githubFlavored;
		if (gfm.ctor === 'Just')
		{
			return {
				highlight: toHighlight,
				gfm: true,
				tables: gfm._0.tables,
				breaks: gfm._0.breaks,
				sanitize: options.sanitize,
				smartypants: options.smartypants
			};
		}
		else
		{
			return {
				highlight: toHighlight,
				gfm: false,
				tables: false,
				breaks: false,
				sanitize: options.sanitize,
				smartypants: options.smartypants
			};
		}
	}

	function toHtmlWith(options, rawMarkdown)
	{
		return new MarkdownWidget(options, rawMarkdown);
	}

	function MarkdownWidget(options, rawMarkdown)
	{
		this.options = options;
		this.markdown = rawMarkdown;
	}

	MarkdownWidget.prototype.type = "Widget";

	MarkdownWidget.prototype.init = function init()
	{
		var html = marked(this.markdown, formatOptions(this.options));
		var div = document.createElement('div');
		div.innerHTML = html;
		return div;
	};

	MarkdownWidget.prototype.update = function update(previous, node)
	{
		if (this.markdown !== previous.markdown || this.options != previous.options)
		{
			var html = marked(this.markdown, formatOptions(this.options));
			node.innerHTML = html;
		}
		return node;
	};


	function toElementWith(options, rawMarkdown)
	{
		return Element.markdown(marked(rawMarkdown, formatOptions(options)));
	}

	return Elm.Native.Markdown.values = {
		toHtmlWith: F2(toHtmlWith),
		toElementWith: F2(toElementWith)
	};
};

Elm.Markdown = Elm.Markdown || {};
Elm.Markdown.make = function (_elm) {
   "use strict";
   _elm.Markdown = _elm.Markdown || {};
   if (_elm.Markdown.values) return _elm.Markdown.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Html = Elm.Html.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Markdown = Elm.Native.Markdown.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var toElementWith = $Native$Markdown.toElementWith;
   var toHtmlWith = $Native$Markdown.toHtmlWith;
   var defaultOptions = {githubFlavored: $Maybe.Just({tables: false,breaks: false}),defaultHighlighting: $Maybe.Nothing,sanitize: false,smartypants: false};
   var Options = F4(function (a,b,c,d) {    return {githubFlavored: a,defaultHighlighting: b,sanitize: c,smartypants: d};});
   var toElement = function (string) {    return A2($Native$Markdown.toElementWith,defaultOptions,string);};
   var toHtml = function (string) {    return A2($Native$Markdown.toHtmlWith,defaultOptions,string);};
   return _elm.Markdown.values = {_op: _op
                                 ,toHtml: toHtml
                                 ,toElement: toElement
                                 ,Options: Options
                                 ,defaultOptions: defaultOptions
                                 ,toHtmlWith: toHtmlWith
                                 ,toElementWith: toElementWith};
};
Elm.Utils = Elm.Utils || {};
Elm.Utils.Markdown = Elm.Utils.Markdown || {};
Elm.Utils.Markdown.make = function (_elm) {
   "use strict";
   _elm.Utils = _elm.Utils || {};
   _elm.Utils.Markdown = _elm.Utils.Markdown || {};
   if (_elm.Utils.Markdown.values) return _elm.Utils.Markdown.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Html = Elm.Html.make(_elm),
   $List = Elm.List.make(_elm),
   $Markdown = Elm.Markdown.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var myOptions = function () {    var options = $Markdown.defaultOptions;return _U.update(options,{defaultHighlighting: $Maybe.Just("elm")});}();
   var block = function (raw) {    return A2($Markdown.toHtmlWith,myOptions,raw);};
   return _elm.Utils.Markdown.values = {_op: _op,block: block};
};
Elm.Docs = Elm.Docs || {};
Elm.Docs.Entry = Elm.Docs.Entry || {};
Elm.Docs.Entry.make = function (_elm) {
   "use strict";
   _elm.Docs = _elm.Docs || {};
   _elm.Docs.Entry = _elm.Docs.Entry || {};
   if (_elm.Docs.Entry.values) return _elm.Docs.Entry.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Docs$Name = Elm.Docs.Name.make(_elm),
   $Docs$Type = Elm.Docs.Type.make(_elm),
   $Effects = Elm.Effects.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Html$Attributes = Elm.Html.Attributes.make(_elm),
   $Json$Encode = Elm.Json.Encode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Regex = Elm.Regex.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm),
   $Utils$Code = Elm.Utils.Code.make(_elm),
   $Utils$Markdown = Elm.Utils.Markdown.make(_elm);
   var _op = {};
   var viewTag = F2(function (tipeToHtml,_p0) {
      var _p1 = _p0;
      return A2($List._op["::"],
      $Html.text(_p1.tag),
      A2($List.concatMap,F2(function (x,y) {    return A2($List._op["::"],x,y);})($Utils$Code.space),A2($List.map,tipeToHtml,_p1.args)));
   });
   var longFunctionAnnotationSearch = F4(function (basePath,nameDict,args,result) {
      var starters = A2($List._op["::"],
      _U.list([$Html.text("    "),$Utils$Code.colon,$Html.text("  ")]),
      A2($List.repeat,$List.length(args),_U.list([$Html.text("    "),$Utils$Code.arrow,$Utils$Code.space])));
      var tipeHtml = A2($List.map,A3($Docs$Type.toHtmlWithBasePath,basePath,nameDict,$Docs$Type.Func),A2($Basics._op["++"],args,_U.list([result])));
      return A3($List.map2,F2(function (x,y) {    return A2($Basics._op["++"],x,y);}),starters,tipeHtml);
   });
   var longFunctionAnnotation = F3(function (nameDict,args,result) {
      var starters = A2($List._op["::"],
      _U.list([$Html.text("    "),$Utils$Code.colon,$Html.text("  ")]),
      A2($List.repeat,$List.length(args),_U.list([$Html.text("    "),$Utils$Code.arrow,$Utils$Code.space])));
      var tipeHtml = A2($List.map,A2($Docs$Type.toHtml,nameDict,$Docs$Type.Func),A2($Basics._op["++"],args,_U.list([result])));
      return A3($List.map2,F2(function (x,y) {    return A2($Basics._op["++"],x,y);}),starters,tipeHtml);
   });
   var valueAnnotationSearch = F5(function (basePath,canonical,nameDict,name,tipe) {
      var _p2 = tipe;
      if (_p2.ctor === "Function") {
            return _U.cmp($String.length(name) + 3 + A2($Docs$Type.length,$Docs$Type.Other,tipe),64) > 0 ? A2($List._op["::"],
            _U.list([A3($Docs$Name.toBaseLink,basePath,nameDict,canonical)]),
            A4(longFunctionAnnotationSearch,basePath,nameDict,_p2._0,_p2._1)) : _U.list([A2($List._op["::"],
            A3($Docs$Name.toBaseLink,basePath,nameDict,canonical),
            A2($Basics._op["++"],$Utils$Code.padded($Utils$Code.colon),A4($Docs$Type.toHtmlWithBasePath,basePath,nameDict,$Docs$Type.Other,tipe)))]);
         } else {
            return _U.list([A2($List._op["::"],
            A3($Docs$Name.toBaseLink,basePath,nameDict,canonical),
            A2($Basics._op["++"],$Utils$Code.padded($Utils$Code.colon),A4($Docs$Type.toHtmlWithBasePath,basePath,nameDict,$Docs$Type.Other,tipe)))]);
         }
   });
   var operator = $Regex.regex("^[^a-zA-Z0-9]+$");
   var annotationBlock = function (bits) {
      return A2($Html.div,_U.list([$Html$Attributes.$class("docs-annotation")]),$List.concat(A2($List.intersperse,_U.list([$Html.text("\n")]),bits)));
   };
   _op["=>"] = F2(function (v0,v1) {    return {ctor: "_Tuple2",_0: v0,_1: v1};});
   var nameToLink = function (name) {
      var humanName = A2($Regex.contains,operator,name) ? A2($Basics._op["++"],"(",A2($Basics._op["++"],name,")")) : name;
      return A2($Html.a,
      _U.list([$Html$Attributes.style(_U.list([A2(_op["=>"],"font-weight","bold")])),$Html$Attributes.href(A2($Basics._op["++"],"#",name))]),
      _U.list([$Html.text(humanName)]));
   };
   var valueAnnotation = F3(function (nameDict,name,tipe) {
      var _p3 = tipe;
      if (_p3.ctor === "Function") {
            return _U.cmp($String.length(name) + 3 + A2($Docs$Type.length,$Docs$Type.Other,tipe),64) > 0 ? A2($List._op["::"],
            _U.list([nameToLink(name)]),
            A3(longFunctionAnnotation,nameDict,_p3._0,_p3._1)) : _U.list([A2($List._op["::"],
            nameToLink(name),
            A2($Basics._op["++"],$Utils$Code.padded($Utils$Code.colon),A3($Docs$Type.toHtml,nameDict,$Docs$Type.Other,tipe)))]);
         } else {
            return _U.list([A2($List._op["::"],
            nameToLink(name),
            A2($Basics._op["++"],$Utils$Code.padded($Utils$Code.colon),A3($Docs$Type.toHtml,nameDict,$Docs$Type.Other,tipe)))]);
         }
   });
   var unionAnnotation = F4(function (tipeToHtml,name,vars,tags) {
      var tagLines = A3($List.map2,
      F2(function (x,y) {    return A2($List._op["::"],x,y);}),
      A2($List._op["::"],$Html.text("    = "),A2($List.repeat,$List.length(tags) - 1,$Html.text("    | "))),
      A2($List.map,viewTag(tipeToHtml),tags));
      var nameLine = _U.list([$Utils$Code.keyword("type")
                             ,$Utils$Code.space
                             ,nameToLink(name)
                             ,$Html.text($String.concat(A2($List.map,F2(function (x,y) {    return A2($Basics._op["++"],x,y);})(" "),vars)))]);
      return A2($List._op["::"],nameLine,tagLines);
   });
   var aliasNameLine = F2(function (name,vars) {
      return _U.list([$Utils$Code.keyword("type")
                     ,$Utils$Code.space
                     ,$Utils$Code.keyword("alias")
                     ,$Utils$Code.space
                     ,nameToLink(name)
                     ,$Html.text($String.concat(A2($List.map,F2(function (x,y) {    return A2($Basics._op["++"],x,y);})(" "),vars)))
                     ,$Utils$Code.space
                     ,$Utils$Code.equals
                     ,$Utils$Code.space]);
   });
   var aliasAnnotation = F4(function (nameDict,name,vars,tipe) {
      var typeLines = function () {
         var _p4 = tipe;
         if (_p4.ctor === "Record") {
               var _p7 = _p4._0;
               var _p5 = function () {
                  var _p6 = _p4._1;
                  if (_p6.ctor === "Nothing") {
                        return {ctor: "_Tuple2"
                               ,_0: _U.list([])
                               ,_1: A2($List._op["::"],$Html.text("    { "),A2($List.repeat,$List.length(_p7),$Html.text("    , ")))};
                     } else {
                        return {ctor: "_Tuple2"
                               ,_0: _U.list([_U.list([$Html.text("    { "),$Html.text(_p6._0),$Html.text(" |")])])
                               ,_1: A2($List._op["::"],$Html.text("      | "),A2($List.repeat,$List.length(_p7),$Html.text("      , ")))};
                     }
               }();
               var firstLine = _p5._0;
               var starters = _p5._1;
               return A2($Basics._op["++"],
               firstLine,
               A2($Basics._op["++"],
               A3($List.map2,F2(function (x,y) {    return A2($List._op["::"],x,y);}),starters,A2($List.map,$Docs$Type.fieldToHtml(nameDict),_p7)),
               _U.list([_U.list([$Html.text("    }")])])));
            } else {
               return _U.list([A2($List._op["::"],$Html.text("    "),A3($Docs$Type.toHtml,nameDict,$Docs$Type.Other,tipe))]);
            }
      }();
      return A2($List._op["::"],A2(aliasNameLine,name,vars),typeLines);
   });
   var typeViewSearch = F4(function (basePath,canonical,nameDict,model) {
      var description = A2($Maybe.withDefault,"",$List.head(A2($String.split,"\n\n",model.docs)));
      var path = A2($Basics._op["++"],"http://package.elm-lang.org/packages/",basePath);
      var annotation = function () {
         var _p8 = model.info;
         switch (_p8.ctor)
         {case "Value": return A5(valueAnnotationSearch,path,canonical,nameDict,model.name,_p8._0);
            case "Union": return A4(unionAnnotation,A2($Docs$Type.toHtml,nameDict,$Docs$Type.App),model.name,_p8._0.vars,_p8._0.tags);
            default: return A4(aliasAnnotation,nameDict,model.name,_p8._0.vars,_p8._0.tipe);}
      }();
      return A2($Html.div,
      _U.list([$Html$Attributes.$class("docs-entry")]),
      _U.list([annotationBlock(annotation)
              ,A2($Html.div,_U.list([]),_U.list([$Utils$Markdown.block(description)]))
              ,A2($Html.span,
              _U.list([]),
              _U.list([A2($Html.a,
              _U.list([$Html$Attributes.href(path),$Html$Attributes.style(_U.list([A2(_op["=>"],"color","#bbb")]))]),
              _U.list([$Html.text(basePath)]))]))]));
   });
   var typeView = F2(function (nameDict,model) {
      var annotation = function () {
         var _p9 = model.info;
         switch (_p9.ctor)
         {case "Value": return A3(valueAnnotation,nameDict,model.name,_p9._0);
            case "Union": return A4(unionAnnotation,A2($Docs$Type.toHtml,nameDict,$Docs$Type.App),model.name,_p9._0.vars,_p9._0.tags);
            default: return A4(aliasAnnotation,nameDict,model.name,_p9._0.vars,_p9._0.tipe);}
      }();
      return A2($Html.div,
      _U.list([$Html$Attributes.$class("docs-entry"),$Html$Attributes.id(model.name)]),
      _U.list([annotationBlock(annotation),A2($Html.div,_U.list([$Html$Attributes.$class("docs-comment")]),_U.list([$Utils$Markdown.block(model.docs)]))]));
   });
   var stringView = function (model) {
      var annotation = function () {
         var _p10 = model.info;
         switch (_p10.ctor)
         {case "Value": return _U.list([A2($List._op["::"],
              nameToLink(model.name),
              A2($Basics._op["++"],$Utils$Code.padded($Utils$Code.colon),_U.list([$Html.text(_p10._0)])))]);
            case "Union": return A4(unionAnnotation,function (t) {    return _U.list([$Html.text(t)]);},model.name,_p10._0.vars,_p10._0.tags);
            default: return _U.list([A2(aliasNameLine,model.name,_p10._0.vars),_U.list([$Html.text("    "),$Html.text(_p10._0.tipe)])]);}
      }();
      return A2($Html.div,
      _U.list([$Html$Attributes.$class("docs-entry"),$Html$Attributes.id(model.name)]),
      _U.list([annotationBlock(annotation),A2($Html.div,_U.list([$Html$Attributes.$class("docs-comment")]),_U.list([$Utils$Markdown.block(model.docs)]))]));
   };
   var typeDistance = F2(function (queryType,model) {
      var _p11 = model.info;
      if (_p11.ctor === "Value") {
            return A2($Docs$Type.distance,queryType,_p11._0);
         } else {
            return 100;
         }
   });
   var nameDistance = F2(function (query,model) {
      var _p12 = model.info;
      if (_p12.ctor === "Value") {
            return _U.eq(query,model.name) ? 0 : A2($String.contains,query,model.name) ? 1 : 10;
         } else {
            return 100;
         }
   });
   var tagMap = F2(function (func,tag) {    return _U.update(tag,{args: A2($List.map,func,tag.args)});});
   var update = F2(function (action,model) {    return {ctor: "_Tuple2",_0: model,_1: $Effects.none};});
   var encodeInfo = function (info) {
      var _p13 = info;
      if (_p13.ctor === "Value") {
            return $Json$Encode.string(_p13._0);
         } else {
            return $Json$Encode.string("TODO");
         }
   };
   var encode = function (model) {
      return $Json$Encode.object(_U.list([{ctor: "_Tuple2",_0: "name",_1: $Json$Encode.string(model.name)}
                                         ,{ctor: "_Tuple2",_0: "comment",_1: $Json$Encode.string(model.docs)}
                                         ,{ctor: "_Tuple2",_0: "type",_1: encodeInfo(model.info)}]));
   };
   var Fixity = F2(function (a,b) {    return {precedence: a,associativity: b};});
   var Tag = F2(function (a,b) {    return {tag: a,args: b};});
   var Alias = function (a) {    return {ctor: "Alias",_0: a};};
   var Union = function (a) {    return {ctor: "Union",_0: a};};
   var Value = F2(function (a,b) {    return {ctor: "Value",_0: a,_1: b};});
   var map = F2(function (func,model) {
      var newInfo = function () {
         var _p14 = model.info;
         switch (_p14.ctor)
         {case "Value": return A2(Value,func(_p14._0),_p14._1);
            case "Union": return Union({vars: _p14._0.vars,tags: A2($List.map,tagMap(func),_p14._0.tags)});
            default: return Alias({vars: _p14._0.vars,tipe: func(_p14._0.tipe)});}
      }();
      return _U.update(model,{info: newInfo});
   });
   var Model = F3(function (a,b,c) {    return {name: a,info: b,docs: c};});
   return _elm.Docs.Entry.values = {_op: _op
                                   ,Model: Model
                                   ,Value: Value
                                   ,Union: Union
                                   ,Alias: Alias
                                   ,Tag: Tag
                                   ,Fixity: Fixity
                                   ,encode: encode
                                   ,encodeInfo: encodeInfo
                                   ,update: update
                                   ,map: map
                                   ,tagMap: tagMap
                                   ,nameDistance: nameDistance
                                   ,typeDistance: typeDistance
                                   ,stringView: stringView
                                   ,typeViewSearch: typeViewSearch
                                   ,typeView: typeView
                                   ,annotationBlock: annotationBlock
                                   ,nameToLink: nameToLink
                                   ,operator: operator
                                   ,valueAnnotationSearch: valueAnnotationSearch
                                   ,valueAnnotation: valueAnnotation
                                   ,longFunctionAnnotation: longFunctionAnnotation
                                   ,longFunctionAnnotationSearch: longFunctionAnnotationSearch
                                   ,unionAnnotation: unionAnnotation
                                   ,viewTag: viewTag
                                   ,aliasAnnotation: aliasAnnotation
                                   ,aliasNameLine: aliasNameLine};
};
Elm.Docs = Elm.Docs || {};
Elm.Docs.Package = Elm.Docs.Package || {};
Elm.Docs.Package.make = function (_elm) {
   "use strict";
   _elm.Docs = _elm.Docs || {};
   _elm.Docs.Package = _elm.Docs.Package || {};
   if (_elm.Docs.Package.values) return _elm.Docs.Package.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Docs$Entry = Elm.Docs.Entry.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $Json$Encode = Elm.Json.Encode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var tipe = $Json$Decode.string;
   var alias = A3($Json$Decode.object2,
   F2(function (vars,tipe) {    return $Docs$Entry.Alias({vars: vars,tipe: tipe});}),
   A2($Json$Decode._op[":="],"args",$Json$Decode.list($Json$Decode.string)),
   A2($Json$Decode._op[":="],"type",tipe));
   var tag = A3($Json$Decode.tuple2,$Docs$Entry.Tag,$Json$Decode.string,$Json$Decode.list(tipe));
   var union = A3($Json$Decode.object2,
   F2(function (vars,tags) {    return $Docs$Entry.Union({vars: vars,tags: tags});}),
   A2($Json$Decode._op[":="],"args",$Json$Decode.list($Json$Decode.string)),
   A2($Json$Decode._op[":="],"cases",$Json$Decode.list(tag)));
   var fixity = A3($Json$Decode.object2,
   $Docs$Entry.Fixity,
   A2($Json$Decode._op[":="],"precedence",$Json$Decode.$int),
   A2($Json$Decode._op[":="],"associativity",$Json$Decode.string));
   var value = A3($Json$Decode.object2,$Docs$Entry.Value,A2($Json$Decode._op[":="],"type",tipe),$Json$Decode.maybe(fixity));
   var entry = function (decodeInfo) {
      return A4($Json$Decode.object3,
      $Docs$Entry.Model,
      A2($Json$Decode._op[":="],"name",$Json$Decode.string),
      decodeInfo,
      A2($Json$Decode._op[":="],"comment",$Json$Decode.string));
   };
   var dictBy = F2(function (f,list) {    return $Dict.fromList(A2($List.map,function (x) {    return {ctor: "_Tuple2",_0: f(x),_1: x};},list));});
   var encodeModule = function (modul) {
      return $Json$Encode.object(_U.list([{ctor: "_Tuple2",_0: "name",_1: $Json$Encode.string(modul.name)}
                                         ,{ctor: "_Tuple2",_0: "comment",_1: $Json$Encode.string(modul.comment)}
                                         ,{ctor: "_Tuple2",_0: "aliases",_1: $Json$Encode.list(_U.list([]))}
                                         ,{ctor: "_Tuple2",_0: "types",_1: $Json$Encode.list(_U.list([]))}
                                         ,{ctor: "_Tuple2"
                                          ,_0: "values"
                                          ,_1: $Json$Encode.list(A2($List.map,
                                          $Docs$Entry.encode,
                                          A2($List.filter,
                                          function (entry) {
                                             var _p0 = entry.info;
                                             if (_p0.ctor === "Value") {
                                                   return true;
                                                } else {
                                                   return false;
                                                }
                                          },
                                          $Dict.values(modul.entries))))}]));
   };
   var encodePackage = function ($package) {    return $Json$Encode.list(A2($List.map,encodeModule,$Dict.values($package)));};
   var Module = F3(function (a,b,c) {    return {name: a,comment: b,entries: c};});
   var decodeModule = function () {
      var make = F5(function (name,comment,values,unions,aliases) {
         return A3(Module,name,comment,A2(dictBy,function (_) {    return _.name;},A2($Basics._op["++"],values,A2($Basics._op["++"],unions,aliases))));
      });
      return A6($Json$Decode.object5,
      make,
      A2($Json$Decode._op[":="],"name",$Json$Decode.string),
      A2($Json$Decode._op[":="],"comment",$Json$Decode.string),
      A2($Json$Decode._op[":="],"aliases",$Json$Decode.list(entry(alias))),
      A2($Json$Decode._op[":="],"types",$Json$Decode.list(entry(union))),
      A2($Json$Decode._op[":="],"values",$Json$Decode.list(entry(value))));
   }();
   var decodePackage = A2($Json$Decode.map,dictBy(function (_) {    return _.name;}),$Json$Decode.list(decodeModule));
   return _elm.Docs.Package.values = {_op: _op,encodePackage: encodePackage,decodePackage: decodePackage,decodeModule: decodeModule,Module: Module};
};
Elm.Utils = Elm.Utils || {};
Elm.Utils.Path = Elm.Utils.Path || {};
Elm.Utils.Path.make = function (_elm) {
   "use strict";
   _elm.Utils = _elm.Utils || {};
   _elm.Utils.Path = _elm.Utils.Path || {};
   if (_elm.Utils.Path.values) return _elm.Utils.Path.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm);
   var _op = {};
   _op["</>"] = F2(function (a,b) {    return A2($Basics._op["++"],a,A2($Basics._op["++"],"/",b));});
   var hyphenate = function (string) {    return A2($String.map,function (c) {    return _U.eq(c,_U.chr(".")) ? _U.chr("-") : c;},string);};
   return _elm.Utils.Path.values = {_op: _op,hyphenate: hyphenate};
};
Elm.Page = Elm.Page || {};
Elm.Page.Context = Elm.Page.Context || {};
Elm.Page.Context.make = function (_elm) {
   "use strict";
   _elm.Page = _elm.Page || {};
   _elm.Page.Context = _elm.Page.Context || {};
   if (_elm.Page.Context.values) return _elm.Page.Context.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Docs$Package = Elm.Docs.Package.make(_elm),
   $Http = Elm.Http.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Task = Elm.Task.make(_elm),
   $Utils$Path = Elm.Utils.Path.make(_elm);
   var _op = {};
   var pathTo = F2(function (_p0,file) {
      var _p1 = _p0;
      return A2($Utils$Path._op["</>"],
      A2($Utils$Path._op["</>"],
      A2($Utils$Path._op["</>"],A2($Utils$Path._op["</>"],"https://crossorigin.me/http://package.elm-lang.org/packages",_p1.user),_p1.project),
      _p1.version),
      file);
   });
   var getDocs = function (context) {    return A2($Http.get,$Docs$Package.decodePackage,A2(pathTo,context,"documentation.json"));};
   var getReadme = function (context) {    return $Http.getString(A2(pathTo,context,"README.md"));};
   var VersionContext = F5(function (a,b,c,d,e) {    return {user: a,project: b,version: c,allVersions: d,moduleName: e};});
   var OverviewContext = F3(function (a,b,c) {    return {user: a,project: b,versions: c};});
   return _elm.Page.Context.values = {_op: _op
                                     ,OverviewContext: OverviewContext
                                     ,VersionContext: VersionContext
                                     ,getReadme: getReadme
                                     ,getDocs: getDocs
                                     ,pathTo: pathTo};
};
Elm.Route = Elm.Route || {};
Elm.Route.make = function (_elm) {
   "use strict";
   _elm.Route = _elm.Route || {};
   if (_elm.Route.values) return _elm.Route.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Page$Context = Elm.Page.Context.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   _op["=>"] = F2(function (f,a) {    return f($Maybe.Just(a));});
   var Version = F3(function (a,b,c) {    return {ctor: "Version",_0: a,_1: b,_2: c};});
   var Package = F2(function (a,b) {    return {ctor: "Package",_0: a,_1: b};});
   var User = F2(function (a,b) {    return {ctor: "User",_0: a,_1: b};});
   var Packages = function (a) {    return {ctor: "Packages",_0: a};};
   var fromOverviewContext = function (ctx) {    return A2(_op["=>"],Packages,A2(_op["=>"],User(ctx.user),A2(Package,ctx.project,$Maybe.Nothing)));};
   var fromVersionContext = function (ctx) {
      return A2(_op["=>"],Packages,A2(_op["=>"],User(ctx.user),A2(_op["=>"],Package(ctx.project),A3(Version,ctx.version,ctx.allVersions,ctx.moduleName))));
   };
   var Help = {ctor: "Help"};
   return _elm.Route.values = {_op: _op
                              ,Help: Help
                              ,Packages: Packages
                              ,User: User
                              ,Package: Package
                              ,Version: Version
                              ,fromOverviewContext: fromOverviewContext
                              ,fromVersionContext: fromVersionContext};
};
Elm.Component = Elm.Component || {};
Elm.Component.Header = Elm.Component.Header || {};
Elm.Component.Header.make = function (_elm) {
   "use strict";
   _elm.Component = _elm.Component || {};
   _elm.Component.Header = _elm.Component.Header || {};
   if (_elm.Component.Header.values) return _elm.Component.Header.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Docs$Version = Elm.Docs.Version.make(_elm),
   $Effects = Elm.Effects.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Html$Attributes = Elm.Html.Attributes.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Route = Elm.Route.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Utils$Path = Elm.Utils.Path.make(_elm);
   var _op = {};
   var versionWarning = function (model) {
      var warning = function () {
         var _p0 = model.route;
         if (_p0.ctor === "Packages" && _p0._0.ctor === "Just" && _p0._0._0._1.ctor === "Just" && _p0._0._0._1._0._1.ctor === "Just") {
               var _p1 = A2($Docs$Version.realMax,_p0._0._0._1._0._1._0._0,_p0._0._0._1._0._1._0._1);
               if (_p1.ctor === "Nothing") {
                     return _U.list([]);
                  } else {
                     var _p2 = _p1._0;
                     var moduleName = A2($Maybe.withDefault,"",A2($Maybe.map,$Utils$Path.hyphenate,_p0._0._0._1._0._1._0._2));
                     return _U.list([A2($Html.p,
                     _U.list([$Html$Attributes.$class("version-warning")]),
                     _U.list([$Html.text("Warning! The latest version of this package is ")
                             ,A2($Html.a,
                             _U.list([$Html$Attributes.href(A2($Utils$Path._op["</>"],
                             A2($Utils$Path._op["</>"],A2($Utils$Path._op["</>"],A2($Utils$Path._op["</>"],"/packages",_p0._0._0._0),_p0._0._0._1._0._0),_p2),
                             moduleName))]),
                             _U.list([$Html.text(_p2)]))]))]);
                  }
            } else {
               return _U.list([]);
            }
      }();
      return _U.list([A2($Html.div,_U.list([$Html$Attributes.$class("header-underbar")]),warning)]);
   };
   var maybe = F2(function (unroll,maybeRoute) {
      var _p3 = maybeRoute;
      if (_p3.ctor === "Nothing") {
            return _U.list([]);
         } else {
            return unroll(_p3._0);
         }
   });
   var spacey = function (token) {    return A2($Html.span,_U.list([$Html$Attributes.$class("spacey-char")]),_U.list([$Html.text(token)]));};
   _op["=>"] = F2(function (v0,v1) {    return {ctor: "_Tuple2",_0: v0,_1: v1};});
   var center = F2(function (color,kids) {
      return A2($Html.div,
      _U.list([$Html$Attributes.style(_U.list([A2(_op["=>"],"background-color",color)]))]),
      _U.list([A2($Html.div,_U.list([$Html$Attributes.$class("center")]),kids)]));
   });
   var logo = A2($Html.div,
   _U.list([$Html$Attributes.style(_U.list([A2(_op["=>"],"display","inline-block")]))]),
   _U.list([A2($Html.img,
           _U.list([$Html$Attributes.src("assets/elm_logo.svg")
                   ,$Html$Attributes.style(_U.list([A2(_op["=>"],"height","30px")
                                                   ,A2(_op["=>"],"vertical-align","bottom")
                                                   ,A2(_op["=>"],"padding-right","8px")]))]),
           _U.list([]))
           ,A2($Html.img,
           _U.list([$Html$Attributes.src("assets/logo-name.png")
                   ,$Html$Attributes.style(_U.list([A2(_op["=>"],"height","30px")
                                                   ,A2(_op["=>"],"vertical-align","bottom")
                                                   ,A2(_op["=>"],"padding-right","24px")]))]),
           _U.list([]))]));
   var headerLink = F2(function (url,words) {
      return A2($Html.a,_U.list([$Html$Attributes.href(url),$Html$Attributes.style(_U.list([A2(_op["=>"],"color","#333333")]))]),_U.list([$Html.text(words)]));
   });
   var unrollModuleeRoute = F4(function (user,pkg,vsn,name) {
      return _U.list([spacey("/")
                     ,A2(headerLink,
                     A2($Utils$Path._op["</>"],
                     A2($Utils$Path._op["</>"],A2($Utils$Path._op["</>"],A2($Utils$Path._op["</>"],"/packages",user),pkg),vsn),
                     $Utils$Path.hyphenate(name)),
                     name)]);
   });
   var unrollVersionRoute = F3(function (user,pkg,_p4) {
      var _p5 = _p4;
      var _p6 = _p5._0;
      return A2($List._op["::"],
      spacey("/"),
      A2($List._op["::"],
      A2(headerLink,A2($Utils$Path._op["</>"],A2($Utils$Path._op["</>"],A2($Utils$Path._op["</>"],"/packages",user),pkg),_p6),_p6),
      A2(maybe,A3(unrollModuleeRoute,user,pkg,_p6),_p5._2)));
   });
   var unrollPackageRoute = F2(function (user,_p7) {
      var _p8 = _p7;
      var _p9 = _p8._0;
      return A2($List._op["::"],
      spacey("/"),
      A2($List._op["::"],
      A2(headerLink,A2($Utils$Path._op["</>"],A2($Utils$Path._op["</>"],"/packages",user),_p9),_p9),
      A2(maybe,A2(unrollVersionRoute,user,_p9),_p8._1)));
   });
   var unrollUserRoute = function (_p10) {
      var _p11 = _p10;
      var _p12 = _p11._0;
      return A2($List._op["::"],A2(headerLink,A2($Utils$Path._op["</>"],"https://github.com",_p12),_p12),A2(maybe,unrollPackageRoute(_p12),_p11._1));
   };
   var unrollRoute = function (route) {
      var _p13 = route;
      if (_p13.ctor === "Help") {
            return _U.list([$Html.text("help")]);
         } else {
            return A2(maybe,unrollUserRoute,_p13._0);
         }
   };
   var headerLinks = function (model) {
      return A2($Html.h1,
      _U.list([$Html$Attributes.$class("header")]),
      A2($List._op["::"],A2($Html.a,_U.list([$Html$Attributes.href("http://package.elm-lang.org/")]),_U.list([logo])),unrollRoute(model.route)));
   };
   var view = F3(function (_p14,model,contents) {
      return A2($Html.div,
      _U.list([]),
      _U.list([A2(center,"#eeeeee",_U.list([headerLinks(model)]))
              ,A2(center,"#60B5CC",versionWarning(model))
              ,A2($Html.div,_U.list([$Html$Attributes.$class("center")]),contents)
              ,A2($Html.div,
              _U.list([$Html$Attributes.$class("footer")]),
              _U.list([$Html.text("All code for this site is open source and written in Elm. ")
                      ,A2($Html.a,
                      _U.list([$Html$Attributes.$class("grey-link"),$Html$Attributes.href("https://github.com/elm-lang/package.elm-lang.org/")]),
                      _U.list([$Html.text("Check it out")]))
                      ,$Html.text("! — © 2012-2015 Evan Czaplicki")]))]));
   });
   var update = F2(function (action,model) {    return {ctor: "_Tuple2",_0: model,_1: $Effects.none};});
   var Model = function (a) {    return {route: a};};
   var init = function (route) {    return {ctor: "_Tuple2",_0: Model(route),_1: $Effects.none};};
   return _elm.Component.Header.values = {_op: _op
                                         ,Model: Model
                                         ,init: init
                                         ,update: update
                                         ,view: view
                                         ,center: center
                                         ,headerLinks: headerLinks
                                         ,spacey: spacey
                                         ,logo: logo
                                         ,headerLink: headerLink
                                         ,unrollRoute: unrollRoute
                                         ,maybe: maybe
                                         ,unrollUserRoute: unrollUserRoute
                                         ,unrollPackageRoute: unrollPackageRoute
                                         ,unrollVersionRoute: unrollVersionRoute
                                         ,unrollModuleeRoute: unrollModuleeRoute
                                         ,versionWarning: versionWarning};
};
Elm.Native.Jump = {};

Elm.Native.Jump.make = function(localRuntime)
{
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Jump = localRuntime.Native.Jump || {};
	if (localRuntime.Native.Jump.values)
	{
		return localRuntime.Native.Jump.values;
	}

	var Task = Elm.Native.Task.make(localRuntime);

	var jump = Task.asyncFunction(function(callback) {
		var hash = window.location.hash;
		if (hash.length > 0 && hash[0] === '#')
		{
			var id = hash.slice(1);
			var target = document.getElementById(id);
			if (typeof target.scrollIntoView === 'function')
			{
				target.scrollIntoView(true);
			}
		}
		return callback(Task.succeed({ ctor: '_Tuple0' }));
	});

	return localRuntime.Native.Jump.values = {
		jump: jump
	};
};

Elm.Native.Parse = {};

Elm.Native.Parse.make = function(localRuntime)
{
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Parse = localRuntime.Native.Parse || {};
	if (localRuntime.Native.Parse.values)
	{
		return localRuntime.Native.Parse.values;
	}

	var List = Elm.Native.List.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);


	// HELPERS

	function ok(index, value) {
		return { ctor: 'Ok', _0: { index: index, value: value } };
	}

	function err(msg) {
		return { ctor: 'Err', _0: msg };
	}


	// BASICS

	function succeed(value)
	{
		return function(input, index) {
			return ok(index, value);
		};
	}

	function fail(msg)
	{
		return function(input, index) {
			return err(msg);
		};
	}


	// SATISFY

	function satisfy(isOk)
	{
		return function(input, index) {
			if (input.length <= index)
			{
				return err('ran out of characters');
			}
			var chr = Utils.chr(input[index]);
			return isOk(chr)
				? ok(index + 1, chr)
				: err("char '" + input[index] + "' at index " + index + " is not okay.");
		};
	}

	function string(expected)
	{
		return function(input, index) {
			var nextIndex = index + expected.length;
			var actual = input.slice(index, nextIndex);
			return expected === actual
				? ok(nextIndex, actual)
				: err("expecting '" + expected + "' at index " + index + " but instead saw '" + actual + "'.");
		};
	}


	// ONE OF

	function oneOf(parserList)
	{
		var parsers = List.toArray(parserList);

		return function(input, index) {
			var len = parsers.length;
			for (var i = 0; i < len; ++i)
			{
				var result = parsers[i](input, index);
				if (result.ctor === 'Ok')
				{
					return result;
				}
			}
			return err('none of the parsers given to oneOf worked');
		};
	}


	// AND THEN

	function andThen(parser, callback)
	{
		return function(input, index) {
			var result = parser(input, index);
			if (result.ctor === 'Ok')
			{
				var newData = result._0;
				return callback(newData.value)(input, newData.index);
			}
			else
			{
				return result;
			}
		};
	}


	// RUN

	function run(parser, input)
	{
		var result = parser(input, 0);
		if (result.ctor === 'Ok')
		{
			return { ctor: 'Ok', _0: result._0.value };
		}
		else
		{
			return result;
		}
	}


	return localRuntime.Native.Parse.values = {
		succeed: succeed,
		fail: fail,
		satisfy: satisfy,
		string: string,
		oneOf: oneOf,
		andThen: F2(andThen),
		run: F2(run)
	};
};

Elm.Parse = Elm.Parse || {};
Elm.Parse.Combinators = Elm.Parse.Combinators || {};
Elm.Parse.Combinators.make = function (_elm) {
   "use strict";
   _elm.Parse = _elm.Parse || {};
   _elm.Parse.Combinators = _elm.Parse.Combinators || {};
   if (_elm.Parse.Combinators.values) return _elm.Parse.Combinators.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Char = Elm.Char.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Parse = Elm.Native.Parse.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var andThen = $Native$Parse.andThen;
   var fail = $Native$Parse.fail;
   var succeed = $Native$Parse.succeed;
   var map = F2(function (func,parser) {    return A2(andThen,parser,function (a) {    return succeed(func(a));});});
   var map2 = F3(function (func,parserA,parserB) {
      return A2(andThen,parserA,function (a) {    return A2(andThen,parserB,function (b) {    return succeed(A2(func,a,b));});});
   });
   var ignore1 = F2(function (x,parser) {    return A3(map2,F2(function (_p0,a) {    return a;}),x,parser);});
   var map3 = F4(function (func,parserA,parserB,parserC) {
      return A2(andThen,
      parserA,
      function (a) {
         return A2(andThen,parserB,function (b) {    return A2(andThen,parserC,function (c) {    return succeed(A3(func,a,b,c));});});
      });
   });
   var ignore2 = F3(function (x,y,parser) {    return A4(map3,F3(function (_p2,_p1,a) {    return a;}),x,y,parser);});
   var middle = F3(function (x,parser,y) {    return A4(map3,F3(function (_p4,a,_p3) {    return a;}),x,parser,y);});
   var map4 = F5(function (func,parserA,parserB,parserC,parserD) {
      return A2(andThen,
      parserA,
      function (a) {
         return A2(andThen,
         parserB,
         function (b) {
            return A2(andThen,parserC,function (c) {    return A2(andThen,parserD,function (d) {    return succeed(A4(func,a,b,c,d));});});
         });
      });
   });
   var ignore3 = F4(function (x,y,z,parser) {    return A5(map4,F4(function (_p7,_p6,_p5,a) {    return a;}),x,y,z,parser);});
   var lazy = function (thunk) {    return A2(andThen,succeed({ctor: "_Tuple0"}),thunk);};
   var $try = function (parser) {    return parser;};
   var oneOf = $Native$Parse.oneOf;
   var zeroOrMore = function (parser) {
      return oneOf(_U.list([A2(andThen,parser,function (a) {    return A2(map,F2(function (x,y) {    return A2($List._op["::"],x,y);})(a),zeroOrMore(parser));})
                           ,succeed(_U.list([]))]));
   };
   var oneOrMore = function (parser) {    return A3(map2,F2(function (x,y) {    return A2($List._op["::"],x,y);}),parser,zeroOrMore(parser));};
   var string = $Native$Parse.string;
   var satisfy = $Native$Parse.satisfy;
   var lower = satisfy($Char.isLower);
   var upper = satisfy($Char.isUpper);
   var letter = satisfy(function (c) {    return $Char.isLower(c) || $Char.isUpper(c);});
   var digit = satisfy($Char.isDigit);
   var $char = function (c) {    return satisfy(F2(function (x,y) {    return _U.eq(x,y);})(c));};
   var run = $Native$Parse.run;
   var P = {ctor: "P"};
   return _elm.Parse.Combinators.values = {_op: _op
                                          ,P: P
                                          ,run: run
                                          ,$char: $char
                                          ,satisfy: satisfy
                                          ,lower: lower
                                          ,upper: upper
                                          ,letter: letter
                                          ,digit: digit
                                          ,string: string
                                          ,oneOf: oneOf
                                          ,$try: $try
                                          ,succeed: succeed
                                          ,fail: fail
                                          ,map: map
                                          ,map2: map2
                                          ,map3: map3
                                          ,map4: map4
                                          ,andThen: andThen
                                          ,zeroOrMore: zeroOrMore
                                          ,oneOrMore: oneOrMore
                                          ,lazy: lazy
                                          ,ignore1: ignore1
                                          ,ignore2: ignore2
                                          ,ignore3: ignore3
                                          ,middle: middle};
};
Elm.Parse = Elm.Parse || {};
Elm.Parse.Type = Elm.Parse.Type || {};
Elm.Parse.Type.make = function (_elm) {
   "use strict";
   _elm.Parse = _elm.Parse || {};
   _elm.Parse.Type = _elm.Parse.Type || {};
   if (_elm.Parse.Type.values) return _elm.Parse.Type.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Char = Elm.Char.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Docs$Name = Elm.Docs.Name.make(_elm),
   $Docs$Type = Elm.Docs.Type.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Parse$Combinators = Elm.Parse.Combinators.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm);
   var _op = {};
   var tuplize = function (args) {
      var _p0 = args;
      if (_p0.ctor === "::" && _p0._1.ctor === "[]") {
            return _p0._0;
         } else {
            return $Docs$Type.Tuple(args);
         }
   };
   var buildFunction = F3(function (args,currentType,remainingTypes) {
      buildFunction: while (true) {
         var _p1 = remainingTypes;
         if (_p1.ctor === "[]") {
               return $List.isEmpty(args) ? currentType : A2($Docs$Type.Function,$List.reverse(args),currentType);
            } else {
               var _v2 = A2($List._op["::"],currentType,args),_v3 = _p1._0,_v4 = _p1._1;
               args = _v2;
               currentType = _v3;
               remainingTypes = _v4;
               continue buildFunction;
            }
      }
   });
   var spaces = A2($Parse$Combinators.map,$Basics.always({ctor: "_Tuple0"}),$Parse$Combinators.zeroOrMore($Parse$Combinators.$char(_U.chr(" "))));
   var commasLeading = function (parser) {
      return $Parse$Combinators.zeroOrMore(A4($Parse$Combinators.ignore3,spaces,$Parse$Combinators.$char(_U.chr(",")),spaces,parser));
   };
   var varChar = $Parse$Combinators.satisfy(function (c) {
      return $Char.isLower(c) || ($Char.isUpper(c) || (_U.eq(c,_U.chr("_")) || (_U.eq(c,_U.chr("\'")) || $Char.isDigit(c))));
   });
   var elmVarWith = function (starter) {
      return A2($Parse$Combinators.map,
      $String.fromList,
      A3($Parse$Combinators.map2,F2(function (x,y) {    return A2($List._op["::"],x,y);}),starter,$Parse$Combinators.zeroOrMore(varChar)));
   };
   var $var = A2($Parse$Combinators.map,$Docs$Type.Var,elmVarWith($Parse$Combinators.lower));
   var nameHelp = function (seen) {
      return A2($Parse$Combinators.andThen,
      elmVarWith($Parse$Combinators.upper),
      function (str) {
         return $Parse$Combinators.oneOf(_U.list([A2($Parse$Combinators.ignore1,$Parse$Combinators.$char(_U.chr(".")),nameHelp(A2($List._op["::"],str,seen)))
                                                 ,$Parse$Combinators.succeed(A2($Docs$Name.Canonical,A2($String.join,".",$List.reverse(seen)),str))]));
      });
   };
   var name = nameHelp(_U.list([]));
   var field = $Parse$Combinators.lazy(function (_p2) {
      return A3($Parse$Combinators.map2,
      F2(function (v0,v1) {    return {ctor: "_Tuple2",_0: v0,_1: v1};}),
      elmVarWith($Parse$Combinators.lower),
      A4($Parse$Combinators.ignore3,spaces,$Parse$Combinators.$char(_U.chr(":")),spaces,tipe));
   });
   var tipe = $Parse$Combinators.lazy(function (_p3) {    return A3($Parse$Combinators.map2,buildFunction(_U.list([])),tipeTerm,arrowTerms);});
   var arrowTerms = $Parse$Combinators.lazy(function (_p4) {
      return $Parse$Combinators.zeroOrMore(A4($Parse$Combinators.ignore3,spaces,$Parse$Combinators.string("->"),spaces,tipeTerm));
   });
   var tipeTerm = $Parse$Combinators.lazy(function (_p5) {    return $Parse$Combinators.oneOf(_U.list([$var,apply,record,parenTipe]));});
   var apply = $Parse$Combinators.lazy(function (_p6) {
      return A3($Parse$Combinators.map2,$Docs$Type.Apply,name,$Parse$Combinators.zeroOrMore(A2($Parse$Combinators.ignore1,spaces,applyTerm)));
   });
   var applyTerm = $Parse$Combinators.lazy(function (_p7) {
      return $Parse$Combinators.oneOf(_U.list([$var
                                              ,A2($Parse$Combinators.map,function (n) {    return A2($Docs$Type.Apply,n,_U.list([]));},name)
                                              ,record
                                              ,parenTipe]));
   });
   var parenTipe = $Parse$Combinators.lazy(function (_p8) {
      return A2($Parse$Combinators.map,
      tuplize,
      A3($Parse$Combinators.middle,
      A2($Parse$Combinators.ignore1,$Parse$Combinators.$char(_U.chr("(")),spaces),
      $Parse$Combinators.oneOf(_U.list([A3($Parse$Combinators.map2,F2(function (x,y) {    return A2($List._op["::"],x,y);}),tipe,commasLeading(tipe))
                                       ,$Parse$Combinators.succeed(_U.list([]))])),
      A2($Parse$Combinators.ignore1,spaces,$Parse$Combinators.$char(_U.chr(")")))));
   });
   var record = $Parse$Combinators.lazy(function (_p9) {
      return A3($Parse$Combinators.middle,
      A2($Parse$Combinators.ignore1,$Parse$Combinators.$char(_U.chr("{")),spaces),
      $Parse$Combinators.oneOf(_U.list([A2($Parse$Combinators.andThen,elmVarWith($Parse$Combinators.lower),recordHelp)
                                       ,$Parse$Combinators.succeed(A2($Docs$Type.Record,_U.list([]),$Maybe.Nothing))])),
      A2($Parse$Combinators.ignore1,spaces,$Parse$Combinators.$char(_U.chr("}"))));
   });
   var recordHelp = function (lowerName) {
      return $Parse$Combinators.lazy(function (_p10) {
         return A2($Parse$Combinators.ignore1,
         spaces,
         $Parse$Combinators.oneOf(_U.list([A3($Parse$Combinators.map2,
                                          F2(function (t,rest) {
                                             return A2($Docs$Type.Record,A2($List._op["::"],{ctor: "_Tuple2",_0: lowerName,_1: t},rest),$Maybe.Nothing);
                                          }),
                                          A3($Parse$Combinators.ignore2,$Parse$Combinators.$char(_U.chr(":")),spaces,tipe),
                                          commasLeading(field))
                                          ,A2($Parse$Combinators.map,
                                          function (fields) {
                                             return A2($Docs$Type.Record,fields,$Maybe.Just(lowerName));
                                          },
                                          A3($Parse$Combinators.ignore2,
                                          $Parse$Combinators.$char(_U.chr("|")),
                                          spaces,
                                          A3($Parse$Combinators.map2,F2(function (x,y) {    return A2($List._op["::"],x,y);}),field,commasLeading(field))))])));
      });
   };
   var parse = function (tipeString) {    return A2($Parse$Combinators.run,tipe,tipeString);};
   return _elm.Parse.Type.values = {_op: _op,parse: parse};
};
Elm.Component = Elm.Component || {};
Elm.Component.PackageDocs = Elm.Component.PackageDocs || {};
Elm.Component.PackageDocs.make = function (_elm) {
   "use strict";
   _elm.Component = _elm.Component || {};
   _elm.Component.PackageDocs = _elm.Component.PackageDocs || {};
   if (_elm.Component.PackageDocs.values) return _elm.Component.PackageDocs.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Docs$Entry = Elm.Docs.Entry.make(_elm),
   $Docs$Name = Elm.Docs.Name.make(_elm),
   $Docs$Package = Elm.Docs.Package.make(_elm),
   $Docs$Type = Elm.Docs.Type.make(_elm),
   $Effects = Elm.Effects.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Html$Attributes = Elm.Html.Attributes.make(_elm),
   $Http = Elm.Http.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Jump = Elm.Native.Jump.make(_elm),
   $Page$Context = Elm.Page.Context.make(_elm),
   $Parse$Type = Elm.Parse.Type.make(_elm),
   $Regex = Elm.Regex.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Set = Elm.Set.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm),
   $Task = Elm.Task.make(_elm),
   $Utils$Markdown = Elm.Utils.Markdown.make(_elm);
   var _op = {};
   var operator = $Regex.regex("^\\([^a-zA-Z0-9]+\\)$");
   var $var = $Regex.regex("^[a-zA-Z0-9_\']+$");
   var isValue = function (str) {
      return A2($Regex.contains,$var,str) ? $Maybe.Just(str) : A2($Regex.contains,operator,str) ? $Maybe.Just(A2($String.dropLeft,
      1,
      A2($String.dropRight,1,str))) : $Maybe.Nothing;
   };
   var viewChunk = F2(function (entryView,chunk) {
      var _p0 = chunk;
      if (_p0.ctor === "Markdown") {
            return A2($Html.span,_U.list([$Html$Attributes.$class("markdown-entry")]),_U.list([$Utils$Markdown.block(_p0._0)]));
         } else {
            return entryView(_p0._0);
         }
   });
   var view = F2(function (addr,model) {
      return A2($Html.div,
      _U.list([$Html$Attributes.$class("entry-list")]),
      function () {
         var _p1 = model;
         switch (_p1.ctor)
         {case "Loading": return _U.list([A2($Html.p,_U.list([]),_U.list([$Html.text("Loading...")]))]);
            case "Failed": return _U.list([A2($Html.p,_U.list([]),_U.list([$Html.text("Documentation did not load.")]))
                                          ,A2($Html.p,_U.list([]),_U.list([$Html.text($Basics.toString(_p1._0))]))]);
            case "Readme": return _U.list([$Utils$Markdown.block(_p1._0)]);
            case "RawDocs": return A2($List._op["::"],
              A2($Html.h1,_U.list([$Html$Attributes.$class("entry-list-title")]),_U.list([$Html.text(_p1._0.name)])),
              A2($List.map,viewChunk($Docs$Entry.stringView),_p1._0.chunks));
            default: return A2($List._op["::"],
              A2($Html.h1,_U.list([$Html$Attributes.$class("entry-list-title")]),_U.list([$Html.text(_p1._0.name)])),
              A2($List.map,viewChunk($Docs$Entry.typeView(_p1._0.nameDict)),_p1._0.chunks));}
      }());
   });
   _op["=>"] = F2(function (v0,v1) {    return {ctor: "_Tuple2",_0: v0,_1: v1};});
   var stringToType = function (str) {    var _p2 = $Parse$Type.parse(str);if (_p2.ctor === "Ok") {    return _p2._0;} else {    return $Docs$Type.Var(str);}};
   var toNameDict = function (pkg) {    return A2($Dict.map,F2(function (_p3,modul) {    return $Set.fromList($Dict.keys(modul.entries));}),pkg);};
   var NoOp = {ctor: "NoOp"};
   var jumpToHash = $Effects.task(A2($Task.map,$Basics.always(NoOp),$Native$Jump.jump));
   var Fail = function (a) {    return {ctor: "Fail",_0: a};};
   var LoadReadme = function (a) {    return {ctor: "LoadReadme",_0: a};};
   var LoadParsedDocs = function (a) {    return {ctor: "LoadParsedDocs",_0: a};};
   var LoadDocs = F2(function (a,b) {    return {ctor: "LoadDocs",_0: a,_1: b};});
   var getContext = function (context) {
      var _p4 = context.moduleName;
      if (_p4.ctor === "Nothing") {
            return $Effects.task(A3($Basics.flip,
            $Task.onError,
            function (_p5) {
               return $Task.succeed(Fail(_p5));
            },
            A2($Task.map,LoadReadme,$Page$Context.getReadme(context))));
         } else {
            return $Effects.task(A3($Basics.flip,
            $Task.onError,
            function (_p6) {
               return $Task.succeed(Fail(_p6));
            },
            A2($Task.map,LoadDocs(_p4._0),$Page$Context.getDocs(context))));
         }
   };
   var Entry = function (a) {    return {ctor: "Entry",_0: a};};
   var toEntry = F2(function (moduleDocs,name) {
      var _p7 = A2($Dict.get,name,moduleDocs.entries);
      if (_p7.ctor === "Nothing") {
            return _U.crashCase("Component.PackageDocs",{start: {line: 304,column: 3},end: {line: 309,column: 20}},_p7)(A2($Basics._op["++"],
            "docs have been corrupted, could not find ",
            name));
         } else {
            return Entry(_p7._0);
         }
   });
   var Markdown = function (a) {    return {ctor: "Markdown",_0: a};};
   var chunkMap = F2(function (func,chunk) {
      var _p9 = chunk;
      if (_p9.ctor === "Markdown") {
            return Markdown(_p9._0);
         } else {
            return Entry(A2($Docs$Entry.map,func,_p9._0));
         }
   });
   var delayedTypeParse = function (chunks) {
      return $Effects.task(A2($Task.andThen,
      $Task.succeed({ctor: "_Tuple0"}),
      function (_p10) {
         return $Task.succeed(LoadParsedDocs(A2($List.map,chunkMap(stringToType),chunks)));
      }));
   };
   var subChunksHelp = F2(function (moduleDocs,parts) {
      var _p11 = parts;
      if (_p11.ctor === "[]") {
            return _U.list([]);
         } else {
            var _p17 = _p11._1;
            var _p16 = _p11._0;
            var part = $String.trim(_p16);
            var _p12 = isValue(part);
            if (_p12.ctor === "Just") {
                  return A2($List._op["::"],A2(toEntry,moduleDocs,_p12._0),A2(subChunksHelp,moduleDocs,_p17));
               } else {
                  var trimmedPart = $String.trimLeft(_p16);
                  var _p13 = $String.words(trimmedPart);
                  if (_p13.ctor === "[]") {
                        return _U.list([Markdown(A2($String.join,",",parts))]);
                     } else {
                        var _p15 = _p13._0;
                        var _p14 = isValue(_p15);
                        if (_p14.ctor === "Just") {
                              return _U.list([A2(toEntry,moduleDocs,_p14._0)
                                             ,Markdown(A2($String.dropLeft,$String.length(_p15),A2($String.join,",",A2($List._op["::"],trimmedPart,_p17))))]);
                           } else {
                              return _U.list([Markdown(A2($String.join,",",parts))]);
                           }
                     }
               }
         }
   });
   var subChunks = F2(function (moduleDocs,postDocs) {    return A2(subChunksHelp,moduleDocs,A2($String.split,",",postDocs));});
   var toChunks = function (moduleDocs) {
      var _p18 = A2($String.split,"\n@docs ",moduleDocs.comment);
      if (_p18.ctor === "[]") {
            return _U.crashCase("Component.PackageDocs",
            {start: {line: 225,column: 3},end: {line: 231,column: 54}},
            _p18)("Expecting some documented functions in this module!");
         } else {
            return A2($List._op["::"],Markdown(_p18._0),A2($List.concatMap,subChunks(moduleDocs),_p18._1));
         }
   };
   var Info = F3(function (a,b,c) {    return {name: a,nameDict: b,chunks: c};});
   var ParsedDocs = function (a) {    return {ctor: "ParsedDocs",_0: a};};
   var RawDocs = function (a) {    return {ctor: "RawDocs",_0: a};};
   var Readme = function (a) {    return {ctor: "Readme",_0: a};};
   var Failed = function (a) {    return {ctor: "Failed",_0: a};};
   var update = F2(function (action,model) {
      var _p20 = action;
      switch (_p20.ctor)
      {case "NoOp": return {ctor: "_Tuple2",_0: model,_1: $Effects.none};
         case "Fail": return {ctor: "_Tuple2",_0: Failed(_p20._0),_1: $Effects.none};
         case "LoadReadme": return {ctor: "_Tuple2",_0: Readme(_p20._0),_1: $Effects.none};
         case "LoadDocs": var _p23 = _p20._0;
           var _p22 = _p20._1;
           var _p21 = A2($Dict.get,_p23,_p22);
           if (_p21.ctor === "Just") {
                 var chunks = toChunks(_p21._0);
                 return {ctor: "_Tuple2",_0: RawDocs(A3(Info,_p23,toNameDict(_p22),chunks)),_1: delayedTypeParse(chunks)};
              } else {
                 return {ctor: "_Tuple2"
                        ,_0: Failed($Http.UnexpectedPayload(A2($Basics._op["++"],"Could not find module \'",A2($Basics._op["++"],_p23,"\'"))))
                        ,_1: $Effects.none};
              }
         default: var _p24 = model;
           if (_p24.ctor === "RawDocs") {
                 return {ctor: "_Tuple2",_0: ParsedDocs(_U.update(_p24._0,{chunks: _p20._0})),_1: jumpToHash};
              } else {
                 return {ctor: "_Tuple2",_0: Failed($Http.UnexpectedPayload("Something went wrong parsing types.")),_1: $Effects.none};
              }}
   });
   var Loading = {ctor: "Loading"};
   var init = function (context) {    return {ctor: "_Tuple2",_0: Loading,_1: getContext(context)};};
   return _elm.Component.PackageDocs.values = {_op: _op
                                              ,Loading: Loading
                                              ,Failed: Failed
                                              ,Readme: Readme
                                              ,RawDocs: RawDocs
                                              ,ParsedDocs: ParsedDocs
                                              ,Info: Info
                                              ,Markdown: Markdown
                                              ,Entry: Entry
                                              ,init: init
                                              ,LoadDocs: LoadDocs
                                              ,LoadParsedDocs: LoadParsedDocs
                                              ,LoadReadme: LoadReadme
                                              ,Fail: Fail
                                              ,NoOp: NoOp
                                              ,update: update
                                              ,toNameDict: toNameDict
                                              ,getContext: getContext
                                              ,delayedTypeParse: delayedTypeParse
                                              ,chunkMap: chunkMap
                                              ,stringToType: stringToType
                                              ,jumpToHash: jumpToHash
                                              ,view: view
                                              ,viewChunk: viewChunk
                                              ,toChunks: toChunks
                                              ,subChunks: subChunks
                                              ,subChunksHelp: subChunksHelp
                                              ,$var: $var
                                              ,operator: operator
                                              ,isValue: isValue
                                              ,toEntry: toEntry};
};
Elm.Docs = Elm.Docs || {};
Elm.Docs.Summary = Elm.Docs.Summary || {};
Elm.Docs.Summary.make = function (_elm) {
   "use strict";
   _elm.Docs = _elm.Docs || {};
   _elm.Docs.Summary = _elm.Docs.Summary || {};
   if (_elm.Docs.Summary.values) return _elm.Docs.Summary.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Docs$Version = Elm.Docs.Version.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var Summary = F3(function (a,b,c) {    return {name: a,summary: b,versions: c};});
   var decodeSummary = A4($Json$Decode.object3,
   Summary,
   A2($Json$Decode._op[":="],"name",$Json$Decode.string),
   A2($Json$Decode._op[":="],"summary",$Json$Decode.string),
   A2($Json$Decode._op[":="],"versions",$Json$Decode.list($Docs$Version.decoder)));
   var decoder = $Json$Decode.list(decodeSummary);
   return _elm.Docs.Summary.values = {_op: _op,decoder: decoder,Summary: Summary};
};
/*!
    localForage -- Offline Storage, Improved
    Version 1.2.2
    https://mozilla.github.io/localForage
    (c) 2013-2015 Mozilla, Apache License 2.0
*/
(function() {
var define, requireModule, require, requirejs;

(function() {
  var registry = {}, seen = {};

  define = function(name, deps, callback) {
    registry[name] = { deps: deps, callback: callback };
  };

  requirejs = require = requireModule = function(name) {
  requirejs._eak_seen = registry;

    if (seen[name]) { return seen[name]; }
    seen[name] = {};

    if (!registry[name]) {
      throw new Error("Could not find module " + name);
    }

    var mod = registry[name],
        deps = mod.deps,
        callback = mod.callback,
        reified = [],
        exports;

    for (var i=0, l=deps.length; i<l; i++) {
      if (deps[i] === 'exports') {
        reified.push(exports = {});
      } else {
        reified.push(requireModule(resolve(deps[i])));
      }
    }

    var value = callback.apply(this, reified);
    return seen[name] = exports || value;

    function resolve(child) {
      if (child.charAt(0) !== '.') { return child; }
      var parts = child.split("/");
      var parentBase = name.split("/").slice(0, -1);

      for (var i=0, l=parts.length; i<l; i++) {
        var part = parts[i];

        if (part === '..') { parentBase.pop(); }
        else if (part === '.') { continue; }
        else { parentBase.push(part); }
      }

      return parentBase.join("/");
    }
  };
})();

define("promise/all",
  ["./utils","exports"],
  function(__dependency1__, __exports__) {
    "use strict";
    /* global toString */

    var isArray = __dependency1__.isArray;
    var isFunction = __dependency1__.isFunction;

    /**
      Returns a promise that is fulfilled when all the given promises have been
      fulfilled, or rejected if any of them become rejected. The return promise
      is fulfilled with an array that gives all the values in the order they were
      passed in the `promises` array argument.

      Example:

      ```javascript
      var promise1 = RSVP.resolve(1);
      var promise2 = RSVP.resolve(2);
      var promise3 = RSVP.resolve(3);
      var promises = [ promise1, promise2, promise3 ];

      RSVP.all(promises).then(function(array){
        // The array here would be [ 1, 2, 3 ];
      });
      ```

      If any of the `promises` given to `RSVP.all` are rejected, the first promise
      that is rejected will be given as an argument to the returned promises's
      rejection handler. For example:

      Example:

      ```javascript
      var promise1 = RSVP.resolve(1);
      var promise2 = RSVP.reject(new Error("2"));
      var promise3 = RSVP.reject(new Error("3"));
      var promises = [ promise1, promise2, promise3 ];

      RSVP.all(promises).then(function(array){
        // Code here never runs because there are rejected promises!
      }, function(error) {
        // error.message === "2"
      });
      ```

      @method all
      @for RSVP
      @param {Array} promises
      @param {String} label
      @return {Promise} promise that is fulfilled when all `promises` have been
      fulfilled, or rejected if any of them become rejected.
    */
    function all(promises) {
      /*jshint validthis:true */
      var Promise = this;

      if (!isArray(promises)) {
        throw new TypeError('You must pass an array to all.');
      }

      return new Promise(function(resolve, reject) {
        var results = [], remaining = promises.length,
        promise;

        if (remaining === 0) {
          resolve([]);
        }

        function resolver(index) {
          return function(value) {
            resolveAll(index, value);
          };
        }

        function resolveAll(index, value) {
          results[index] = value;
          if (--remaining === 0) {
            resolve(results);
          }
        }

        for (var i = 0; i < promises.length; i++) {
          promise = promises[i];

          if (promise && isFunction(promise.then)) {
            promise.then(resolver(i), reject);
          } else {
            resolveAll(i, promise);
          }
        }
      });
    }

    __exports__.all = all;
  });
define("promise/asap",
  ["exports"],
  function(__exports__) {
    "use strict";
    var browserGlobal = (typeof window !== 'undefined') ? window : {};
    var BrowserMutationObserver = browserGlobal.MutationObserver || browserGlobal.WebKitMutationObserver;
    var local = (typeof global !== 'undefined') ? global : (this === undefined? window:this);

    // node
    function useNextTick() {
      return function() {
        process.nextTick(flush);
      };
    }

    function useMutationObserver() {
      var iterations = 0;
      var observer = new BrowserMutationObserver(flush);
      var node = document.createTextNode('');
      observer.observe(node, { characterData: true });

      return function() {
        node.data = (iterations = ++iterations % 2);
      };
    }

    function useSetTimeout() {
      return function() {
        local.setTimeout(flush, 1);
      };
    }

    var queue = [];
    function flush() {
      for (var i = 0; i < queue.length; i++) {
        var tuple = queue[i];
        var callback = tuple[0], arg = tuple[1];
        callback(arg);
      }
      queue = [];
    }

    var scheduleFlush;

    // Decide what async method to use to triggering processing of queued callbacks:
    if (typeof process !== 'undefined' && {}.toString.call(process) === '[object process]') {
      scheduleFlush = useNextTick();
    } else if (BrowserMutationObserver) {
      scheduleFlush = useMutationObserver();
    } else {
      scheduleFlush = useSetTimeout();
    }

    function asap(callback, arg) {
      var length = queue.push([callback, arg]);
      if (length === 1) {
        // If length is 1, that means that we need to schedule an async flush.
        // If additional callbacks are queued before the queue is flushed, they
        // will be processed by this flush that we are scheduling.
        scheduleFlush();
      }
    }

    __exports__.asap = asap;
  });
define("promise/config",
  ["exports"],
  function(__exports__) {
    "use strict";
    var config = {
      instrument: false
    };

    function configure(name, value) {
      if (arguments.length === 2) {
        config[name] = value;
      } else {
        return config[name];
      }
    }

    __exports__.config = config;
    __exports__.configure = configure;
  });
define("promise/polyfill",
  ["./promise","./utils","exports"],
  function(__dependency1__, __dependency2__, __exports__) {
    "use strict";
    /*global self*/
    var RSVPPromise = __dependency1__.Promise;
    var isFunction = __dependency2__.isFunction;

    function polyfill() {
      var local;

      if (typeof global !== 'undefined') {
        local = global;
      } else if (typeof window !== 'undefined' && window.document) {
        local = window;
      } else {
        local = self;
      }

      var es6PromiseSupport =
        "Promise" in local &&
        // Some of these methods are missing from
        // Firefox/Chrome experimental implementations
        "resolve" in local.Promise &&
        "reject" in local.Promise &&
        "all" in local.Promise &&
        "race" in local.Promise &&
        // Older version of the spec had a resolver object
        // as the arg rather than a function
        (function() {
          var resolve;
          new local.Promise(function(r) { resolve = r; });
          return isFunction(resolve);
        }());

      if (!es6PromiseSupport) {
        local.Promise = RSVPPromise;
      }
    }

    __exports__.polyfill = polyfill;
  });
define("promise/promise",
  ["./config","./utils","./all","./race","./resolve","./reject","./asap","exports"],
  function(__dependency1__, __dependency2__, __dependency3__, __dependency4__, __dependency5__, __dependency6__, __dependency7__, __exports__) {
    "use strict";
    var config = __dependency1__.config;
    var configure = __dependency1__.configure;
    var objectOrFunction = __dependency2__.objectOrFunction;
    var isFunction = __dependency2__.isFunction;
    var now = __dependency2__.now;
    var all = __dependency3__.all;
    var race = __dependency4__.race;
    var staticResolve = __dependency5__.resolve;
    var staticReject = __dependency6__.reject;
    var asap = __dependency7__.asap;

    var counter = 0;

    config.async = asap; // default async is asap;

    function Promise(resolver) {
      if (!isFunction(resolver)) {
        throw new TypeError('You must pass a resolver function as the first argument to the promise constructor');
      }

      if (!(this instanceof Promise)) {
        throw new TypeError("Failed to construct 'Promise': Please use the 'new' operator, this object constructor cannot be called as a function.");
      }

      this._subscribers = [];

      invokeResolver(resolver, this);
    }

    function invokeResolver(resolver, promise) {
      function resolvePromise(value) {
        resolve(promise, value);
      }

      function rejectPromise(reason) {
        reject(promise, reason);
      }

      try {
        resolver(resolvePromise, rejectPromise);
      } catch(e) {
        rejectPromise(e);
      }
    }

    function invokeCallback(settled, promise, callback, detail) {
      var hasCallback = isFunction(callback),
          value, error, succeeded, failed;

      if (hasCallback) {
        try {
          value = callback(detail);
          succeeded = true;
        } catch(e) {
          failed = true;
          error = e;
        }
      } else {
        value = detail;
        succeeded = true;
      }

      if (handleThenable(promise, value)) {
        return;
      } else if (hasCallback && succeeded) {
        resolve(promise, value);
      } else if (failed) {
        reject(promise, error);
      } else if (settled === FULFILLED) {
        resolve(promise, value);
      } else if (settled === REJECTED) {
        reject(promise, value);
      }
    }

    var PENDING   = void 0;
    var SEALED    = 0;
    var FULFILLED = 1;
    var REJECTED  = 2;

    function subscribe(parent, child, onFulfillment, onRejection) {
      var subscribers = parent._subscribers;
      var length = subscribers.length;

      subscribers[length] = child;
      subscribers[length + FULFILLED] = onFulfillment;
      subscribers[length + REJECTED]  = onRejection;
    }

    function publish(promise, settled) {
      var child, callback, subscribers = promise._subscribers, detail = promise._detail;

      for (var i = 0; i < subscribers.length; i += 3) {
        child = subscribers[i];
        callback = subscribers[i + settled];

        invokeCallback(settled, child, callback, detail);
      }

      promise._subscribers = null;
    }

    Promise.prototype = {
      constructor: Promise,

      _state: undefined,
      _detail: undefined,
      _subscribers: undefined,

      then: function(onFulfillment, onRejection) {
        var promise = this;

        var thenPromise = new this.constructor(function() {});

        if (this._state) {
          var callbacks = arguments;
          config.async(function invokePromiseCallback() {
            invokeCallback(promise._state, thenPromise, callbacks[promise._state - 1], promise._detail);
          });
        } else {
          subscribe(this, thenPromise, onFulfillment, onRejection);
        }

        return thenPromise;
      },

      'catch': function(onRejection) {
        return this.then(null, onRejection);
      }
    };

    Promise.all = all;
    Promise.race = race;
    Promise.resolve = staticResolve;
    Promise.reject = staticReject;

    function handleThenable(promise, value) {
      var then = null,
      resolved;

      try {
        if (promise === value) {
          throw new TypeError("A promises callback cannot return that same promise.");
        }

        if (objectOrFunction(value)) {
          then = value.then;

          if (isFunction(then)) {
            then.call(value, function(val) {
              if (resolved) { return true; }
              resolved = true;

              if (value !== val) {
                resolve(promise, val);
              } else {
                fulfill(promise, val);
              }
            }, function(val) {
              if (resolved) { return true; }
              resolved = true;

              reject(promise, val);
            });

            return true;
          }
        }
      } catch (error) {
        if (resolved) { return true; }
        reject(promise, error);
        return true;
      }

      return false;
    }

    function resolve(promise, value) {
      if (promise === value) {
        fulfill(promise, value);
      } else if (!handleThenable(promise, value)) {
        fulfill(promise, value);
      }
    }

    function fulfill(promise, value) {
      if (promise._state !== PENDING) { return; }
      promise._state = SEALED;
      promise._detail = value;

      config.async(publishFulfillment, promise);
    }

    function reject(promise, reason) {
      if (promise._state !== PENDING) { return; }
      promise._state = SEALED;
      promise._detail = reason;

      config.async(publishRejection, promise);
    }

    function publishFulfillment(promise) {
      publish(promise, promise._state = FULFILLED);
    }

    function publishRejection(promise) {
      publish(promise, promise._state = REJECTED);
    }

    __exports__.Promise = Promise;
  });
define("promise/race",
  ["./utils","exports"],
  function(__dependency1__, __exports__) {
    "use strict";
    /* global toString */
    var isArray = __dependency1__.isArray;

    /**
      `RSVP.race` allows you to watch a series of promises and act as soon as the
      first promise given to the `promises` argument fulfills or rejects.

      Example:

      ```javascript
      var promise1 = new RSVP.Promise(function(resolve, reject){
        setTimeout(function(){
          resolve("promise 1");
        }, 200);
      });

      var promise2 = new RSVP.Promise(function(resolve, reject){
        setTimeout(function(){
          resolve("promise 2");
        }, 100);
      });

      RSVP.race([promise1, promise2]).then(function(result){
        // result === "promise 2" because it was resolved before promise1
        // was resolved.
      });
      ```

      `RSVP.race` is deterministic in that only the state of the first completed
      promise matters. For example, even if other promises given to the `promises`
      array argument are resolved, but the first completed promise has become
      rejected before the other promises became fulfilled, the returned promise
      will become rejected:

      ```javascript
      var promise1 = new RSVP.Promise(function(resolve, reject){
        setTimeout(function(){
          resolve("promise 1");
        }, 200);
      });

      var promise2 = new RSVP.Promise(function(resolve, reject){
        setTimeout(function(){
          reject(new Error("promise 2"));
        }, 100);
      });

      RSVP.race([promise1, promise2]).then(function(result){
        // Code here never runs because there are rejected promises!
      }, function(reason){
        // reason.message === "promise2" because promise 2 became rejected before
        // promise 1 became fulfilled
      });
      ```

      @method race
      @for RSVP
      @param {Array} promises array of promises to observe
      @param {String} label optional string for describing the promise returned.
      Useful for tooling.
      @return {Promise} a promise that becomes fulfilled with the value the first
      completed promises is resolved with if the first completed promise was
      fulfilled, or rejected with the reason that the first completed promise
      was rejected with.
    */
    function race(promises) {
      /*jshint validthis:true */
      var Promise = this;

      if (!isArray(promises)) {
        throw new TypeError('You must pass an array to race.');
      }
      return new Promise(function(resolve, reject) {
        var results = [], promise;

        for (var i = 0; i < promises.length; i++) {
          promise = promises[i];

          if (promise && typeof promise.then === 'function') {
            promise.then(resolve, reject);
          } else {
            resolve(promise);
          }
        }
      });
    }

    __exports__.race = race;
  });
define("promise/reject",
  ["exports"],
  function(__exports__) {
    "use strict";
    /**
      `RSVP.reject` returns a promise that will become rejected with the passed
      `reason`. `RSVP.reject` is essentially shorthand for the following:

      ```javascript
      var promise = new RSVP.Promise(function(resolve, reject){
        reject(new Error('WHOOPS'));
      });

      promise.then(function(value){
        // Code here doesn't run because the promise is rejected!
      }, function(reason){
        // reason.message === 'WHOOPS'
      });
      ```

      Instead of writing the above, your code now simply becomes the following:

      ```javascript
      var promise = RSVP.reject(new Error('WHOOPS'));

      promise.then(function(value){
        // Code here doesn't run because the promise is rejected!
      }, function(reason){
        // reason.message === 'WHOOPS'
      });
      ```

      @method reject
      @for RSVP
      @param {Any} reason value that the returned promise will be rejected with.
      @param {String} label optional string for identifying the returned promise.
      Useful for tooling.
      @return {Promise} a promise that will become rejected with the given
      `reason`.
    */
    function reject(reason) {
      /*jshint validthis:true */
      var Promise = this;

      return new Promise(function (resolve, reject) {
        reject(reason);
      });
    }

    __exports__.reject = reject;
  });
define("promise/resolve",
  ["exports"],
  function(__exports__) {
    "use strict";
    function resolve(value) {
      /*jshint validthis:true */
      if (value && typeof value === 'object' && value.constructor === this) {
        return value;
      }

      var Promise = this;

      return new Promise(function(resolve) {
        resolve(value);
      });
    }

    __exports__.resolve = resolve;
  });
define("promise/utils",
  ["exports"],
  function(__exports__) {
    "use strict";
    function objectOrFunction(x) {
      return isFunction(x) || (typeof x === "object" && x !== null);
    }

    function isFunction(x) {
      return typeof x === "function";
    }

    function isArray(x) {
      return Object.prototype.toString.call(x) === "[object Array]";
    }

    // Date.now is not available in browsers < IE9
    // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/now#Compatibility
    var now = Date.now || function() { return new Date().getTime(); };


    __exports__.objectOrFunction = objectOrFunction;
    __exports__.isFunction = isFunction;
    __exports__.isArray = isArray;
    __exports__.now = now;
  });
requireModule('promise/polyfill').polyfill();
}());(function() {
    'use strict';

    // Sadly, the best way to save binary data in WebSQL/localStorage is serializing
    // it to Base64, so this is how we store it to prevent very strange errors with less
    // verbose ways of binary <-> string data storage.
    var BASE_CHARS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

    var SERIALIZED_MARKER = '__lfsc__:';
    var SERIALIZED_MARKER_LENGTH = SERIALIZED_MARKER.length;

    // OMG the serializations!
    var TYPE_ARRAYBUFFER = 'arbf';
    var TYPE_BLOB = 'blob';
    var TYPE_INT8ARRAY = 'si08';
    var TYPE_UINT8ARRAY = 'ui08';
    var TYPE_UINT8CLAMPEDARRAY = 'uic8';
    var TYPE_INT16ARRAY = 'si16';
    var TYPE_INT32ARRAY = 'si32';
    var TYPE_UINT16ARRAY = 'ur16';
    var TYPE_UINT32ARRAY = 'ui32';
    var TYPE_FLOAT32ARRAY = 'fl32';
    var TYPE_FLOAT64ARRAY = 'fl64';
    var TYPE_SERIALIZED_MARKER_LENGTH = SERIALIZED_MARKER_LENGTH +
                                        TYPE_ARRAYBUFFER.length;

    // Serialize a value, afterwards executing a callback (which usually
    // instructs the `setItem()` callback/promise to be executed). This is how
    // we store binary data with localStorage.
    function serialize(value, callback) {
        var valueString = '';
        if (value) {
            valueString = value.toString();
        }

        // Cannot use `value instanceof ArrayBuffer` or such here, as these
        // checks fail when running the tests using casper.js...
        //
        // TODO: See why those tests fail and use a better solution.
        if (value && (value.toString() === '[object ArrayBuffer]' ||
                      value.buffer &&
                      value.buffer.toString() === '[object ArrayBuffer]')) {
            // Convert binary arrays to a string and prefix the string with
            // a special marker.
            var buffer;
            var marker = SERIALIZED_MARKER;

            if (value instanceof ArrayBuffer) {
                buffer = value;
                marker += TYPE_ARRAYBUFFER;
            } else {
                buffer = value.buffer;

                if (valueString === '[object Int8Array]') {
                    marker += TYPE_INT8ARRAY;
                } else if (valueString === '[object Uint8Array]') {
                    marker += TYPE_UINT8ARRAY;
                } else if (valueString === '[object Uint8ClampedArray]') {
                    marker += TYPE_UINT8CLAMPEDARRAY;
                } else if (valueString === '[object Int16Array]') {
                    marker += TYPE_INT16ARRAY;
                } else if (valueString === '[object Uint16Array]') {
                    marker += TYPE_UINT16ARRAY;
                } else if (valueString === '[object Int32Array]') {
                    marker += TYPE_INT32ARRAY;
                } else if (valueString === '[object Uint32Array]') {
                    marker += TYPE_UINT32ARRAY;
                } else if (valueString === '[object Float32Array]') {
                    marker += TYPE_FLOAT32ARRAY;
                } else if (valueString === '[object Float64Array]') {
                    marker += TYPE_FLOAT64ARRAY;
                } else {
                    callback(new Error('Failed to get type for BinaryArray'));
                }
            }

            callback(marker + bufferToString(buffer));
        } else if (valueString === '[object Blob]') {
            // Conver the blob to a binaryArray and then to a string.
            var fileReader = new FileReader();

            fileReader.onload = function() {
                var str = bufferToString(this.result);

                callback(SERIALIZED_MARKER + TYPE_BLOB + str);
            };

            fileReader.readAsArrayBuffer(value);
        } else {
            try {
                callback(JSON.stringify(value));
            } catch (e) {
                window.console.error("Couldn't convert value into a JSON " +
                                     'string: ', value);

                callback(null, e);
            }
        }
    }

    // Deserialize data we've inserted into a value column/field. We place
    // special markers into our strings to mark them as encoded; this isn't
    // as nice as a meta field, but it's the only sane thing we can do whilst
    // keeping localStorage support intact.
    //
    // Oftentimes this will just deserialize JSON content, but if we have a
    // special marker (SERIALIZED_MARKER, defined above), we will extract
    // some kind of arraybuffer/binary data/typed array out of the string.
    function deserialize(value) {
        // If we haven't marked this string as being specially serialized (i.e.
        // something other than serialized JSON), we can just return it and be
        // done with it.
        if (value.substring(0,
            SERIALIZED_MARKER_LENGTH) !== SERIALIZED_MARKER) {
            return JSON.parse(value);
        }

        // The following code deals with deserializing some kind of Blob or
        // TypedArray. First we separate out the type of data we're dealing
        // with from the data itself.
        var serializedString = value.substring(TYPE_SERIALIZED_MARKER_LENGTH);
        var type = value.substring(SERIALIZED_MARKER_LENGTH,
                                   TYPE_SERIALIZED_MARKER_LENGTH);

        var buffer = stringToBuffer(serializedString);

        // Return the right type based on the code/type set during
        // serialization.
        switch (type) {
            case TYPE_ARRAYBUFFER:
                return buffer;
            case TYPE_BLOB:
                return new Blob([buffer]);
            case TYPE_INT8ARRAY:
                return new Int8Array(buffer);
            case TYPE_UINT8ARRAY:
                return new Uint8Array(buffer);
            case TYPE_UINT8CLAMPEDARRAY:
                return new Uint8ClampedArray(buffer);
            case TYPE_INT16ARRAY:
                return new Int16Array(buffer);
            case TYPE_UINT16ARRAY:
                return new Uint16Array(buffer);
            case TYPE_INT32ARRAY:
                return new Int32Array(buffer);
            case TYPE_UINT32ARRAY:
                return new Uint32Array(buffer);
            case TYPE_FLOAT32ARRAY:
                return new Float32Array(buffer);
            case TYPE_FLOAT64ARRAY:
                return new Float64Array(buffer);
            default:
                throw new Error('Unkown type: ' + type);
        }
    }

    function stringToBuffer(serializedString) {
        // Fill the string into a ArrayBuffer.
        var bufferLength = serializedString.length * 0.75;
        var len = serializedString.length;
        var i;
        var p = 0;
        var encoded1, encoded2, encoded3, encoded4;

        if (serializedString[serializedString.length - 1] === '=') {
            bufferLength--;
            if (serializedString[serializedString.length - 2] === '=') {
                bufferLength--;
            }
        }

        var buffer = new ArrayBuffer(bufferLength);
        var bytes = new Uint8Array(buffer);

        for (i = 0; i < len; i+=4) {
            encoded1 = BASE_CHARS.indexOf(serializedString[i]);
            encoded2 = BASE_CHARS.indexOf(serializedString[i+1]);
            encoded3 = BASE_CHARS.indexOf(serializedString[i+2]);
            encoded4 = BASE_CHARS.indexOf(serializedString[i+3]);

            /*jslint bitwise: true */
            bytes[p++] = (encoded1 << 2) | (encoded2 >> 4);
            bytes[p++] = ((encoded2 & 15) << 4) | (encoded3 >> 2);
            bytes[p++] = ((encoded3 & 3) << 6) | (encoded4 & 63);
        }
        return buffer;
    }

    // Converts a buffer to a string to store, serialized, in the backend
    // storage library.
    function bufferToString(buffer) {
        // base64-arraybuffer
        var bytes = new Uint8Array(buffer);
        var base64String = '';
        var i;

        for (i = 0; i < bytes.length; i += 3) {
            /*jslint bitwise: true */
            base64String += BASE_CHARS[bytes[i] >> 2];
            base64String += BASE_CHARS[((bytes[i] & 3) << 4) | (bytes[i + 1] >> 4)];
            base64String += BASE_CHARS[((bytes[i + 1] & 15) << 2) | (bytes[i + 2] >> 6)];
            base64String += BASE_CHARS[bytes[i + 2] & 63];
        }

        if ((bytes.length % 3) === 2) {
            base64String = base64String.substring(0, base64String.length - 1) + '=';
        } else if (bytes.length % 3 === 1) {
            base64String = base64String.substring(0, base64String.length - 2) + '==';
        }

        return base64String;
    }

    var localforageSerializer = {
        serialize: serialize,
        deserialize: deserialize,
        stringToBuffer: stringToBuffer,
        bufferToString: bufferToString
    };

    if (typeof module !== 'undefined' && module.exports) {
        module.exports = localforageSerializer;
    } else if (typeof define === 'function' && define.amd) {
        define('localforageSerializer', function() {
            return localforageSerializer;
        });
    } else {
        this.localforageSerializer = localforageSerializer;
    }
}).call(window);
// Some code originally from async_storage.js in
// [Gaia](https://github.com/mozilla-b2g/gaia).
(function() {
    'use strict';

    // Originally found in https://github.com/mozilla-b2g/gaia/blob/e8f624e4cc9ea945727278039b3bc9bcb9f8667a/shared/js/async_storage.js

    // Promises!
    var Promise = (typeof module !== 'undefined' && module.exports) ?
                  require('promise') : this.Promise;

    // Initialize IndexedDB; fall back to vendor-prefixed versions if needed.
    var indexedDB = indexedDB || this.indexedDB || this.webkitIndexedDB ||
                    this.mozIndexedDB || this.OIndexedDB ||
                    this.msIndexedDB;

    // If IndexedDB isn't available, we get outta here!
    if (!indexedDB) {
        return;
    }

    // Open the IndexedDB database (automatically creates one if one didn't
    // previously exist), using any options set in the config.
    function _initStorage(options) {
        var self = this;
        var dbInfo = {
            db: null
        };

        if (options) {
            for (var i in options) {
                dbInfo[i] = options[i];
            }
        }

        return new Promise(function(resolve, reject) {
            var openreq = indexedDB.open(dbInfo.name, dbInfo.version);
            openreq.onerror = function() {
                reject(openreq.error);
            };
            openreq.onupgradeneeded = function() {
                // First time setup: create an empty object store
                openreq.result.createObjectStore(dbInfo.storeName);
            };
            openreq.onsuccess = function() {
                dbInfo.db = openreq.result;
                self._dbInfo = dbInfo;
                resolve();
            };
        });
    }

    function getItem(key, callback) {
        var self = this;

        // Cast the key to a string, as that's all we can set as a key.
        if (typeof key !== 'string') {
            window.console.warn(key +
                                ' used as a key, but it is not a string.');
            key = String(key);
        }

        var promise = new Promise(function(resolve, reject) {
            self.ready().then(function() {
                var dbInfo = self._dbInfo;
                var store = dbInfo.db.transaction(dbInfo.storeName, 'readonly')
                    .objectStore(dbInfo.storeName);
                var req = store.get(key);

                req.onsuccess = function() {
                    var value = req.result;
                    if (value === undefined) {
                        value = null;
                    }

                    resolve(value);
                };

                req.onerror = function() {
                    reject(req.error);
                };
            })["catch"](reject);
        });

        executeDeferedCallback(promise, callback);
        return promise;
    }

    // Iterate over all items stored in database.
    function iterate(iterator, callback) {
        var self = this;

        var promise = new Promise(function(resolve, reject) {
            self.ready().then(function() {
                var dbInfo = self._dbInfo;
                var store = dbInfo.db.transaction(dbInfo.storeName, 'readonly')
                                     .objectStore(dbInfo.storeName);

                var req = store.openCursor();
                var iterationNumber = 1;

                req.onsuccess = function() {
                    var cursor = req.result;

                    if (cursor) {
                        var result = iterator(cursor.value, cursor.key, iterationNumber++);

                        if (result !== void(0)) {
                            resolve(result);
                        } else {
                            cursor["continue"]();
                        }
                    } else {
                        resolve();
                    }
                };

                req.onerror = function() {
                    reject(req.error);
                };
            })["catch"](reject);
        });

        executeDeferedCallback(promise, callback);

        return promise;
    }

    function setItem(key, value, callback) {
        var self = this;

        // Cast the key to a string, as that's all we can set as a key.
        if (typeof key !== 'string') {
            window.console.warn(key +
                                ' used as a key, but it is not a string.');
            key = String(key);
        }

        var promise = new Promise(function(resolve, reject) {
            self.ready().then(function() {
                var dbInfo = self._dbInfo;
                var transaction = dbInfo.db.transaction(dbInfo.storeName, 'readwrite');
                var store = transaction.objectStore(dbInfo.storeName);

                // The reason we don't _save_ null is because IE 10 does
                // not support saving the `null` type in IndexedDB. How
                // ironic, given the bug below!
                // See: https://github.com/mozilla/localForage/issues/161
                if (value === null) {
                    value = undefined;
                }

                var req = store.put(value, key);
                transaction.oncomplete = function() {
                    // Cast to undefined so the value passed to
                    // callback/promise is the same as what one would get out
                    // of `getItem()` later. This leads to some weirdness
                    // (setItem('foo', undefined) will return `null`), but
                    // it's not my fault localStorage is our baseline and that
                    // it's weird.
                    if (value === undefined) {
                        value = null;
                    }

                    resolve(value);
                };
                transaction.onabort = transaction.onerror = function() {
                    reject(req.error);
                };
            })["catch"](reject);
        });

        executeDeferedCallback(promise, callback);
        return promise;
    }

    function removeItem(key, callback) {
        var self = this;

        // Cast the key to a string, as that's all we can set as a key.
        if (typeof key !== 'string') {
            window.console.warn(key +
                                ' used as a key, but it is not a string.');
            key = String(key);
        }

        var promise = new Promise(function(resolve, reject) {
            self.ready().then(function() {
                var dbInfo = self._dbInfo;
                var transaction = dbInfo.db.transaction(dbInfo.storeName, 'readwrite');
                var store = transaction.objectStore(dbInfo.storeName);

                // We use a Grunt task to make this safe for IE and some
                // versions of Android (including those used by Cordova).
                // Normally IE won't like `.delete()` and will insist on
                // using `['delete']()`, but we have a build step that
                // fixes this for us now.
                var req = store["delete"](key);
                transaction.oncomplete = function() {
                    resolve();
                };

                transaction.onerror = function() {
                    reject(req.error);
                };

                // The request will be aborted if we've exceeded our storage
                // space. In this case, we will reject with a specific
                // "QuotaExceededError".
                transaction.onabort = function(event) {
                    var error = event.target.error;
                    if (error === 'QuotaExceededError') {
                        reject(error);
                    }
                };
            })["catch"](reject);
        });

        executeDeferedCallback(promise, callback);
        return promise;
    }

    function clear(callback) {
        var self = this;

        var promise = new Promise(function(resolve, reject) {
            self.ready().then(function() {
                var dbInfo = self._dbInfo;
                var transaction = dbInfo.db.transaction(dbInfo.storeName, 'readwrite');
                var store = transaction.objectStore(dbInfo.storeName);
                var req = store.clear();

                transaction.oncomplete = function() {
                    resolve();
                };

                transaction.onabort = transaction.onerror = function() {
                    reject(req.error);
                };
            })["catch"](reject);
        });

        executeDeferedCallback(promise, callback);
        return promise;
    }

    function length(callback) {
        var self = this;

        var promise = new Promise(function(resolve, reject) {
            self.ready().then(function() {
                var dbInfo = self._dbInfo;
                var store = dbInfo.db.transaction(dbInfo.storeName, 'readonly')
                              .objectStore(dbInfo.storeName);
                var req = store.count();

                req.onsuccess = function() {
                    resolve(req.result);
                };

                req.onerror = function() {
                    reject(req.error);
                };
            })["catch"](reject);
        });

        executeCallback(promise, callback);
        return promise;
    }

    function key(n, callback) {
        var self = this;

        var promise = new Promise(function(resolve, reject) {
            if (n < 0) {
                resolve(null);

                return;
            }

            self.ready().then(function() {
                var dbInfo = self._dbInfo;
                var store = dbInfo.db.transaction(dbInfo.storeName, 'readonly')
                              .objectStore(dbInfo.storeName);

                var advanced = false;
                var req = store.openCursor();
                req.onsuccess = function() {
                    var cursor = req.result;
                    if (!cursor) {
                        // this means there weren't enough keys
                        resolve(null);

                        return;
                    }

                    if (n === 0) {
                        // We have the first key, return it if that's what they
                        // wanted.
                        resolve(cursor.key);
                    } else {
                        if (!advanced) {
                            // Otherwise, ask the cursor to skip ahead n
                            // records.
                            advanced = true;
                            cursor.advance(n);
                        } else {
                            // When we get here, we've got the nth key.
                            resolve(cursor.key);
                        }
                    }
                };

                req.onerror = function() {
                    reject(req.error);
                };
            })["catch"](reject);
        });

        executeCallback(promise, callback);
        return promise;
    }

    function keys(callback) {
        var self = this;

        var promise = new Promise(function(resolve, reject) {
            self.ready().then(function() {
                var dbInfo = self._dbInfo;
                var store = dbInfo.db.transaction(dbInfo.storeName, 'readonly')
                              .objectStore(dbInfo.storeName);

                var req = store.openCursor();
                var keys = [];

                req.onsuccess = function() {
                    var cursor = req.result;

                    if (!cursor) {
                        resolve(keys);
                        return;
                    }

                    keys.push(cursor.key);
                    cursor["continue"]();
                };

                req.onerror = function() {
                    reject(req.error);
                };
            })["catch"](reject);
        });

        executeCallback(promise, callback);
        return promise;
    }

    function executeCallback(promise, callback) {
        if (callback) {
            promise.then(function(result) {
                callback(null, result);
            }, function(error) {
                callback(error);
            });
        }
    }

    function executeDeferedCallback(promise, callback) {
        if (callback) {
            promise.then(function(result) {
                deferCallback(callback, result);
            }, function(error) {
                callback(error);
            });
        }
    }

    // Under Chrome the callback is called before the changes (save, clear)
    // are actually made. So we use a defer function which wait that the
    // call stack to be empty.
    // For more info : https://github.com/mozilla/localForage/issues/175
    // Pull request : https://github.com/mozilla/localForage/pull/178
    function deferCallback(callback, result) {
        if (callback) {
            return setTimeout(function() {
                return callback(null, result);
            }, 0);
        }
    }

    var asyncStorage = {
        _driver: 'asyncStorage',
        _initStorage: _initStorage,
        iterate: iterate,
        getItem: getItem,
        setItem: setItem,
        removeItem: removeItem,
        clear: clear,
        length: length,
        key: key,
        keys: keys
    };

    if (typeof module !== 'undefined' && module.exports) {
        module.exports = asyncStorage;
    } else if (typeof define === 'function' && define.amd) {
        define('asyncStorage', function() {
            return asyncStorage;
        });
    } else {
        this.asyncStorage = asyncStorage;
    }
}).call(window);
// If IndexedDB isn't available, we'll fall back to localStorage.
// Note that this will have considerable performance and storage
// side-effects (all data will be serialized on save and only data that
// can be converted to a string via `JSON.stringify()` will be saved).
(function() {
    'use strict';

    // Promises!
    var Promise = (typeof module !== 'undefined' && module.exports) ?
                  require('promise') : this.Promise;

    var globalObject = this;
    var serializer = null;
    var localStorage = null;

    // If the app is running inside a Google Chrome packaged webapp, or some
    // other context where localStorage isn't available, we don't use
    // localStorage. This feature detection is preferred over the old
    // `if (window.chrome && window.chrome.runtime)` code.
    // See: https://github.com/mozilla/localForage/issues/68
    try {
        // If localStorage isn't available, we get outta here!
        // This should be inside a try catch
        if (!this.localStorage || !('setItem' in this.localStorage)) {
            return;
        }
        // Initialize localStorage and create a variable to use throughout
        // the code.
        localStorage = this.localStorage;
    } catch (e) {
        return;
    }

    var ModuleType = {
        DEFINE: 1,
        EXPORT: 2,
        WINDOW: 3
    };

    // Attaching to window (i.e. no module loader) is the assumed,
    // simple default.
    var moduleType = ModuleType.WINDOW;

    // Find out what kind of module setup we have; if none, we'll just attach
    // localForage to the main window.
    if (typeof module !== 'undefined' && module.exports) {
        moduleType = ModuleType.EXPORT;
    } else if (typeof define === 'function' && define.amd) {
        moduleType = ModuleType.DEFINE;
    }

    // Config the localStorage backend, using options set in the config.
    function _initStorage(options) {
        var self = this;
        var dbInfo = {};
        if (options) {
            for (var i in options) {
                dbInfo[i] = options[i];
            }
        }

        dbInfo.keyPrefix = dbInfo.name + '/';

        self._dbInfo = dbInfo;

        var serializerPromise = new Promise(function(resolve/*, reject*/) {
            // We allow localForage to be declared as a module or as a
            // library available without AMD/require.js.
            if (moduleType === ModuleType.DEFINE) {
                require(['localforageSerializer'], resolve);
            } else if (moduleType === ModuleType.EXPORT) {
                // Making it browserify friendly
                resolve(require('./../utils/serializer'));
            } else {
                resolve(globalObject.localforageSerializer);
            }
        });

        return serializerPromise.then(function(lib) {
            serializer = lib;
            return Promise.resolve();
        });
    }

    // Remove all keys from the datastore, effectively destroying all data in
    // the app's key/value store!
    function clear(callback) {
        var self = this;
        var promise = self.ready().then(function() {
            var keyPrefix = self._dbInfo.keyPrefix;

            for (var i = localStorage.length - 1; i >= 0; i--) {
                var key = localStorage.key(i);

                if (key.indexOf(keyPrefix) === 0) {
                    localStorage.removeItem(key);
                }
            }
        });

        executeCallback(promise, callback);
        return promise;
    }

    // Retrieve an item from the store. Unlike the original async_storage
    // library in Gaia, we don't modify return values at all. If a key's value
    // is `undefined`, we pass that value to the callback function.
    function getItem(key, callback) {
        var self = this;

        // Cast the key to a string, as that's all we can set as a key.
        if (typeof key !== 'string') {
            window.console.warn(key +
                                ' used as a key, but it is not a string.');
            key = String(key);
        }

        var promise = self.ready().then(function() {
            var dbInfo = self._dbInfo;
            var result = localStorage.getItem(dbInfo.keyPrefix + key);

            // If a result was found, parse it from the serialized
            // string into a JS object. If result isn't truthy, the key
            // is likely undefined and we'll pass it straight to the
            // callback.
            if (result) {
                result = serializer.deserialize(result);
            }

            return result;
        });

        executeCallback(promise, callback);
        return promise;
    }

    // Iterate over all items in the store.
    function iterate(iterator, callback) {
        var self = this;

        var promise = self.ready().then(function() {
            var keyPrefix = self._dbInfo.keyPrefix;
            var keyPrefixLength = keyPrefix.length;
            var length = localStorage.length;

            for (var i = 0; i < length; i++) {
                var key = localStorage.key(i);
                var value = localStorage.getItem(key);

                // If a result was found, parse it from the serialized
                // string into a JS object. If result isn't truthy, the
                // key is likely undefined and we'll pass it straight
                // to the iterator.
                if (value) {
                    value = serializer.deserialize(value);
                }

                value = iterator(value, key.substring(keyPrefixLength), i + 1);

                if (value !== void(0)) {
                    return value;
                }
            }
        });

        executeCallback(promise, callback);
        return promise;
    }

    // Same as localStorage's key() method, except takes a callback.
    function key(n, callback) {
        var self = this;
        var promise = self.ready().then(function() {
            var dbInfo = self._dbInfo;
            var result;
            try {
                result = localStorage.key(n);
            } catch (error) {
                result = null;
            }

            // Remove the prefix from the key, if a key is found.
            if (result) {
                result = result.substring(dbInfo.keyPrefix.length);
            }

            return result;
        });

        executeCallback(promise, callback);
        return promise;
    }

    function keys(callback) {
        var self = this;
        var promise = self.ready().then(function() {
            var dbInfo = self._dbInfo;
            var length = localStorage.length;
            var keys = [];

            for (var i = 0; i < length; i++) {
                if (localStorage.key(i).indexOf(dbInfo.keyPrefix) === 0) {
                    keys.push(localStorage.key(i).substring(dbInfo.keyPrefix.length));
                }
            }

            return keys;
        });

        executeCallback(promise, callback);
        return promise;
    }

    // Supply the number of keys in the datastore to the callback function.
    function length(callback) {
        var self = this;
        var promise = self.keys().then(function(keys) {
            return keys.length;
        });

        executeCallback(promise, callback);
        return promise;
    }

    // Remove an item from the store, nice and simple.
    function removeItem(key, callback) {
        var self = this;

        // Cast the key to a string, as that's all we can set as a key.
        if (typeof key !== 'string') {
            window.console.warn(key +
                                ' used as a key, but it is not a string.');
            key = String(key);
        }

        var promise = self.ready().then(function() {
            var dbInfo = self._dbInfo;
            localStorage.removeItem(dbInfo.keyPrefix + key);
        });

        executeCallback(promise, callback);
        return promise;
    }

    // Set a key's value and run an optional callback once the value is set.
    // Unlike Gaia's implementation, the callback function is passed the value,
    // in case you want to operate on that value only after you're sure it
    // saved, or something like that.
    function setItem(key, value, callback) {
        var self = this;

        // Cast the key to a string, as that's all we can set as a key.
        if (typeof key !== 'string') {
            window.console.warn(key +
                                ' used as a key, but it is not a string.');
            key = String(key);
        }

        var promise = self.ready().then(function() {
            // Convert undefined values to null.
            // https://github.com/mozilla/localForage/pull/42
            if (value === undefined) {
                value = null;
            }

            // Save the original value to pass to the callback.
            var originalValue = value;

            return new Promise(function(resolve, reject) {
                serializer.serialize(value, function(value, error) {
                    if (error) {
                        reject(error);
                    } else {
                        try {
                            var dbInfo = self._dbInfo;
                            localStorage.setItem(dbInfo.keyPrefix + key, value);
                            resolve(originalValue);
                        } catch (e) {
                            // localStorage capacity exceeded.
                            // TODO: Make this a specific error/event.
                            if (e.name === 'QuotaExceededError' ||
                                e.name === 'NS_ERROR_DOM_QUOTA_REACHED') {
                                reject(e);
                            }
                            reject(e);
                        }
                    }
                });
            });
        });

        executeCallback(promise, callback);
        return promise;
    }

    function executeCallback(promise, callback) {
        if (callback) {
            promise.then(function(result) {
                callback(null, result);
            }, function(error) {
                callback(error);
            });
        }
    }

    var localStorageWrapper = {
        _driver: 'localStorageWrapper',
        _initStorage: _initStorage,
        // Default API, from Gaia/localStorage.
        iterate: iterate,
        getItem: getItem,
        setItem: setItem,
        removeItem: removeItem,
        clear: clear,
        length: length,
        key: key,
        keys: keys
    };

    if (moduleType === ModuleType.EXPORT) {
        module.exports = localStorageWrapper;
    } else if (moduleType === ModuleType.DEFINE) {
        define('localStorageWrapper', function() {
            return localStorageWrapper;
        });
    } else {
        this.localStorageWrapper = localStorageWrapper;
    }
}).call(window);
/*
 * Includes code from:
 *
 * base64-arraybuffer
 * https://github.com/niklasvh/base64-arraybuffer
 *
 * Copyright (c) 2012 Niklas von Hertzen
 * Licensed under the MIT license.
 */
(function() {
    'use strict';

    // Promises!
    var Promise = (typeof module !== 'undefined' && module.exports) ?
                  require('promise') : this.Promise;

    var globalObject = this;
    var serializer = null;
    var openDatabase = this.openDatabase;

    // If WebSQL methods aren't available, we can stop now.
    if (!openDatabase) {
        return;
    }

    var ModuleType = {
        DEFINE: 1,
        EXPORT: 2,
        WINDOW: 3
    };

    // Attaching to window (i.e. no module loader) is the assumed,
    // simple default.
    var moduleType = ModuleType.WINDOW;

    // Find out what kind of module setup we have; if none, we'll just attach
    // localForage to the main window.
    if (typeof module !== 'undefined' && module.exports) {
        moduleType = ModuleType.EXPORT;
    } else if (typeof define === 'function' && define.amd) {
        moduleType = ModuleType.DEFINE;
    }

    // Open the WebSQL database (automatically creates one if one didn't
    // previously exist), using any options set in the config.
    function _initStorage(options) {
        var self = this;
        var dbInfo = {
            db: null
        };

        if (options) {
            for (var i in options) {
                dbInfo[i] = typeof(options[i]) !== 'string' ?
                            options[i].toString() : options[i];
            }
        }

        var serializerPromise = new Promise(function(resolve/*, reject*/) {
            // We allow localForage to be declared as a module or as a
            // library available without AMD/require.js.
            if (moduleType === ModuleType.DEFINE) {
                require(['localforageSerializer'], resolve);
            } else if (moduleType === ModuleType.EXPORT) {
                // Making it browserify friendly
                resolve(require('./../utils/serializer'));
            } else {
                resolve(globalObject.localforageSerializer);
            }
        });

        var dbInfoPromise = new Promise(function(resolve, reject) {
            // Open the database; the openDatabase API will automatically
            // create it for us if it doesn't exist.
            try {
                dbInfo.db = openDatabase(dbInfo.name, String(dbInfo.version),
                                         dbInfo.description, dbInfo.size);
            } catch (e) {
                return self.setDriver(self.LOCALSTORAGE).then(function() {
    return self._initStorage(options);
}).then(resolve)["catch"](reject);
            }

            // Create our key/value table if it doesn't exist.
            dbInfo.db.transaction(function(t) {
                t.executeSql('CREATE TABLE IF NOT EXISTS ' + dbInfo.storeName +
                             ' (id INTEGER PRIMARY KEY, key unique, value)', [],
                             function() {
                    self._dbInfo = dbInfo;
                    resolve();
                }, function(t, error) {
                    reject(error);
                });
            });
        });

        return serializerPromise.then(function(lib) {
            serializer = lib;
            return dbInfoPromise;
        });
    }

    function getItem(key, callback) {
        var self = this;

        // Cast the key to a string, as that's all we can set as a key.
        if (typeof key !== 'string') {
            window.console.warn(key +
                                ' used as a key, but it is not a string.');
            key = String(key);
        }

        var promise = new Promise(function(resolve, reject) {
            self.ready().then(function() {
                var dbInfo = self._dbInfo;
                dbInfo.db.transaction(function(t) {
                    t.executeSql('SELECT * FROM ' + dbInfo.storeName +
                                 ' WHERE key = ? LIMIT 1', [key],
                                 function(t, results) {
                        var result = results.rows.length ?
                                     results.rows.item(0).value : null;

                        // Check to see if this is serialized content we need to
                        // unpack.
                        if (result) {
                            result = serializer.deserialize(result);
                        }

                        resolve(result);
                    }, function(t, error) {

                        reject(error);
                    });
                });
            })["catch"](reject);
        });

        executeCallback(promise, callback);
        return promise;
    }

    function iterate(iterator, callback) {
        var self = this;

        var promise = new Promise(function(resolve, reject) {
            self.ready().then(function() {
                var dbInfo = self._dbInfo;

                dbInfo.db.transaction(function(t) {
                    t.executeSql('SELECT * FROM ' + dbInfo.storeName, [],
                        function(t, results) {
                            var rows = results.rows;
                            var length = rows.length;

                            for (var i = 0; i < length; i++) {
                                var item = rows.item(i);
                                var result = item.value;

                                // Check to see if this is serialized content
                                // we need to unpack.
                                if (result) {
                                    result = serializer.deserialize(result);
                                }

                                result = iterator(result, item.key, i + 1);

                                // void(0) prevents problems with redefinition
                                // of `undefined`.
                                if (result !== void(0)) {
                                    resolve(result);
                                    return;
                                }
                            }

                            resolve();
                        }, function(t, error) {
                            reject(error);
                        });
                });
            })["catch"](reject);
        });

        executeCallback(promise, callback);
        return promise;
    }

    function setItem(key, value, callback) {
        var self = this;

        // Cast the key to a string, as that's all we can set as a key.
        if (typeof key !== 'string') {
            window.console.warn(key +
                                ' used as a key, but it is not a string.');
            key = String(key);
        }

        var promise = new Promise(function(resolve, reject) {
            self.ready().then(function() {
                // The localStorage API doesn't return undefined values in an
                // "expected" way, so undefined is always cast to null in all
                // drivers. See: https://github.com/mozilla/localForage/pull/42
                if (value === undefined) {
                    value = null;
                }

                // Save the original value to pass to the callback.
                var originalValue = value;

                serializer.serialize(value, function(value, error) {
                    if (error) {
                        reject(error);
                    } else {
                        var dbInfo = self._dbInfo;
                        dbInfo.db.transaction(function(t) {
                            t.executeSql('INSERT OR REPLACE INTO ' +
                                         dbInfo.storeName +
                                         ' (key, value) VALUES (?, ?)',
                                         [key, value], function() {
                                resolve(originalValue);
                            }, function(t, error) {
                                reject(error);
                            });
                        }, function(sqlError) { // The transaction failed; check
                                                // to see if it's a quota error.
                            if (sqlError.code === sqlError.QUOTA_ERR) {
                                // We reject the callback outright for now, but
                                // it's worth trying to re-run the transaction.
                                // Even if the user accepts the prompt to use
                                // more storage on Safari, this error will
                                // be called.
                                //
                                // TODO: Try to re-run the transaction.
                                reject(sqlError);
                            }
                        });
                    }
                });
            })["catch"](reject);
        });

        executeCallback(promise, callback);
        return promise;
    }

    function removeItem(key, callback) {
        var self = this;

        // Cast the key to a string, as that's all we can set as a key.
        if (typeof key !== 'string') {
            window.console.warn(key +
                                ' used as a key, but it is not a string.');
            key = String(key);
        }

        var promise = new Promise(function(resolve, reject) {
            self.ready().then(function() {
                var dbInfo = self._dbInfo;
                dbInfo.db.transaction(function(t) {
                    t.executeSql('DELETE FROM ' + dbInfo.storeName +
                                 ' WHERE key = ?', [key], function() {

                        resolve();
                    }, function(t, error) {

                        reject(error);
                    });
                });
            })["catch"](reject);
        });

        executeCallback(promise, callback);
        return promise;
    }

    // Deletes every item in the table.
    // TODO: Find out if this resets the AUTO_INCREMENT number.
    function clear(callback) {
        var self = this;

        var promise = new Promise(function(resolve, reject) {
            self.ready().then(function() {
                var dbInfo = self._dbInfo;
                dbInfo.db.transaction(function(t) {
                    t.executeSql('DELETE FROM ' + dbInfo.storeName, [],
                                 function() {
                        resolve();
                    }, function(t, error) {
                        reject(error);
                    });
                });
            })["catch"](reject);
        });

        executeCallback(promise, callback);
        return promise;
    }

    // Does a simple `COUNT(key)` to get the number of items stored in
    // localForage.
    function length(callback) {
        var self = this;

        var promise = new Promise(function(resolve, reject) {
            self.ready().then(function() {
                var dbInfo = self._dbInfo;
                dbInfo.db.transaction(function(t) {
                    // Ahhh, SQL makes this one soooooo easy.
                    t.executeSql('SELECT COUNT(key) as c FROM ' +
                                 dbInfo.storeName, [], function(t, results) {
                        var result = results.rows.item(0).c;

                        resolve(result);
                    }, function(t, error) {

                        reject(error);
                    });
                });
            })["catch"](reject);
        });

        executeCallback(promise, callback);
        return promise;
    }

    // Return the key located at key index X; essentially gets the key from a
    // `WHERE id = ?`. This is the most efficient way I can think to implement
    // this rarely-used (in my experience) part of the API, but it can seem
    // inconsistent, because we do `INSERT OR REPLACE INTO` on `setItem()`, so
    // the ID of each key will change every time it's updated. Perhaps a stored
    // procedure for the `setItem()` SQL would solve this problem?
    // TODO: Don't change ID on `setItem()`.
    function key(n, callback) {
        var self = this;

        var promise = new Promise(function(resolve, reject) {
            self.ready().then(function() {
                var dbInfo = self._dbInfo;
                dbInfo.db.transaction(function(t) {
                    t.executeSql('SELECT key FROM ' + dbInfo.storeName +
                                 ' WHERE id = ? LIMIT 1', [n + 1],
                                 function(t, results) {
                        var result = results.rows.length ?
                                     results.rows.item(0).key : null;
                        resolve(result);
                    }, function(t, error) {
                        reject(error);
                    });
                });
            })["catch"](reject);
        });

        executeCallback(promise, callback);
        return promise;
    }

    function keys(callback) {
        var self = this;

        var promise = new Promise(function(resolve, reject) {
            self.ready().then(function() {
                var dbInfo = self._dbInfo;
                dbInfo.db.transaction(function(t) {
                    t.executeSql('SELECT key FROM ' + dbInfo.storeName, [],
                                 function(t, results) {
                        var keys = [];

                        for (var i = 0; i < results.rows.length; i++) {
                            keys.push(results.rows.item(i).key);
                        }

                        resolve(keys);
                    }, function(t, error) {

                        reject(error);
                    });
                });
            })["catch"](reject);
        });

        executeCallback(promise, callback);
        return promise;
    }

    function executeCallback(promise, callback) {
        if (callback) {
            promise.then(function(result) {
                callback(null, result);
            }, function(error) {
                callback(error);
            });
        }
    }

    var webSQLStorage = {
        _driver: 'webSQLStorage',
        _initStorage: _initStorage,
        iterate: iterate,
        getItem: getItem,
        setItem: setItem,
        removeItem: removeItem,
        clear: clear,
        length: length,
        key: key,
        keys: keys
    };

    if (moduleType === ModuleType.DEFINE) {
        define('webSQLStorage', function() {
            return webSQLStorage;
        });
    } else if (moduleType === ModuleType.EXPORT) {
        module.exports = webSQLStorage;
    } else {
        this.webSQLStorage = webSQLStorage;
    }
}).call(window);
(function() {
    'use strict';

    // Promises!
    var Promise = (typeof module !== 'undefined' && module.exports) ?
                  require('promise') : this.Promise;

    // Custom drivers are stored here when `defineDriver()` is called.
    // They are shared across all instances of localForage.
    var CustomDrivers = {};

    var DriverType = {
        INDEXEDDB: 'asyncStorage',
        LOCALSTORAGE: 'localStorageWrapper',
        WEBSQL: 'webSQLStorage'
    };

    var DefaultDriverOrder = [
        DriverType.INDEXEDDB,
        DriverType.WEBSQL,
        DriverType.LOCALSTORAGE
    ];

    var LibraryMethods = [
        'clear',
        'getItem',
        'iterate',
        'key',
        'keys',
        'length',
        'removeItem',
        'setItem'
    ];

    var ModuleType = {
        DEFINE: 1,
        EXPORT: 2,
        WINDOW: 3
    };

    var DefaultConfig = {
        description: '',
        driver: DefaultDriverOrder.slice(),
        name: 'localforage',
        // Default DB size is _JUST UNDER_ 5MB, as it's the highest size
        // we can use without a prompt.
        size: 4980736,
        storeName: 'keyvaluepairs',
        version: 1.0
    };

    // Attaching to window (i.e. no module loader) is the assumed,
    // simple default.
    var moduleType = ModuleType.WINDOW;

    // Find out what kind of module setup we have; if none, we'll just attach
    // localForage to the main window.
    if (typeof module !== 'undefined' && module.exports) {
        moduleType = ModuleType.EXPORT;
    } else if (typeof define === 'function' && define.amd) {
        moduleType = ModuleType.DEFINE;
    }

    // Check to see if IndexedDB is available and if it is the latest
    // implementation; it's our preferred backend library. We use "_spec_test"
    // as the name of the database because it's not the one we'll operate on,
    // but it's useful to make sure its using the right spec.
    // See: https://github.com/mozilla/localForage/issues/128
    var driverSupport = (function(self) {
        // Initialize IndexedDB; fall back to vendor-prefixed versions
        // if needed.
        var indexedDB = indexedDB || self.indexedDB || self.webkitIndexedDB ||
                        self.mozIndexedDB || self.OIndexedDB ||
                        self.msIndexedDB;

        var result = {};

        result[DriverType.WEBSQL] = !!self.openDatabase;
        result[DriverType.INDEXEDDB] = !!(function() {
            // We mimic PouchDB here; just UA test for Safari (which, as of
            // iOS 8/Yosemite, doesn't properly support IndexedDB).
            // IndexedDB support is broken and different from Blink's.
            // This is faster than the test case (and it's sync), so we just
            // do this. *SIGH*
            // http://bl.ocks.org/nolanlawson/raw/c83e9039edf2278047e9/
            //
            // We test for openDatabase because IE Mobile identifies itself
            // as Safari. Oh the lulz...
            if (typeof self.openDatabase !== 'undefined' && self.navigator &&
                self.navigator.userAgent &&
                /Safari/.test(self.navigator.userAgent) &&
                !/Chrome/.test(self.navigator.userAgent)) {
                return false;
            }
            try {
                return indexedDB &&
                       typeof indexedDB.open === 'function' &&
                       // Some Samsung/HTC Android 4.0-4.3 devices
                       // have older IndexedDB specs; if this isn't available
                       // their IndexedDB is too old for us to use.
                       // (Replaces the onupgradeneeded test.)
                       typeof self.IDBKeyRange !== 'undefined';
            } catch (e) {
                return false;
            }
        })();

        result[DriverType.LOCALSTORAGE] = !!(function() {
            try {
                return (self.localStorage &&
                        ('setItem' in self.localStorage) &&
                        (self.localStorage.setItem));
            } catch (e) {
                return false;
            }
        })();

        return result;
    })(this);

    var isArray = Array.isArray || function(arg) {
        return Object.prototype.toString.call(arg) === '[object Array]';
    };

    function callWhenReady(localForageInstance, libraryMethod) {
        localForageInstance[libraryMethod] = function() {
            var _args = arguments;
            return localForageInstance.ready().then(function() {
                return localForageInstance[libraryMethod].apply(localForageInstance, _args);
            });
        };
    }

    function extend() {
        for (var i = 1; i < arguments.length; i++) {
            var arg = arguments[i];

            if (arg) {
                for (var key in arg) {
                    if (arg.hasOwnProperty(key)) {
                        if (isArray(arg[key])) {
                            arguments[0][key] = arg[key].slice();
                        } else {
                            arguments[0][key] = arg[key];
                        }
                    }
                }
            }
        }

        return arguments[0];
    }

    function isLibraryDriver(driverName) {
        for (var driver in DriverType) {
            if (DriverType.hasOwnProperty(driver) &&
                DriverType[driver] === driverName) {
                return true;
            }
        }

        return false;
    }

    var globalObject = this;

    function LocalForage(options) {
        this._config = extend({}, DefaultConfig, options);
        this._driverSet = null;
        this._ready = false;
        this._dbInfo = null;

        // Add a stub for each driver API method that delays the call to the
        // corresponding driver method until localForage is ready. These stubs
        // will be replaced by the driver methods as soon as the driver is
        // loaded, so there is no performance impact.
        for (var i = 0; i < LibraryMethods.length; i++) {
            callWhenReady(this, LibraryMethods[i]);
        }

        this.setDriver(this._config.driver);
    }

    LocalForage.prototype.INDEXEDDB = DriverType.INDEXEDDB;
    LocalForage.prototype.LOCALSTORAGE = DriverType.LOCALSTORAGE;
    LocalForage.prototype.WEBSQL = DriverType.WEBSQL;

    // Set any config values for localForage; can be called anytime before
    // the first API call (e.g. `getItem`, `setItem`).
    // We loop through options so we don't overwrite existing config
    // values.
    LocalForage.prototype.config = function(options) {
        // If the options argument is an object, we use it to set values.
        // Otherwise, we return either a specified config value or all
        // config values.
        if (typeof(options) === 'object') {
            // If localforage is ready and fully initialized, we can't set
            // any new configuration values. Instead, we return an error.
            if (this._ready) {
                return new Error("Can't call config() after localforage " +
                                 'has been used.');
            }

            for (var i in options) {
                if (i === 'storeName') {
                    options[i] = options[i].replace(/\W/g, '_');
                }

                this._config[i] = options[i];
            }

            // after all config options are set and
            // the driver option is used, try setting it
            if ('driver' in options && options.driver) {
                this.setDriver(this._config.driver);
            }

            return true;
        } else if (typeof(options) === 'string') {
            return this._config[options];
        } else {
            return this._config;
        }
    };

    // Used to define a custom driver, shared across all instances of
    // localForage.
    LocalForage.prototype.defineDriver = function(driverObject, callback,
                                                  errorCallback) {
        var defineDriver = new Promise(function(resolve, reject) {
            try {
                var driverName = driverObject._driver;
                var complianceError = new Error(
                    'Custom driver not compliant; see ' +
                    'https://mozilla.github.io/localForage/#definedriver'
                );
                var namingError = new Error(
                    'Custom driver name already in use: ' + driverObject._driver
                );

                // A driver name should be defined and not overlap with the
                // library-defined, default drivers.
                if (!driverObject._driver) {
                    reject(complianceError);
                    return;
                }
                if (isLibraryDriver(driverObject._driver)) {
                    reject(namingError);
                    return;
                }

                var customDriverMethods = LibraryMethods.concat('_initStorage');
                for (var i = 0; i < customDriverMethods.length; i++) {
                    var customDriverMethod = customDriverMethods[i];
                    if (!customDriverMethod ||
                        !driverObject[customDriverMethod] ||
                        typeof driverObject[customDriverMethod] !== 'function') {
                        reject(complianceError);
                        return;
                    }
                }

                var supportPromise = Promise.resolve(true);
                if ('_support'  in driverObject) {
                    if (driverObject._support && typeof driverObject._support === 'function') {
                        supportPromise = driverObject._support();
                    } else {
                        supportPromise = Promise.resolve(!!driverObject._support);
                    }
                }

                supportPromise.then(function(supportResult) {
                    driverSupport[driverName] = supportResult;
                    CustomDrivers[driverName] = driverObject;
                    resolve();
                }, reject);
            } catch (e) {
                reject(e);
            }
        });

        defineDriver.then(callback, errorCallback);
        return defineDriver;
    };

    LocalForage.prototype.driver = function() {
        return this._driver || null;
    };

    LocalForage.prototype.ready = function(callback) {
        var self = this;

        var ready = new Promise(function(resolve, reject) {
            self._driverSet.then(function() {
                if (self._ready === null) {
                    self._ready = self._initStorage(self._config);
                }

                self._ready.then(resolve, reject);
            })["catch"](reject);
        });

        ready.then(callback, callback);
        return ready;
    };

    LocalForage.prototype.setDriver = function(drivers, callback,
                                               errorCallback) {
        var self = this;

        if (typeof drivers === 'string') {
            drivers = [drivers];
        }

        this._driverSet = new Promise(function(resolve, reject) {
            var driverName = self._getFirstSupportedDriver(drivers);
            var error = new Error('No available storage method found.');

            if (!driverName) {
                self._driverSet = Promise.reject(error);
                reject(error);
                return;
            }

            self._dbInfo = null;
            self._ready = null;

            if (isLibraryDriver(driverName)) {
                // We allow localForage to be declared as a module or as a
                // library available without AMD/require.js.
                if (moduleType === ModuleType.DEFINE) {
                    require([driverName], function(lib) {
                        self._extend(lib);

                        resolve();
                    });

                    return;
                } else if (moduleType === ModuleType.EXPORT) {
                    // Making it browserify friendly
                    var driver;
                    switch (driverName) {
                        case self.INDEXEDDB:
                            driver = require('./drivers/indexeddb');
                            break;
                        case self.LOCALSTORAGE:
                            driver = require('./drivers/localstorage');
                            break;
                        case self.WEBSQL:
                            driver = require('./drivers/websql');
                    }

                    self._extend(driver);
                } else {
                    self._extend(globalObject[driverName]);
                }
            } else if (CustomDrivers[driverName]) {
                self._extend(CustomDrivers[driverName]);
            } else {
                self._driverSet = Promise.reject(error);
                reject(error);
                return;
            }

            resolve();
        });

        function setDriverToConfig() {
            self._config.driver = self.driver();
        }
        this._driverSet.then(setDriverToConfig, setDriverToConfig);

        this._driverSet.then(callback, errorCallback);
        return this._driverSet;
    };

    LocalForage.prototype.supports = function(driverName) {
        return !!driverSupport[driverName];
    };

    LocalForage.prototype._extend = function(libraryMethodsAndProperties) {
        extend(this, libraryMethodsAndProperties);
    };

    // Used to determine which driver we should use as the backend for this
    // instance of localForage.
    LocalForage.prototype._getFirstSupportedDriver = function(drivers) {
        if (drivers && isArray(drivers)) {
            for (var i = 0; i < drivers.length; i++) {
                var driver = drivers[i];

                if (this.supports(driver)) {
                    return driver;
                }
            }
        }

        return null;
    };

    LocalForage.prototype.createInstance = function(options) {
        return new LocalForage(options);
    };

    // The actual localForage object that we expose as a module or via a
    // global. It's extended by pulling in one of our other libraries.
    var localForage = new LocalForage();

    // We allow localForage to be declared as a module or as a library
    // available without AMD/require.js.
    if (moduleType === ModuleType.DEFINE) {
        define('localforage', function() {
            return localForage;
        });
    } else if (moduleType === ModuleType.EXPORT) {
        module.exports = localForage;
    } else {
        this.localforage = localForage;
    }
}).call(window);




/////////////////////////////////
/// NATIVE MODULE STARTS HERE ///
/////////////////////////////////

Elm.Native = Elm.Native || {};
Elm.Native.Storage = {};
Elm.Native.Storage.make = function(localRuntime){

  localRuntime.Native = localRuntime.Native || {};
  localRuntime.Native.Storage = localRuntime.Native.Storage || {};

  if (localRuntime.Native.Storage.values){
    return localRuntime.Native.Storage.values;
  }

  var Task = Elm.Native.Task.make(localRuntime);
  var Utils = Elm.Native.Utils.make(localRuntime);
  var List = Elm.Native.List.make(localRuntime);

  // getItemAsJson : String -> Task error Value
  var getItemAsJson = function(key){
    return Task.asyncFunction(function(callback){
      localforage.getItem(key).then(function(value){
        console.log("get succeeded");
        callback(Task.succeed(value));
      }).catch(function(err){
        console.log("Storage Call: getItemAsJson has failed with key: " + key);
        callback(Task.fail("Storage Call: getItemAsJson has failed with key: " + key));
      });
    });
  };

  // setItem : String -> Value -> Task error ()
  var setItem = function(key, value){
    return Task.asyncFunction(function(callback){
      localforage.setItem(key, value).then(function(){
        console.log("set succeeded");
        callback(Task.succeed(Utils.Tuple0));
      }).catch(function(){
        console.log("Storage Call: setItem has failed with key: " + key + " and value: " + value);
        callback(Task.fail("Storage Call: setItem has failed with key: " + key + " and value: " + value));
      });
    });
  };

  // removeItem : String -> Task error ()
  var removeItem = function(key){
    return Task.asyncFunction(function(callback){
      localforage.removeItem(key).then(function(){
        callback(Task.succeed(Utils.Tuple0));
      }).catch(function(){
        callback(Task.fail("Storage Call: removeItem has failed with key: " + key));
      });
    });
  };

  // clear : Task error ()
  var clear = Task.asyncFunction(function(callback){
    localforage.clear().then(function(){
      callback(Task.succeed(Utils.Tuple0));
    }).catch(function(){
      callback(Task.fail("Storage Call: clear has failed"));
    });
  });


  // keys : Task error (List String)
  var keys = Task.asyncFunction(function(callback){
    localforage.keys().then(function(keys){
      callback(Task.succeed(List.fromArray(keys)));
    }).catch(function(){
      callback(Task.fail("Storage Call: keys has failed"));
    });
  });

  // length : Task error Int
  var length = Task.asyncFunction(function(callback){
    localforage.length().then(function(numberOfKeys){
      callback(Task.succeed(numberOfKeys));
    }).catch(function(){
      callback(Task.fail("Storage Call: length has failed"));
    });
  });


  return {
    getItemAsJson : getItemAsJson,
    setItem       : F2(setItem),
    removeItem    : removeItem,
    clear         : clear,
    keys          : keys,
    length        : length
  };


};

Elm.Storage = Elm.Storage || {};
Elm.Storage.make = function (_elm) {
   "use strict";
   _elm.Storage = _elm.Storage || {};
   if (_elm.Storage.values) return _elm.Storage.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $Json$Encode = Elm.Json.Encode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Storage = Elm.Native.Storage.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Task = Elm.Task.make(_elm);
   var _op = {};
   var length = $Native$Storage.length;
   var keys = $Native$Storage.keys;
   var clear = $Native$Storage.clear;
   var removeItem = $Native$Storage.removeItem;
   var setItem = $Native$Storage.setItem;
   var getItemAsJson = $Native$Storage.getItemAsJson;
   var getItem = F2(function (key,decoder) {
      var decode = function (value) {
         var _p0 = A2($Json$Decode.decodeValue,decoder,value);
         if (_p0.ctor === "Ok") {
               return $Task.succeed(_p0._0);
            } else {
               return $Task.fail("Failed");
            }
      };
      return A2($Task.andThen,getItemAsJson(key),decode);
   });
   return _elm.Storage.values = {_op: _op
                                ,getItemAsJson: getItemAsJson
                                ,getItem: getItem
                                ,setItem: setItem
                                ,removeItem: removeItem
                                ,clear: clear
                                ,keys: keys
                                ,length: length};
};
Elm.Component = Elm.Component || {};
Elm.Component.Search = Elm.Component.Search || {};
Elm.Component.Search.make = function (_elm) {
   "use strict";
   _elm.Component = _elm.Component || {};
   _elm.Component.Search = _elm.Component.Search || {};
   if (_elm.Component.Search.values) return _elm.Component.Search.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Component$PackageDocs = Elm.Component.PackageDocs.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Docs$Entry = Elm.Docs.Entry.make(_elm),
   $Docs$Name = Elm.Docs.Name.make(_elm),
   $Docs$Package = Elm.Docs.Package.make(_elm),
   $Docs$Summary = Elm.Docs.Summary.make(_elm),
   $Docs$Type = Elm.Docs.Type.make(_elm),
   $Docs$Version = Elm.Docs.Version.make(_elm),
   $Effects = Elm.Effects.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Html$Attributes = Elm.Html.Attributes.make(_elm),
   $Html$Events = Elm.Html.Events.make(_elm),
   $Http = Elm.Http.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Page$Context = Elm.Page.Context.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Set = Elm.Set.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Signal$Time = Elm.Signal.Time.make(_elm),
   $Storage = Elm.Storage.make(_elm),
   $String = Elm.String.make(_elm),
   $Task = Elm.Task.make(_elm),
   $Utils$Path = Elm.Utils.Path.make(_elm);
   var _op = {};
   var catMaybes = function (xs) {
      catMaybes: while (true) {
         var _p0 = xs;
         if (_p0.ctor === "[]") {
               return _U.list([]);
            } else {
               if (_p0._0.ctor === "Nothing") {
                     var _v1 = _p0._1;
                     xs = _v1;
                     continue catMaybes;
                  } else {
                     return A2($List._op["::"],_p0._0._0,catMaybes(_p0._1));
                  }
            }
      }
   };
   var chunkPackage = F2(function (packageDict,name) {
      var _p1 = A2($Dict.get,name,packageDict);
      if (_p1.ctor === "Just") {
            return function (_) {
               return _.$package;
            }(_p1._0);
         } else {
            return $Dict.empty;
         }
   });
   var nameDict = F2(function (packageDict,name) {
      var _p2 = A2($Dict.get,name,packageDict);
      if (_p2.ctor === "Just") {
            return function (_) {
               return _.nameDict;
            }(_p2._0);
         } else {
            return $Dict.empty;
         }
   });
   var searchResultsChunks = F2(function (_p3,weightedChunks) {
      var _p4 = _p3;
      return A2($List.map,
      function (_p5) {
         var _p6 = _p5;
         var _p7 = _p6._1.$package;
         return A4($Docs$Entry.typeViewSearch,_p7,_p6._1.name,A2(nameDict,_p4.packageDict,_p7),_p6._1.entry);
      },
      A2($List.sortBy,function (_p8) {    var _p9 = _p8;return _p9._0;},weightedChunks));
   });
   var viewPackesInfo = function (info) {
      return A2($Html.div,
      _U.list([]),
      _U.list([A2($Html.h2,_U.list([]),_U.list([$Html.text("Some statistics")]))
              ,A2($Html.p,
              _U.list([]),
              _U.list([$Html.text("The search index contains ")
                      ,A2($Html.strong,_U.list([]),_U.list([$Html.text($Basics.toString($Dict.size(info.packageDict)))]))
                      ,$Html.text(" packages with a total of ")
                      ,A2($Html.strong,_U.list([]),_U.list([$Html.text($Basics.toString($List.length(info.chunks)))]))
                      ,$Html.text(" type definitions.")]))
              ,$Basics.not($List.isEmpty(info.failed)) ? A2($Html.div,
              _U.list([]),
              _U.list([A2($Html.p,_U.list([]),_U.list([$Html.text("The following packages did not load or parse,")]))
                      ,A2($Html.ul,
                      _U.list([]),
                      A2($List.map,
                      function (summary) {
                         return A2($Html.li,
                         _U.list([]),
                         _U.list([A2($Html.a,
                         _U.list([$Html$Attributes.href(A2($Basics._op["++"],"http://package.elm-lang.org/packages/",summary.name))
                                 ,$Html$Attributes.style(_U.list([{ctor: "_Tuple2",_0: "color",_1: "#bbb"}]))]),
                         _U.list([$Html.text(summary.name)]))]));
                      },
                      info.failed))])) : $Html.text("")]));
   };
   var latestVersionContext = function (summary) {
      var latestVersionSingleton = A2($List.map,$Docs$Version.vsnToString,A2($List.take,1,summary.versions));
      var userProjectList = A2($List.take,2,A2($String.split,"/",summary.name));
      var _p10 = A2($List.append,userProjectList,latestVersionSingleton);
      if (_p10.ctor === "::" && _p10._1.ctor === "::" && _p10._1._1.ctor === "::" && _p10._1._1._1.ctor === "[]") {
            return $Result.Ok(A5($Page$Context.VersionContext,_p10._0,_p10._1._0,_p10._1._1._0,_U.list([]),$Maybe.Nothing));
         } else {
            return $Result.Err("Summary is corrupted");
         }
   };
   var Query = function (a) {    return {ctor: "Query",_0: a};};
   var exampleSearches = function (addr) {
      var exampleSearchItem = function (query) {
         return A2($Html.li,
         _U.list([]),
         _U.list([A2($Html.a,
         _U.list([$Html$Attributes.style(_U.list([{ctor: "_Tuple2",_0: "cursor",_1: "pointer"}])),A2($Html$Events.onClick,addr,Query(query))]),
         _U.list([$Html.text(query)]))]));
      };
      var exampleQueries = _U.list(["map","(a -> b -> b) -> b -> List a -> b","Result x a -> (a -> Result x b) -> Result x b","(x -> y -> z) -> y -> x -> z"]);
      return A2($Html.ul,_U.list([]),A2($List.map,exampleSearchItem,exampleQueries));
   };
   var viewSearchIntro = F2(function (addr,info) {
      return A2($Html.div,
      _U.list([]),
      _U.list([A2($Html.h1,_U.list([]),_U.list([$Html.text("Welcome to the Elm API Search")]))
              ,A2($Html.p,
              _U.list([]),
              _U.list([$Html.text("Search the modules of the latest Elm packages by either function name or by approximate type signature.")]))
              ,A2($Html.h2,_U.list([]),_U.list([$Html.text("Example searches")]))
              ,exampleSearches(addr)
              ,viewPackesInfo(info)]));
   });
   var viewSearchResults = F2(function (addr,_p11) {
      var _p12 = _p11;
      var _p19 = _p12.query;
      var _p18 = _p12.chunks;
      var queryType = $Docs$Type.normalize($Component$PackageDocs.stringToType(_p19));
      var filteredChunks = function () {
         var _p13 = queryType;
         if (_p13.ctor === "Var") {
               return A2($List.filter,
               function (_p14) {
                  var _p15 = _p14;
                  return _U.cmp(_p15._0,10) < 0;
               },
               A2($List.map,function (chunk) {    return {ctor: "_Tuple2",_0: A2($Docs$Entry.nameDistance,_p19,chunk.entry),_1: chunk};},_p18));
            } else {
               return A2($List.filter,
               function (_p16) {
                  var _p17 = _p16;
                  return _U.cmp(_p17._0,10) < 0;
               },
               A2($List.map,function (chunk) {    return {ctor: "_Tuple2",_0: A2($Docs$Entry.typeDistance,queryType,chunk.entryNormalized),_1: chunk};},_p18));
            }
      }();
      return _U.eq($List.length(filteredChunks),0) ? A2($Html.div,
      _U.list([]),
      _U.list([A2($Html.p,_U.list([]),_U.list([$Html.text("Your search did not yield any results. You can try one of the examples below.")]))
              ,exampleSearches(addr)])) : A2($Html.div,_U.list([]),A2(searchResultsChunks,_p12,filteredChunks));
   });
   var MakeDocs = F2(function (a,b) {    return {ctor: "MakeDocs",_0: a,_1: b};});
   var RequestDocs = function (a) {    return {ctor: "RequestDocs",_0: a};};
   var FailDocs = function (a) {    return {ctor: "FailDocs",_0: a};};
   var getDocsFromStorage = function (summary) {
      var failTask = $Task.succeed(FailDocs(summary));
      var contextResult = latestVersionContext(summary);
      var _p20 = contextResult;
      if (_p20.ctor === "Ok") {
            return $Effects.task(A3($Basics.flip,
            $Task.onError,
            $Basics.always($Task.succeed(RequestDocs(summary))),
            A2($Task.map,
            MakeDocs(_p20._0),
            A2($Storage.getItem,
            A2($Utils$Path._op["</>"],A2($Utils$Path._op["</>"],_p20._0.user,_p20._0.project),_p20._0.version),
            $Docs$Package.decodePackage))));
         } else {
            return $Effects.task(failTask);
         }
   };
   var getDocs = function (summary) {
      var failTask = $Task.succeed(FailDocs(summary));
      var contextResult = latestVersionContext(summary);
      var _p21 = contextResult;
      if (_p21.ctor === "Ok") {
            var _p22 = _p21._0;
            return $Effects.task(A3($Basics.flip,
            $Task.onError,
            $Basics.always(failTask),
            A2($Task.map,
            MakeDocs(_p22),
            A3($Basics.flip,
            $Task.andThen,
            function (docs) {
               return A3($Basics.flip,
               $Task.andThen,
               $Basics.always($Task.succeed(docs)),
               A2($Storage.setItem,
               A2($Utils$Path._op["</>"],A2($Utils$Path._op["</>"],_p21._0.user,_p21._0.project),_p21._0.version),
               $Docs$Package.encodePackage(docs)));
            },
            A2($Task.mapError,$Basics.always("Could not get docs"),$Page$Context.getDocs(_p22))))));
         } else {
            return $Effects.task(failTask);
         }
   };
   var Load = function (a) {    return {ctor: "Load",_0: a};};
   var Fail = function (a) {    return {ctor: "Fail",_0: a};};
   var getPackageInfo = function () {
      var getNew = A2($Http.get,$Json$Decode.list($Json$Decode.string),"https://crossorigin.me/http://package.elm-lang.org/new-packages");
      var getAll = A2($Http.get,$Docs$Summary.decoder,"https://crossorigin.me/http://package.elm-lang.org/all-packages");
      return $Effects.task(A3($Basics.flip,
      $Task.onError,
      function (_p23) {
         return $Task.succeed(Fail(_p23));
      },
      A2($Task.map,Load,A3($Task.map2,F2(function (v0,v1) {    return {ctor: "_Tuple2",_0: v0,_1: v1};}),getAll,getNew))));
   }();
   var queryMailbox = $Signal.mailbox(Query(""));
   var querySignal = A2($Signal$Time.settledAfter,300,queryMailbox.signal);
   var viewSearchInput = F2(function (addr,info) {
      return A2($Html.input,
      _U.list([$Html$Attributes.placeholder("Search function by name or type")
              ,$Html$Attributes.value(info.query)
              ,A3($Html$Events.on,"input",$Html$Events.targetValue,function (_p24) {    return A2($Signal.message,queryMailbox.address,Query(_p24));})]),
      _U.list([]));
   });
   var view = F2(function (addr,model) {
      return A2($Html.div,
      _U.list([$Html$Attributes.$class("search")]),
      function () {
         var _p25 = model;
         switch (_p25.ctor)
         {case "Loading": return _U.list([A2($Html.p,_U.list([]),_U.list([$Html.text("Loading list of packages...")]))]);
            case "Failed": return _U.list([A2($Html.p,_U.list([]),_U.list([$Html.text("Package summary did not load.")]))
                                          ,A2($Html.p,_U.list([]),_U.list([$Html.text($Basics.toString(_p25._0))]))]);
            case "Catalog": return _U.list([A2($Html.p,
              _U.list([]),
              _U.list([$Html.text(A2($Basics._op["++"],"Loading docs for ",A2($Basics._op["++"],$Basics.toString($List.length(_p25._0)),"packages...")))]))]);
            default: var _p26 = _p25._0;
              return _U.list([A2(viewSearchInput,addr,_p26),$String.isEmpty(_p26.query) ? A2(viewSearchIntro,addr,_p26) : A2(viewSearchResults,addr,_p26)]);}
      }());
   });
   var Chunk = F4(function (a,b,c,d) {    return {$package: a,name: b,entry: c,entryNormalized: d};});
   var toMaybeChunk = F3(function (pkgIdent,moduleDocs,name) {
      var _p27 = A2($Dict.get,name,moduleDocs.entries);
      if (_p27.ctor === "Nothing") {
            return $Maybe.Nothing;
         } else {
            var entry = A2($Docs$Entry.map,$Component$PackageDocs.stringToType,_p27._0);
            var entryNormalized = A2($Docs$Entry.map,$Docs$Type.normalize,entry);
            return $Maybe.Just(A4(Chunk,pkgIdent,A2($Docs$Name.Canonical,moduleDocs.name,name),entry,entryNormalized));
         }
   });
   var subChunksHelp = F3(function (pkgIdent,moduleDocs,parts) {
      var _p28 = parts;
      if (_p28.ctor === "[]") {
            return _U.list([]);
         } else {
            var _p32 = _p28._0;
            var part = $String.trim(_p32);
            var _p29 = $Component$PackageDocs.isValue(part);
            if (_p29.ctor === "Just") {
                  return A2($List._op["::"],A3(toMaybeChunk,pkgIdent,moduleDocs,_p29._0),A3(subChunksHelp,pkgIdent,moduleDocs,_p28._1));
               } else {
                  var trimmedPart = $String.trimLeft(_p32);
                  var _p30 = $String.words(trimmedPart);
                  if (_p30.ctor === "[]") {
                        return _U.list([]);
                     } else {
                        var _p31 = $Component$PackageDocs.isValue(_p30._0);
                        if (_p31.ctor === "Just") {
                              return _U.list([A3(toMaybeChunk,pkgIdent,moduleDocs,_p31._0)]);
                           } else {
                              return _U.list([]);
                           }
                     }
               }
         }
   });
   var subChunks = F3(function (pkgIdent,moduleDocs,postDocs) {    return catMaybes(A3(subChunksHelp,pkgIdent,moduleDocs,A2($String.split,",",postDocs)));});
   var toChunks = F2(function (pkgIdent,moduleDocs) {
      var _p33 = A2($String.split,"\n@docs ",moduleDocs.comment);
      if (_p33.ctor === "[]") {
            return _U.list([]);
         } else {
            return A2($List.concatMap,A2(subChunks,pkgIdent,moduleDocs),_p33._1);
         }
   });
   var PackageInfo = F3(function (a,b,c) {    return {$package: a,context: b,nameDict: c};});
   var Info = F4(function (a,b,c,d) {    return {packageDict: a,chunks: b,failed: c,query: d};});
   var Docs = function (a) {    return {ctor: "Docs",_0: a};};
   var Catalog = function (a) {    return {ctor: "Catalog",_0: a};};
   var Failed = function (a) {    return {ctor: "Failed",_0: a};};
   var update = F2(function (action,model) {
      var _p34 = action;
      switch (_p34.ctor)
      {case "Query": return A3($Basics.flip,
           F2(function (v0,v1) {    return {ctor: "_Tuple2",_0: v0,_1: v1};}),
           $Effects.none,
           function () {
              var _p35 = model;
              if (_p35.ctor === "Docs") {
                    return Docs(_U.update(_p35._0,{query: _p34._0}));
                 } else {
                    return model;
                 }
           }());
         case "Fail": return {ctor: "_Tuple2",_0: Failed(_p34._0),_1: $Effects.none};
         case "Load": var updatedSet = $Set.fromList(_p34._0._1);
           var _p36 = A2($List.partition,function (_p37) {    var _p38 = _p37;return A2($Set.member,_p38.name,updatedSet);},_p34._0._0);
           var summaries = _p36._0;
           var oldSummaries = _p36._1;
           var contextEffects = A2($List.map,getDocsFromStorage,summaries);
           return {ctor: "_Tuple2",_0: Catalog(summaries),_1: $Effects.batch(contextEffects)};
         case "FailDocs": var _p41 = _p34._0;
           var _p39 = model;
           if (_p39.ctor === "Docs") {
                 var _p40 = _p39._0;
                 return {ctor: "_Tuple2",_0: Docs(_U.update(_p40,{failed: A2($List._op["::"],_p41,_p40.failed)})),_1: $Effects.none};
              } else {
                 return {ctor: "_Tuple2",_0: Docs(A4(Info,$Dict.empty,_U.list([]),_U.list([_p41]),"")),_1: $Effects.none};
              }
         case "RequestDocs": return {ctor: "_Tuple2",_0: model,_1: getDocs(_p34._0)};
         default: var _p48 = _p34._1;
           var _p47 = _p34._0;
           var pkgInfo = A3(PackageInfo,_p48,_p47,$Component$PackageDocs.toNameDict(_p48));
           var _p42 = _p47;
           var user = _p42.user;
           var project = _p42.project;
           var version = _p42.version;
           var pkgName = A2($Utils$Path._op["</>"],A2($Utils$Path._op["</>"],user,project),version);
           var chunks = A2($List.concatMap,function (_p43) {    var _p44 = _p43;return A2(toChunks,pkgName,_p44._1);},$Dict.toList(_p48));
           var _p45 = model;
           if (_p45.ctor === "Docs") {
                 var _p46 = _p45._0;
                 return {ctor: "_Tuple2"
                        ,_0: Docs(_U.update(_p46,{packageDict: A3($Dict.insert,pkgName,pkgInfo,_p46.packageDict),chunks: A2($List.append,_p46.chunks,chunks)}))
                        ,_1: $Effects.none};
              } else {
                 return {ctor: "_Tuple2",_0: Docs(A4(Info,A2($Dict.singleton,pkgName,pkgInfo),chunks,_U.list([]),"")),_1: $Effects.none};
              }}
   });
   var Loading = {ctor: "Loading"};
   var init = {ctor: "_Tuple2",_0: Loading,_1: getPackageInfo};
   return _elm.Component.Search.values = {_op: _op
                                         ,Loading: Loading
                                         ,Failed: Failed
                                         ,Catalog: Catalog
                                         ,Docs: Docs
                                         ,Info: Info
                                         ,PackageInfo: PackageInfo
                                         ,Chunk: Chunk
                                         ,init: init
                                         ,queryMailbox: queryMailbox
                                         ,querySignal: querySignal
                                         ,Fail: Fail
                                         ,Load: Load
                                         ,FailDocs: FailDocs
                                         ,RequestDocs: RequestDocs
                                         ,MakeDocs: MakeDocs
                                         ,Query: Query
                                         ,update: update
                                         ,latestVersionContext: latestVersionContext
                                         ,getPackageInfo: getPackageInfo
                                         ,getDocsFromStorage: getDocsFromStorage
                                         ,getDocs: getDocs
                                         ,view: view
                                         ,viewSearchInput: viewSearchInput
                                         ,viewSearchIntro: viewSearchIntro
                                         ,exampleSearches: exampleSearches
                                         ,viewPackesInfo: viewPackesInfo
                                         ,viewSearchResults: viewSearchResults
                                         ,searchResultsChunks: searchResultsChunks
                                         ,toChunks: toChunks
                                         ,subChunks: subChunks
                                         ,subChunksHelp: subChunksHelp
                                         ,toMaybeChunk: toMaybeChunk
                                         ,nameDict: nameDict
                                         ,chunkPackage: chunkPackage
                                         ,catMaybes: catMaybes};
};
Elm.StartApp = Elm.StartApp || {};
Elm.StartApp.make = function (_elm) {
   "use strict";
   _elm.StartApp = _elm.StartApp || {};
   if (_elm.StartApp.values) return _elm.StartApp.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Effects = Elm.Effects.make(_elm),
   $Html = Elm.Html.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Task = Elm.Task.make(_elm);
   var _op = {};
   var start = function (config) {
      var updateStep = F2(function (action,_p0) {
         var _p1 = _p0;
         var _p2 = A2(config.update,action,_p1._0);
         var newModel = _p2._0;
         var additionalEffects = _p2._1;
         return {ctor: "_Tuple2",_0: newModel,_1: $Effects.batch(_U.list([_p1._1,additionalEffects]))};
      });
      var update = F2(function (actions,_p3) {    var _p4 = _p3;return A3($List.foldl,updateStep,{ctor: "_Tuple2",_0: _p4._0,_1: $Effects.none},actions);});
      var messages = $Signal.mailbox(_U.list([]));
      var singleton = function (action) {    return _U.list([action]);};
      var address = A2($Signal.forwardTo,messages.address,singleton);
      var inputs = $Signal.mergeMany(A2($List._op["::"],messages.signal,A2($List.map,$Signal.map(singleton),config.inputs)));
      var effectsAndModel = A3($Signal.foldp,update,config.init,inputs);
      var model = A2($Signal.map,$Basics.fst,effectsAndModel);
      return {html: A2($Signal.map,config.view(address),model)
             ,model: model
             ,tasks: A2($Signal.map,function (_p5) {    return A2($Effects.toTask,messages.address,$Basics.snd(_p5));},effectsAndModel)};
   };
   var App = F3(function (a,b,c) {    return {html: a,model: b,tasks: c};});
   var Config = F4(function (a,b,c,d) {    return {init: a,update: b,view: c,inputs: d};});
   return _elm.StartApp.values = {_op: _op,start: start,Config: Config,App: App};
};
Elm.Page = Elm.Page || {};
Elm.Page.Search = Elm.Page.Search || {};
Elm.Page.Search.make = function (_elm) {
   "use strict";
   _elm.Page = _elm.Page || {};
   _elm.Page.Search = _elm.Page.Search || {};
   if (_elm.Page.Search.values) return _elm.Page.Search.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Component$CatalogSidebar = Elm.Component.CatalogSidebar.make(_elm),
   $Component$Header = Elm.Component.Header.make(_elm),
   $Component$Search = Elm.Component.Search.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Effects = Elm.Effects.make(_elm),
   $Html = Elm.Html.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Route = Elm.Route.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $StartApp = Elm.StartApp.make(_elm),
   $Task = Elm.Task.make(_elm);
   var _op = {};
   var UpdateSidebar = function (a) {    return {ctor: "UpdateSidebar",_0: a};};
   var UpdateSearch = function (a) {    return {ctor: "UpdateSearch",_0: a};};
   var update = F2(function (action,model) {
      var _p0 = action;
      if (_p0.ctor === "UpdateSearch") {
            var _p1 = A2($Component$Search.update,_p0._0,model.search);
            var newSearch = _p1._0;
            var fx = _p1._1;
            return {ctor: "_Tuple2",_0: _U.update(model,{search: newSearch}),_1: A2($Effects.map,UpdateSearch,fx)};
         } else {
            var _p2 = A2($Component$CatalogSidebar.update,_p0._0,model.sidebar);
            var newSidebar = _p2._0;
            var fx = _p2._1;
            return {ctor: "_Tuple2",_0: _U.update(model,{sidebar: newSidebar}),_1: A2($Effects.map,UpdateSidebar,fx)};
         }
   });
   var view = F2(function (addr,model) {
      return A3($Component$Header.view,
      addr,
      model.header,
      _U.list([A2($Component$Search.view,A2($Signal.forwardTo,addr,UpdateSearch),model.search)
              ,A2($Component$CatalogSidebar.view,A2($Signal.forwardTo,addr,UpdateSidebar),model.sidebar)]));
   });
   var Model = F3(function (a,b,c) {    return {header: a,search: b,sidebar: c};});
   var init = function () {
      var _p3 = $Component$CatalogSidebar.init;
      var sidebar = _p3._0;
      var sidebarFx = _p3._1;
      var _p4 = $Component$Search.init;
      var search = _p4._0;
      var searchFx = _p4._1;
      var _p5 = $Component$Header.init($Route.Packages($Maybe.Nothing));
      var header = _p5._0;
      var headerFx = _p5._1;
      return {ctor: "_Tuple2"
             ,_0: A3(Model,header,search,sidebar)
             ,_1: $Effects.batch(_U.list([headerFx,A2($Effects.map,UpdateSearch,searchFx),A2($Effects.map,UpdateSidebar,sidebarFx)]))};
   }();
   var app = $StartApp.start({init: init,view: view,update: update,inputs: _U.list([A2($Signal.map,UpdateSearch,$Component$Search.querySignal)])});
   var main = app.html;
   var worker = Elm.Native.Task.make(_elm).performSignal("worker",app.tasks);
   return _elm.Page.Search.values = {_op: _op
                                    ,app: app
                                    ,main: main
                                    ,Model: Model
                                    ,init: init
                                    ,UpdateSearch: UpdateSearch
                                    ,UpdateSidebar: UpdateSidebar
                                    ,update: update
                                    ,view: view};
};

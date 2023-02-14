Mediador
========

[ ![Codeship Status for xaviervia/mediador](https://codeship.io/projects/1b988be0-ebb2-0131-3c3b-4ebb653f9bc0/status)](https://codeship.io/projects/26547)

Event venue built as a mixin.

Installation
------------

```
npm install mediador --save
```

### Browser and RequireJS

Mediador is also available for the browser and 
[RequireJS](http://requirejs.org/). You can install it with `bower`.

```
bower install mediador --save
```

Usage
-----

### Instance

`emit` maps each element in the second argument array as an argument to
send to the listener function.

```javascript
var Mediador = require("mediador")

var mediador = new Mediador

mediador.on("event", function (text) {
  console.log("event emitted with arg: " + text)
})

mediador.emit("event", ["hello"])
```

### Mixin: Inheriting on/emit

```javascript
var Mediador = require("mediador")

var YourClass = function () {}
YourClass.prototype.on      = Mediador.prototype.on
YourClass.prototype.emit = Mediador.prototype.emit

var yourInstance = new YourClass()

yourInstance.on("event", function (you) {
  console.log(you + " already firing events!")
})

yourInstance.emit("event", ["Me"])
```

The simplest way to use Mediador is to make instances, but it is not the
recommended way.

Mediador is a [**mixin**](https://en.wikipedia.org/wiki/Mixin). The idea is
that you can add its methods to the prototypes or objects that you want to
amplify with events, without having to make them inherit directly from
Mediador.

The events are stored in the `listeners` property within the emitter object.
Bear this in mind so to not accidentally override the property.

You can find these examples in the [`examples`](examples) folder in this 
repo. You can run them without installing `mediador` by running `npm link`
on the repo folder (may require `sudo`).

### Off with the listener

```javascript
var Mediador = require("mediador")

var YourClass = function () {}
YourClass.prototype.on      = Mediador.prototype.on
YourClass.prototype.off     = Mediador.prototype.off
YourClass.prototype.emit = Mediador.prototype.emit

var yourInstance = new YourClass()
var listener     = function (you) {
  console.log(you + " already firing events!")
}

yourInstance.on("event", listener)

yourInstance.emit("event", ["Me"])

yourInstance.off("event", listener)

// Will do nothing
yourInstance.emit("event", ["Nothing"])
```

### You can add the methods directly in an object

```javascript
var Mediador     = require("mediador")
var emitter      = {}

emitter.on       = Mediator.prototype.on
emitter.emit  = Mediator.prototype.emit

emitter.on("event", function (text) {
  console.log(text)
})

emitter.emit("event", ["Hello World"])
```

### The emitter is sent as argument

The event emitter is always sent to the listener functions
as the last argument.

This is crucial, because otherwise in the contexts of many listeners there
would be no available references to the emitter–the emitter would be 
unreachable. The emitter is needed for listeners to be able to emit further
events in it–in fact, that might be the only way for a listener to pass
information forward.

> Another strategy to make the emitter available for the listeners would be
> to bind the listener to `this`, `this` being the emitter object. I 
> really don't like when libraries do that.

```javascript
var Mediador = require("mediador")

var YourClass = function () {}
YourClass.prototype.on      = Mediador.prototype.on
YourClass.prototype.off     = Mediador.prototype.off
YourClass.prototype.emit = Mediador.prototype.emit

var yourInstance = new YourClass()

yourInstance.on("event", function (irrelevant, emitter) {
  emitter.emit("completed")
})

yourInstance.on("completed", function () {
  console.log("The 'event' was successfully emited and 'completed' too")
})

yourInstance.emit("event", ["something irrelevant"])
```

### It also works in bulk

```javascript
var Mediador = require("mediador")

var YourClass = function () {}
YourClass.prototype.on      = Mediador.prototype.on
YourClass.prototype.off     = Mediador.prototype.off
YourClass.prototype.emit = Mediador.prototype.emit

var yourInstance = new YourClass()

var listenerSet = {
  event: function () {
    console.log("Called the event")
  }
}

yourInstance.on(listenerSet)

yourInstance.emit("event")

yourInstance.off(listenerSet)
```

#### What is the `listenerSet`?

**Mediador** introduces the concept of an `listenerSet`, that is just an
object where every property method will be added as a listener in the
emitter. For example, if the set has a `read` method, its function will
be added as a listener for the `read` event in the emitter.

This is very useful both for adding events in bulk and for removing them.
**Mediador** provides support for both operations in the `on` and `off`
methods.

### Why does Mediador inserts the `listeners` property?

This is a question about namespace pollution.

When you add a listener using the `on` method, Mediador adds the listener
function to the `listeners` property of the object, which will be _created_
if not present and used as it is if present. Mediador uses the `listeners`
property without checking for the property's type. The property may well
have been written by another function not related to Mediador. Is this OK?

The thing is, there are several strategies to avoid name collision within an
object, and most of them involve encapsulating the library specific data
in a private data object or using some kind of namespace (such a as
naming the property `mediator_listeners` or `_listeners`).
I don't favor this approach because:

1. Are you really planning on using several event libraries on the same
   object?
2. Will you gladly use a library that extends your object blindfolded and
   risk name collision anyways?
3. Why closing the door to interacting with the properties set by the 
   library? You may very well wish to modify or query the `listeners` set.

In other words, I consider that using lightly a library _that extends your
objects_ is a poor design choice. **Mediador** and other libraries of
its kind should be considered part of your design and taken for what they
are: tested, encapsulated and standardized methods to achieve certain
behaviors that are useful only because they save you _time_.

### Why is there no `once` method?

Remember:

```javascript
// You can name an anonymous function to call it from within
// You can totally do this:
mediador.on("fire", function notAnonymousAnymore() {
  mediador.off("fire", notAnonymousAnymore)
  console.log("The 'fire' event was called")
})
```

> Corollary: EventEmitter's `once` method is not required. Keep your APIs
> simple

Mediador.prototype.on
---------------------

### on( event, callback )

Stores the `callback` function as a listener for the specified `event`.
If the callback was already present, does nothing.

Chainable.

#### Arguments

- `String` event
- `Function` callback

#### Returns

- `Mediador` this

### on( eventHash )

Binds all property methods of the `eventHash` as listeners in their
respective events. For example, if `on` is called with the hash:

```javascript
{
  hear: function (something) { console.log(something); },
  see: function (something) { console.log(something); },
}
```

the effect will be the same as if `on` had been called with `('hear',
function (...) {...})` and `('see', function (...) {...})`.

Chainable.

#### Arguments

- `Object` eventHash

#### Returns

- `Mediador` this

Mediador.prototype.emit
-----------------------

### emit( event, args )

Fires all the listener callbacks associated with the `event`. Chainable.
The arguments for the listeners are each element within the `args` array,
followed by the emitter itself.

#### Arguments

- `String` event
- `Array` args

#### Returns

- `Mediador` this

Mediador.prototype.off
----------------------

### off( event, callback )

Removes the `callback` function from the listener list to the `event`.
Does nothing if the callback was not in the list.

Chainable.

#### Arguments

- `String` event
- `Function` callback

#### Returns

- `Mediador` this

### off( eventHash )

Releases all property methods of the `eventHash` from their
respective events. For example, if `off` is called with the hash:

```javascript
{
  hear: function (something) { console.log(something); },
  see: function (something) { console.log(something); },
}
```

the effect will be the same as if `off` had been called with `('hear',
function (...) {...})` and `('see', function (...) {...})`.

Chainable.

#### Arguments

- `Object` eventHash

#### Returns

- `Mediador` this

Testing
-------

Tests require CoffeeScript. Install with `sudo npm install -g coffee-script`.

Then clone the repo and run:

```
> npm install
> npm test
```

License
-------

Copyright 2014 Xavier Via

ISC license.

See [LICENSE](LICENSE) attached.

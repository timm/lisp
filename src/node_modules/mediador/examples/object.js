var Mediador     = require("mediador")
var emitter      = {}

emitter.on       = Mediador.prototype.on
emitter.emit  = Mediador.prototype.emit

emitter.on("event", function (text) {
  console.log(text)
})

emitter.emit("event", ["Hello World"])

var Mediador = require("mediador")

var YourClass = function () {}
YourClass.prototype.on      = Mediador.prototype.on
YourClass.prototype.off     = Mediador.prototype.off
YourClass.prototype.emit = Mediador.prototype.emit

var yourInstance = new YourClass()

yourInstance.on("completed", function () {
  console.log("The 'event' was successfully emited and 'completed' too")
})

yourInstance.on("event", function (irrelevant, emitter) {
  emitter.emit("completed")
})

yourInstance.emit("event", ["something irrelevant"])

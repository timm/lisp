var Mediador = require("mediador")

var YourClass = function () {}
YourClass.prototype.on      = Mediador.prototype.on
YourClass.prototype.off     = Mediador.prototype.off
YourClass.prototype.emit = Mediador.prototype.emit

var yourInstance = new YourClass()

var eventHash = {
  event: function () {
    console.log("Called the event")
  }
}

yourInstance.on(eventHash)

yourInstance.emit("event")

yourInstance.off(eventHash)

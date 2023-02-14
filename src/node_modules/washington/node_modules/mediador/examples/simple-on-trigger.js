var Mediador = require("mediador")

var YourClass = function () {}
YourClass.prototype.on      = Mediador.prototype.on
YourClass.prototype.emit = Mediador.prototype.emit

var yourInstance = new YourClass()

yourInstance.on("event", function (you) {
  console.log(you + " already firing events!")
})

yourInstance.emit("event", ["Me"])

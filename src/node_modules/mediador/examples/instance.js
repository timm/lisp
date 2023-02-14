var Mediador = require("mediador")

var mediador = new Mediador

mediador.on("event", function (text) {
  console.log("event emitted with arg: " + text)
})

mediador.emit("event", ["hello"])
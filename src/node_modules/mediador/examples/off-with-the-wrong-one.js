var Mediador = require("../mediador")

var venue = new Mediador

var set   = {
  event: function () {}
}

venue.on(set)

venue.off("event", function () {})

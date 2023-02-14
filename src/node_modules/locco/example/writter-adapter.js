var locco = require("locco")
var Mediador = require("mediador")

var loggerAdapter = {
  comment: function (data) {
    console.log("From file: " + data.file.path)
    console.log("...a comment line: " + data.comment)
    this.emit("post", [
      "destination.file.html",
      "<p>" + data.comment + "</p>"
    ])
  },

  code: function (data) {
    console.log("From file: " + data.file.path)
    console.log("...a code line: " + data.code)
    this.emit("post", [
      "destination.file.html",
      "<p><code>" + data.code + "</code></p>"
    ])
  }
}

loggerAdapter.on     = Mediador.prototype.on
loggerAdapter.off    = Mediador.prototype.off
loggerAdapter.emit   = Mediador.prototype.emit

locco({
  source: "**/*.rb",
  commentStart: "#",
  escapeSequence: "!",
  adapter: loggerAdapter
})

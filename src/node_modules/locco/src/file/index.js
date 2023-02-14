// File
// ====
//
// File adapter for Mediador. Concentrates a Reader and a Writer which
// expose a basic API for reading the content of files using a glob pattern and
// writing to them with events.
var Mediador  = require("mediador")

var example = require("washington")
var assert  = require("assert")

var File = function () {
  this.on(new File.Writer)
  this.on(new File.Reader().on(this))
}

File.Reader = require("./reader")
File.Writer = require("./writer")

File.prototype = Object.create(Mediador.prototype)

//
// Methods
// -------
//
// ### file
//
// Emits a `file` event with the provided argument.
//
File.prototype.file = function (file) {
  this.emit("file", [file]) }

// ### line
//
// Emits a `line` event with the provided argument.
File.prototype.line = function (line) {
  this.emit("line", [line]) }

example("proxies the call to file", function () {
  var file = new File
  var listener = {
    file: function (file) {
      (listener.file.calls = listener.file.calls || [])
        .push(file) } }

  file.on(listener)

  file.file("content")

  assert.equal(listener.file.calls[0], "content")
})

example("proxies the call to line", function () {
  var file = new File
  var listener = {
    line: function (file) {
      (listener.line.calls = listener.line.calls || [])
        .push(file) } }

  file.on(listener)

  file.line("content")

  assert.equal(listener.line.calls[0], "content")
})

module.exports = File

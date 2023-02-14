var example   = require("washington")
var assert    = require("assert")
var Mediador  = require("mediador")
var fs        = require("fs")

var Writer = function () {}

Writer.prototype = Object.create(Mediador.prototype)

Writer.prototype.post = function (path, piece) {
  this.stream = this.stream || {}
  this.stream[path] = this.stream[path] ||
    fs.createWriteStream(path, { flags: "w"})

  this.stream[path].write(piece)

  return this
}

example("should append into the corresponding file, chainable", function () {
  var hijack = {}
  hijack.createWriteStream = fs.createWriteStream
  var listener = {
    write: function (piece) {
      (listener.write.calls = listener.write.calls || [])
        .push(piece) } }

  fs.createWriteStream = function (path, flags) {
    (fs.createWriteStream.calls = fs.createWriteStream.calls || [])
      .push([path, flags])
    return listener }

  var fileWriter = new Writer().on(listener)

  var chainable = fileWriter.post("tmp/file.txt", "some piece")

  assert.equal(chainable, fileWriter)
  assert.equal(fs.createWriteStream.calls[0][0], "tmp/file.txt")
  assert.equal(fs.createWriteStream.calls[0][1].flags, "w")
  assert.equal(listener.write.calls[0], "some piece")

  fs.createWriteStream = hijack.createWriteStream
})

module.exports = Writer

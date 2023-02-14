var example   = require("washington")
var assert    = require("assert")
var fs        = require("fs")
var Mediador  = require("mediador")
var glob      = require("glob")

var Reader = function (options) {
  this.options = options || {}
}
Reader.prototype = Object.create(Mediador.prototype)

Reader.prototype.get = function (path) {
  glob.sync(path).forEach( function (name) {
    this.streams = this.streams || {}
    this.streams[name] = {
      metadata: { path: name },
      buffer: "",
      stream: fs.createReadStream(name) }

    this.streams[name].stream.on("data", function (chunk) {
      if (this.options.fullFile)
        this.streams[name].buffer += chunk.toString()

      else {
        var lines = (this.streams[name].buffer + chunk.toString()).split("\n")
        lines.forEach(function (line, index) {

          //! Leave the last in the buffer
          if (index === lines.length - 1)
            this.streams[name].buffer = line

          //! Emit all the others
          else
            this.emit("line", [{ line: line, file: this.streams[name].metadata }])

        }.bind(this))
      }

    }.bind(this))

    this.streams[name].stream.on("end", function () {
      if (this.options.fullFile)
        this.emit("file", [{ text: this.streams[name].buffer, file: this.streams[name].metadata }])
      else
        this.emit("line", [{ line: this.streams[name].buffer, file: this.streams[name].metadata }])

      this.streams[name].complete = true
      if (Object.keys(this.streams).filter(function (key) {
            return !this.streams[key].complete }.bind(this)).length === 0)
        this.emit("complete")

    }.bind(this))
  }.bind(this) )
}

example("should emit line by line with file metadata", function (done) {
  //! given
  try { fs.mkdirSync("tmp")           } catch (e) {}
  try { fs.mkdirSync("tmp/test")      } catch (e) {}
  fs.writeFileSync("tmp/test/file.txt", "some data\nlala")
  fs.writeFileSync("tmp/test/another.txt", "some other data")

  var listener = {
    line: function (line) {
      this.line.calls = this.line.calls || {}
      this.line.calls[line.file.path] = this.line.calls[line.file.path] || []
      this.line.calls[line.file.path].push(line) },

    complete: function () {
      try {
        assert.equal(listener.line.calls["tmp/test/file.txt"][0].file.path, "tmp/test/file.txt")
        assert.equal(listener.line.calls["tmp/test/file.txt"][0].line, "some data")
        assert.equal(listener.line.calls["tmp/test/file.txt"][1].file.path, "tmp/test/file.txt")
        assert.equal(listener.line.calls["tmp/test/file.txt"][1].line, "lala")

        assert.equal( //! for optimization
          listener.line.calls["tmp/test/file.txt"][0].file,
          listener.line.calls["tmp/test/file.txt"][1].file)

        assert.equal(listener.line.calls["tmp/test/another.txt"][0].file.path, "tmp/test/another.txt")
        assert.equal(listener.line.calls["tmp/test/another.txt"][0].line, "some other data")

        done()
      } catch (e) {
        fs.unlinkSync("tmp/test/file.txt")
        fs.unlinkSync("tmp/test/another.txt")
        fs.rmdirSync("tmp/test")
        fs.rmdirSync("tmp")
        done(e)
      }
    }
  }

  //! when
  new Reader()
    .on(listener)
    .get("tmp/**/*.txt")

})

example("when configured it should emit each file matched", function (done) {
  //! given
  try { fs.mkdirSync("tmp")           } catch (e) {}
  try { fs.mkdirSync("tmp/test")      } catch (e) {}
  fs.writeFileSync("tmp/test/file.txt", "some data\nlala")
  fs.writeFileSync("tmp/test/another.txt", "some other data")
  fs.writeFileSync("tmp/test/another.backup", "not match")

  var listener = {
    file: function (file) {
      (this.file.calls = this.file.calls || {})[file.file.path] = file },

    complete: function () {
      try {
        assert.equal(listener.file.calls["tmp/test/file.txt"].file.path, "tmp/test/file.txt")
        assert.equal(listener.file.calls["tmp/test/file.txt"].text, "some data\nlala")

        assert.equal(listener.file.calls["tmp/test/another.txt"].file.path, "tmp/test/another.txt")
        assert.equal(listener.file.calls["tmp/test/another.txt"].text, "some other data")

        assert.equal(listener.file.calls["tmp/test/another.backup"], undefined)

        done()
      } catch (e) {
        fs.unlinkSync("tmp/test/file.txt")
        fs.unlinkSync("tmp/test/another.txt")
        fs.unlinkSync("tmp/test/another.backup")
        fs.rmdirSync("tmp/test")
        fs.rmdirSync("tmp")
        done(e)
      }
    }
  }

  //! when
  new Reader({ fullFile: true })
    .on(listener)
    .get("tmp/**/*.txt")

})

module.exports = Reader

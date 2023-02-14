var spec      = require("washington")
var assert    = require("assert")
var Mediador  = require("mediador")

var clone = function (object) {
  var clone = {}
  Object.keys(object).forEach(function (key) {
    clone[key] = object[key] })
  return clone
}

var isQuoted = function (position, source) {

  //! Double quotes!
  if (

    //! Is there at least one double quote before this character?
    source.substring(0, position).indexOf('"') != -1 &&

    //! Are there an odd amount of double quotes before this character?
    source.substring(0, position).match(/"/g).length % 2 == 1 &&

    //! Is there at least one double quote after this character?
    source.substring(position).indexOf('"') != -1)

      //! Then it is quoted!
      return true

  //! Single quotes!
  if (

    //! Is there at least one single quote before this character?
    source.substring(0, position).indexOf("'") != -1 &&

    //! Are there an odd amount of single quotes before this character?
    source.substring(0, position).match(/'/g).length % 2 == 1 &&

    //! Is there at least one single quote after this character?
    source.substring(position).indexOf("'") != -1)

      //! Then it is quoted!
      return true

  //! Otherwise it is not quoted
  return false

}

var Parser = function (options) {
  if (options) {
    this.commentStart     = options.commentStart
    this.escapeSequence   = options.escapeSequence
  }
}

Parser.prototype = Object.create(Mediador.prototype)

Parser.prototype.line = function (options) {
  var argument    = {}
  var position    = null
  var quoted      = true
  var substring   = options.line
  var newPosition = null

  if (!this.commentStart) {
    argument = clone(options)
    argument.code = options.line
    return this.emit("code", [argument]) }

  do {
    newPosition   = substring.indexOf(this.commentStart)
    quoted        = isQuoted(newPosition, substring)
    substring     = substring.substring(newPosition + this.commentStart.length)
    position      = position + newPosition + this.commentStart.length
  } while (quoted && newPosition !== -1 && substring)


  position = newPosition === -1 ? newPosition : position - this.commentStart.length

  if (position === -1) {
    argument = clone(options)
    argument.code = options.line
    return this.emit("code", [argument]) }

  if (this.escapeSequence
    && options.line
      .substring(
        position + this.commentStart.length,
        position + this.commentStart.length + this.escapeSequence.length) === this.escapeSequence) {

    argument = clone(options)
    argument.code = options.line

    return this.emit("code", [argument]) }

  if (position > 0
    && options.line.substring(0, position).trim() !== "") {

    argument.code = clone(options)
    argument.code.code = options.line
      .substring(0, position)

    argument.comment = clone(options)
    argument.comment.comment = options.line
      .substring(position + this.commentStart.length + 1)

    return this
      .emit("code", [argument.code])
      .emit("comment", [argument.comment]) }

  argument = clone(options)
  argument.comment = options.line
    .substring(position + this.commentStart.length + 1)

  this.emit("comment", [argument])
}

spec("should emit as 'code' by default and forward extras", function () {
  //! given
  var options = {
    line: "Some content",
    extra: "Some extra"
  }

  var listener = {
    code: function (code) {
      (this.code.calls = this.code.calls || [])
        .push(code) }
  }

  var emitter = new Mediador

  emitter.on(
    new Parser()
      .on(listener) )

  //! when
  emitter.emit("line", [options])

  //! then
  assert.equal(listener.code.calls[0].code, options.line)
  assert.equal(listener.code.calls[0].line, options.line)
  assert.equal(listener.code.calls[0].extra, options.extra)
})

spec("should emit as 'comment' when starts with comment token", function () {
  //! given
  var options = { line: "// comment" }
  var listener = {
    comment: function (comment) {
      (this.comment.calls = this.comment.calls || [])
        .push(comment) }
  }

  var emitter = new Mediador

  emitter.on(
    new Parser({ commentStart: "//"})
      .on(listener) )

  //! when
  emitter.emit("line", [options])

  //! then
  assert.equal(listener.comment.calls[0].comment, "comment")
})

spec("should emit 'code' and 'comment' if comment starts in the middle", function () {
  //! given
  var options = { line: "some code # comment" }
  var listener = {
    comment: function (comment) {
      (this.comment.calls = this.comment.calls || [])
        .push(comment) },

    code: function (code) {
      (this.code.calls = this.code.calls || [])
        .push(code) }
  }

  var emitter = new Mediador

  emitter.on(
    new Parser({ commentStart: "#"})
      .on(listener) )

  //! when
  emitter.emit("line", [options])

  //! then
  assert.equal(listener.code.calls[0].code, "some code ")
  assert.equal(listener.comment.calls[0].comment, "comment")
})

spec("should not emit 'code' if empty", function () {
  //! given
  var options = { line: "   # comment" }
  var listener = {
    comment: function (comment) {
      (this.comment.calls = this.comment.calls || [])
        .push(comment) },

    code: function (code) {
      (this.code.calls = this.code.calls || [])
        .push(code) }
  }

  var emitter = new Mediador

  emitter.on(
    new Parser({ commentStart: "#"})
      .on(listener) )

  //! when
  emitter.emit("line", [options])

  //! then
  assert.equal(listener.code.calls, undefined)
  assert.equal(listener.comment.calls[0].comment, "comment")
})


spec("should ignore comment when escaped", function () {
  //! given
  var options = { line: "some code #! comment" }
  var listener = {
    comment: function (comment) {
      (this.comment.calls = this.comment.calls || [])
        .push(comment) },

    code: function (code) {
      (this.code.calls = this.code.calls || [])
        .push(code) }
  }

  var emitter = new Mediador

  emitter.on(
    new Parser({ commentStart: "#", escapeSequence: "!" })
      .on(listener) )

  //! when
  emitter.emit("line", [options])

  //! then
  assert.equal(listener.code.calls[0].code, "some code #! comment")
  assert.equal(listener.comment.calls, undefined)
})

spec("should ignore comment sign when quoted with ''", function () {
  //! given
  var options = { line: "some code '#' comment" }
  var listener = {
    comment: function (comment) {
      (this.comment.calls = this.comment.calls || [])
        .push(comment) },

    code: function (code) {
      (this.code.calls = this.code.calls || [])
        .push(code) }
  }

  var emitter = new Mediador

  emitter.on(
    new Parser({
        commentStart: "#",
        escapeSequence: "!" })
      .on(listener) )

  //! when
  emitter.emit("line", [options])

  //! then
  assert.equal(listener.code.calls[0].code, "some code '#' comment")
  assert.equal(listener.comment.calls, undefined)
})

spec("should ignore comment sign when quoted with \"\"", function () {
  //! given
  var options = { line: "some code \"#\" comment" }
  var listener = {
    comment: function (comment) {
      (this.comment.calls = this.comment.calls || [])
        .push(comment) },

    code: function (code) {
      (this.code.calls = this.code.calls || [])
        .push(code) }
  }

  var emitter = new Mediador

  emitter.on(
    new Parser({
        commentStart: "#",
        escapeSequence: "!" })
      .on(listener) )

  //! when
  emitter.emit("line", [options])

  //! then
  assert.equal(listener.code.calls[0].code, "some code \"#\" comment")
  assert.equal(listener.comment.calls, undefined)
})

spec("should use comment sign when quoted only once", function () {
  //! given
  var options = { line: "some code \"#\" # comment" }
  var listener = {
    comment: function (comment) {
      (this.comment.calls = this.comment.calls || [])
        .push(comment) },

    code: function (code) {
      (this.code.calls = this.code.calls || [])
        .push(code) }
  }

  var emitter = new Mediador

  emitter.on(
    new Parser({
        commentStart: "#",
        escapeSequence: "!" })
      .on(listener) )

  //! when
  emitter.emit("line", [options])

  //! then
  assert.equal(listener.code.calls[0].code, "some code \"#\" ")
  assert.equal(listener.comment.calls[0].comment, "comment")
})

module.exports = Parser

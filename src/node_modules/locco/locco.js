// locco
// =====
//
// ![Codeship status](https://www.codeship.io/projects/173f7bd0-ad2d-0131-d326-5a3e053281b1/status)
//
// Simple documentation extractor.
//
// Usage
// -----
//
// ```sh
// locco --adapter=markdown --source=**/*.js --commentStart=// \
//       --escapeSequence=! --adapter-readme=locco.js
// ```
//
// - `adapter`: The adapter to be used. Locco searches for adapter matching the
//   `locco-<name>` pattern. Install markdown adapter with `npm install
//   -g locco-markdown`
// - `source`: The glob pattern matching the files to be parsed.
// - _optional_ `commentStart`: Start of the comment line. Defaults to `//`
// - _optional_ `escapeSequence`: Characters that right after the start of
//   comment indicate that the comment is not documentation.
// - _dependant on adapter_ `adapter-<property>`: option sent to the adapter
//
// Or programmatically:
//
// ```javascript
// var locco = require("locco")
// var loccoMarkdown = require("locco-markdown")
//
// locco({
//   source: "**/*.js",
//   commentStart: "//",
//   escapeSequence: "!",
//   adapter: new loccoMarkdown({
//     readme: "locco.js"
//   })
// })
// ```
//
// Installation
// ------------
//
// ```sh
// npm install -g locco
// ```
//
// Then you need to install some adapter for it or add your own.
//
// Adapters
// --------
//
// Look for NPMs starting with `locco-`.
//
// ### Known adapters
//
// - [`locco-markdown`](https://github.com/xaviervia/locco-markdown)
//
// ### Writing an adapter
//
// To just log each line to the console:
//
// ```javascript
// var locco = require("locco");
//
// var loggerAdapter = {
//   comment: function (data) {
//     console.log("From file: " + data.file.path)
//     console.log("...a comment line: " + data.comment)
//   },
//
//   code: function (data) {
//     console.log("From file: " + data.file.path)
//     console.log("...a code line: " + data.code)
//   }
// }
//
// locco({
//   source: "**/*.rb",
//   commentStart: "#",
//   escapeSequence: "!",
//   adapter: loggerAdapter
// })
// ```
//
// If you want to write to a file, the adapter should
// implement the event emitter interface (use
// [Mediador](https://github.com/xaviervia/mediador) if you don't know how
// to implement one) and emit `post` events with each line to be
// written.
//
// ```javascript
// var locco = require("locco")
// var Mediador = require("mediador")
//
// var loggerAdapter = {
//   comment: function (data) {
//     console.log("From file: " + data.file.path)
//     console.log("...a comment line: " + data.comment)
//     this.emit("post", [
//       "destination.file.html",
//       "<p>" + data.comment + "</p>"
//     ])
//   },
//
//   code: function (data) {
//     console.log("From file: " + data.file.path)
//     console.log("...a code line: " + data.code)
//     this.emit("post", [
//       "destination.file.html",
//       "<p><code>" + data.code + "</code></p>"
//     ])
//   }
// }
//
// loggerAdapter.on     = Mediador.prototype.on
// loggerAdapter.off    = Mediador.prototype.off
// loggerAdapter.emit   = Mediador.prototype.emit
//
// locco({
//   source: "**/*.rb",
//   commentStart: "#",
//   escapeSequence: "!",
//   adapter: loggerAdapter
// })
// ```
var spec              = require("washington")
var assert            = require("assert")

var locco = function (options) {

  new locco.File.Reader()
    .on(new locco.Parser(options)
      .on(
        options.adapter.on ?
          options.adapter.on("post", new locco.File.Writer().post) :
          options.adapter ))
    .get(options.source)

}

locco.File              = require("./src/file")
locco.Parser            = require("./src/parser")

module.exports = locco

//
// License
// -------
//
// Copyright 2014 Xavier Via
//
// ISC license.
//
// See [LICENSE](LICENSE) attached.

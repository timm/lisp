var locco = require("locco");

var loggerAdapter = {
  comment: function (data) {
    console.log("From file: " + data.file.path)
    console.log("...a comment line: " + data.comment)
  },

  code: function (data) {
    console.log("From file: " + data.file.path)
    console.log("...a code line: " + data.code)
  }
}

locco({
  source: "**/*.rb",
  commentStart: "#",
  escapeSequence: "!",
  adapter: loggerAdapter
})

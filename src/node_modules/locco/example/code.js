// This is a comment
// =================
//
// That will be output as _markdown_, eventually
var locco = require("../locco")

locco({
  commentStart: "//",
  escapeSequence: "!",
  source: "**/*.js",
  adapter: new (require("locco-" + "markdown"))({
    readme: "locco.js"
  })
})

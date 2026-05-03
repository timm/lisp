// Extend Prism's lisp grammar to recognise glu macros and project sigils.
// Loaded with Prism in data-manual mode so we modify grammar before any
// highlighting runs.

(function () {
  if (!window.Prism || !Prism.languages.lisp) {
    if (window.Prism) Prism.highlightAll();
    return;
  }

  Prism.languages.insertBefore('lisp', 'keyword', {

    // Project macros. Trailing (?=[\s()]) handles `glu+`, `let+`, `for/`
    // where `+` and `/` are non-word characters and \b alone fails.
    'glu-macro': {
      pattern: /\b(?:glu\+?|let\+|for\/|aif|defread|new|fnn?n?)(?=[\s()])/,
      alias: 'keyword'
    },

    // $foo and @foo
    'glu-sigil': {
      pattern: /[$@][\w\-?!*+/<>=]+/,
      alias: 'variable'
    },

    // ! and ? as the head of a form  (! f a)  (? x a b)
    'glu-op': {
      pattern: /(\()\s*[!?](?=\s)/,
      lookbehind: true,
      alias: 'keyword'
    },

    // _ __ ___ inside fn / fnn / fnnn forms
    'glu-anaphor': {
      pattern: /\b___?_?\b/,
      alias: 'variable'
    }
  });

  Prism.highlightAll();
})();

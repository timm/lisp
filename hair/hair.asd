;;; hair: tiny AI, explained. (ql:quickload "hair")
(defsystem "hair"
  :description "Tiny AI, explained: data, distances, stats and trees."
  :author "Tim Menzies <timm@ieee.org>"
  :license "MIT"
  :serial t
  :components ((:file "hr") (:file "ai")))

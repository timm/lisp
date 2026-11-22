;;; hair: tiny AI, explained. (ql:quickload "hair") = the model;
;;; (ql:quickload "hair/eg") = plus printing, egs and cli.
;;; Only this file and hair.lisp know the load order.
(defsystem "hair"
  :description "Tiny AI, explained: data, distances, stats and trees."
  :author "Tim Menzies <timm@ieee.org>"
  :license "MIT"
  :components ((:file "lithp")
               (:file "ai" :depends-on ("lithp"))))

(defsystem "hair/eg"
  :description "hair's printing, experiments and command line."
  :depends-on ("hair")
  :components ((:file "eg")))

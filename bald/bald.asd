;;; bald: tiny AI, explained. (ql:quickload "bald") = the model;
;;; (ql:quickload "bald/eg") = plus printing, egs and cli.
;;; Only this file and bald.lisp know the load order.
(defsystem "bald"
  :description "Tiny AI, explained: data, distances, stats and trees."
  :author "Tim Menzies <timm@ieee.org>"
  :license "MIT"
  :components ((:file "lithp")
               (:file "ai" :depends-on ("lithp"))))

(defsystem "bald/eg"
  :description "bald's printing, experiments and command line."
  :depends-on ("bald")
  :components ((:file "eg")))

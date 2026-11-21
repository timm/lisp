; vim: set ts=2 sw=2 sts=2 et lisp cc=75 lispwords+=loop :
;;; package: the hair package, plus its own readtable so `$x` works in
;;; hair's files without touching anyone else's reader.
;;; (c) 2026, Tim Menzies <timm@ieee.org>, MIT license
(defpackage :hair (:use :cl)
  (:export #:*k* #:about #:make-about
           #:tbl #:clone #:add #:adds #:mid #:div #:norm
           #:distx #:disty #:near #:same #:rank #:xval
           #:tree #:leaf #:show #:csv #:srand #:rand #:normal #:cli))
(in-package :hair)

(defvar *hair-readtable*         ; $x = (slot-value self 'x)
  (let ((r (copy-readtable nil)))
    (set-macro-character #\$
      (lambda (s c) (declare (ignore c))
        `(slot-value self ',(read s t nil t))) t r)
    r))

(defmacro in-hair () "Use hair's readtable for the rest of this file"
  ;; load and compile-file rebind *readtable* per file, so this is local
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *readtable* *hair-readtable*)))

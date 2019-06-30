;; vim: ts=2 sw=2 sts=2 et:
;-------- -------- -------- -------- -------- --------
(unless (fboundp 'got) (load "../got"))

(got "lib/rand.lisp")

(defun head (lst) "Return first item" (car lst))
(defun tail (lst) "Return last item" (car (last lst)))

(defun l->a (lst)
  "coerce list to array"
  (make-array (length lst) :initial-contents lst))

(defun a->l (a &key (lo 0) (hi (length a)))
  "coerce array to list"
  (coerce (subseq a lo hi) 'list))

(defun select (selector-fn facts)
  "return all list items satisying selector-fn"
  (remove-if-not selector-fn facts))

(defun nshuffle (sequence)
  (loop for i from (length sequence) downto 1
        do (rotatef (elt sequence (randi i))
                    (elt sequence (1- i))))
  sequence)

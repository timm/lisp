(defun nshuffle (seq)
	(loop :for i :from (length seq) :downto 2 :do 
		(rotatef (elt seq (random i)) (elt seq (1- i))))
	seq)

(defun bst+ (tree value)
  "Insert VALUE into the BST TREE."
  (if (null tree) 
    (list value nil nil)
    (let ((node  (first tree))
          (left  (second tree))
          (right (third tree)))
      (if (< value node)
        (list node (bst+ left value) right)
        (list node left (bst+ right value))))))

(defun show3 (tree &optional (lvl 0))
  (when tree
    (format t "~v@{~A ~:*~}" lvl "|..")
    (format t "~a~%" (first tree))
    (show3 (second tree) (+ 1 lvl))
    (show3 (third tree) (+ 1 lvl))))

;; Example usage
(dotimes (_ 10)
	(terpri)
	(let* ((tree nil) lst)
		(dotimes (_ 50) (push (random 1000) lst))
		(dolist (x lst) (setq tree (bst+  tree x)))
		(show3 tree)))




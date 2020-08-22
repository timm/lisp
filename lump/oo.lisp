; vim: noai:ts=2:sw=2:et: 
(load "got")
(got "my" "macros" "os")

(defmacro ? (x &rest fs) 
  `(getr  slot-value ,x ,@fs))

(let ((id 0))
  (defclass thing () 
    ((id :initform (incf id)))))

(defmacro defthing (x parent &rest slots)
  "Succinct class creation"
  `(defclass ,x (,parent)
     ,(loop for (slot form) in slots collect 
            (_defslot slot x form))))

(defmethod print-object ((it thing) out)
  "for things, print all public slots"
  (let ((lst (mapcar
               #'(lambda (s)
                   (list s (slot-value it s)))
               (sort (_public-slot-names it)
                     #'string<))))
    (format out "~a" 
            (cons (class-name (class-of it)) lst))))

;;; utils
(defun _defslot (slot x form)
  "Helper function for defthing"
  `(,slot
     :initarg  ,(intern (symbol-name slot) "KEYWORD")
     :initform ,form
     :accessor ,(intern (format nil "~a-~a" x slot))))


(defmethod _public-slot-names ((it thing))
  "return all thing slots that don't start with '_'"
  (remove-if
    #'(lambda (x)
        (and (symbolp x)
             (equal (elt (symbol-name x) 0) #\_)))
    (mapcar
      #'klass-slot-definition-name
      (klass-slots it))))


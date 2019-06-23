#-ish(load "../lib/ish")

(defun klass-slots (it)
  "what are the slots of a class?"
  #+clisp
  (class-slots (class-of it))
  #+sbcl
  (sb-mop:class-slots (class-of it)))

(defun klass-slot-definition-name (x)
  "what is a slot's name?"
  #+clisp
  (slot-definition-name x)
  #+sbcl
  (sb-mop:slot-definition-name x))

(defun args ()
  "what are the command line args?"
  #+clisp ext:*args*
  #+sbcl sb-ext:*posix-argv*
  #+allegro (sys:command-line-arguments))

(defun stop ()
  "how to halt the program?"
  #+sbcl (sb-ext:exit)
  #+:clisp (ext:exit)
  #+allegro (excl:exit))

(defun sh (cmd)
   "A multi-implementation function equivalent for the C function system"
   #+clisp (shell cmd)
   #+ecl (si:system cmd)
   #+sbcl (sb-ext:run-program "/bin/sh" (list "-c" cmd) :input nil :output *standard-output*)
   #+clozure (ccl:run-program "/bin/sh" (list "-c" cmd) :input nil :output *standard-output*))

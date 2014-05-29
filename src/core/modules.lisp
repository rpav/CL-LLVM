(in-package :llvm)

(defun make-module (name &optional (context (global-context)))
  (module-create-with-name-in-context name context))

(defun (setf data-layout) (triple m)
  (set-data-layout m triple)
  triple)

(defun (setf target) (triple m)
  (set-target m triple)
  triple)

(defun dump-module (m)
  (finish-output *error-output*)
  (%dump-module m))

(in-package :llvm)

(defun make-builder (&optional (context (global-context)))
  (create-builder-in-context context))

(defun position-builder (builder block &optional instr)
  (%position-builder builder block instr))


(defun insert-into-builder (builder instr &key name)
  (if name
    (insert-into-builder-with-name builder instr name)
    (%insert-into-builder builder instr)))

(defun build-ret (builder &rest values)
  (case (length values)
    (0 (build-ret-void builder))
    (1 (%build-ret builder (car values)))
    (otherwise (build-aggregate-ret builder values (length values)))))

(defun build-invoke (builder fn args then catch name)
  (%build-invoke builder fn args (length args) then catch name))

(defun build-gep (b pointer indices name)
  (%build-gep b pointer indices (length indices) name))

(defun build-in-bounds-gep (b pointer indices name)
  (%build-in-bounds-gep b pointer indices (length indices) name))

(defun build-call (builder fn args name)
  (%build-call builder fn args (length args) name))


(in-package :llvm)

(defun make-execution-engine (module)
  (create-or-error (out-ee out-error ptr llvm.ffi:execution-engine-ref)
      (create-execution-engine-for-module (out-ee &) module (out-error &))))

(defun make-interpreter (module)
  (create-or-error (out-interp out-error ptr llvm.ffi:execution-engine-ref)
      (create-interpreter-for-module (out-interp &) module (out-error &))))

(defun make-jit-compiler (module &optional (optimization-level :default))
  (create-or-error (out-jit out-error ptr llvm.ffi:execution-engine-ref)
      (create-jit-compiler-for-module (out-jit &) module optimization-level
                                      (out-error &))))

;; FIXME: Not handling args and env properly here
(defun run-function-as-main (execution-engine function args env)
  (%run-function-as-main execution-engine function (length args) args env))

(defun run-function (ee f args)
  (%run-function ee f (length args) args))

(defun remove-module-provider (ee m)
  (create-or-error (out-mod out-error ptr llvm.ffi:module-ref)
      (%remove-module ee m (out-mod &) (out-error &))
    (%dispose-module ptr)))

(defun find-function (ee name)
  (c-with ((out-fn :pointer))
    (when (= 0 (%find-function ee name (out-fn &)))
      (autowrap:make-wrapper-instance 'llvm.ffi:value-ref :ptr out-fn))))

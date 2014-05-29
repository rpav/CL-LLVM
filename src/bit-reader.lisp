(in-package :llvm)

(defun parse-bitcode (mem-buf &key (context (global-context)))
  (c-with ((out-module :pointer)
           (out-message :pointer))
    (if (= 0 (parse-bitcode-in-context context mem-buf
                                       (out-module &) (out-message &)))
        (making-autocollect-instance (ptr llvm.ffi:module-ref) out-module
          (%dispose-module ptr))
        (error 'llvm-error :message out-message))))

(defun dispose-module (module)
  (with-autocollect-cancel (module)
    (%dispose-module module)))

(defun bitcode-module (mem-buf &key (context (global-context)))
  (c-with ((out-message :pointer)
           (out-m :pointer))
    (if (= 0 (bitcode-module-in-context context mem-buf
                                        (out-m &)
                                        (out-message &)))
        (making-autocollect-instance (ptr llvm.ffi:module-ref) out-m
          (%dispose-module ptr))
        (error 'llvm-error :message out-message))))

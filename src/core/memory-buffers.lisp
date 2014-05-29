(in-package :llvm)

(defun make-memory-buffer (&optional path)
  (c-with ((out-mem-buf llvm.ffi:memory-buffer-ref)
           (out-message :pointer))
    (if (= (if path
               (create-memory-buffer-with-contents-of-file path
                                                           (out-mem-buf &)
                                                           (out-message &))
               (create-memory-buffer-with-stdin (out-mem-buf &)
                                                (out-message &)))
           0)
        (making-autocollect-instance (ptr llvm.ffi:memory-buffer-ref) out-mem-buf
          (%dispose-memory-buffer ptr))
        (error 'llvm-error :message out-message))))

(defun dispose-memory-buffer (buffer)
  (with-autocollect-cancel (buffer)
    (%dispose-memory-buffer buffer)))

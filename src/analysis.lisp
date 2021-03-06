(in-package :llvm)

(defun verify-module (m)
  (c-with ((out-message :pointer))
    (if (%verify-module m :return-status (out-message &))
      (error 'llvm-error :message out-message)
      t)))

(defun verify-function (fn)
  (= 0 (%verify-function fn :return-status)))


(in-package :llvm)

(let ((enum-subs
        '((:= . "LLVMIntEQ")
          (:/= . "LLVMIntNE")
          (:unsigned-> . "LLVMIntUGT")
          (:unsigned->= . "LLVMIntUGE")
          (:unsigned-< . "LLVMIntULT")
          (:unsigned-<= . "LLVMIntULE")
          (:> . "LLVMIntSGT")
          (:>= . "LLVMIntSGE")
          (:< . "LLVMIntSLT")
          (:<= . "LLVMIntSLE")
          (:false . "LLVMRealPredicateFalse")
          (:= . "LLVMRealOEQ")
          (:> . "LLVMRealOGT")
          (:>= . "LLVMRealOGE")
          (:< . "LLVMRealOLT")
          (:<= . "LLVMRealOLE")
          (:/= . "LLVMRealONE")
          (:ordered . "LLVMRealORD")
          (:unordered . "LLVMRealUNO")
          (:unordered-= . "LLVMRealUEQ")
          (:unordered-> . "LLVMRealUGT")
          (:unordered->= . "LLVMRealUGE")
          (:unordered-< . "LLVMRealULT")
          (:unordered-<= . "LLVMRealULE")
          (:unordered-/= . "LLVMRealUNE")
          (:true . "LLVMRealPredicateTrue"))))
  (defun llvm-foreign-type-symbol (string type package)
    "Custom symbol generation for LLVM* functions; otherwise use
the default autowrap version."
    (flet ((default ()
             (autowrap:default-foreign-type-symbol string type package)))
      (cond
        ((eq :cenumfield type)
         (let ((match (rassoc string enum-subs :test #'string=)))
           (if match
               (car match)
               (default))))
        (t (default))))))

(cffi:define-foreign-library libllvm
  (:darwin (:or (:default "libLLVM")
                (:default "libLLVM-3.4")
                (:default "libLLVM-3.4svn")))
  (:unix (:or "libLLVM.so" "libLLVM.so.1"
              "libLLVM-3.4.so" "libLLVM-3.4.so.1"
              "libLLVM-3.4svn.so" "libLLVM-3.4svn.so.1"))
  (t (:or (:default "libLLVM")
          (:default "libLLVM-3.4")
          (:default "libLLVM-3.4svn"))))

(cffi:use-foreign-library libllvm)

(in-package :llvm)

(define-condition llvm-error (error)
  ((message-string :reader message))
  (:report (lambda (condition stream)
             (write-string (message condition) stream))))

(defmethod initialize-instance :after ((object llvm-error)
                                       &key message &allow-other-keys)
  "This cleans up the memory used by the C string passed as the MESSAGE
   parameter."
  (setf (slot-value object 'message-string)
        (cffi:foreign-string-to-lisp message))
  (dispose-message message))

(define-condition required-parameter-error (error)
  ((parameter-name :initarg :name :reader name))
  (:report (lambda (condition stream)
             (format stream "~a is a required parameter."
                     (name condition)))))

(autowrap:define-bitmask-from-enum (attribute llvm.ffi:attribute))

(autowrap:define-foreign-enum 'optimization-level nil
  '((:none . 0)
    (:less . 1)
    (:default . 2)
    (:aggressive . 3)))

(defmacro carray-to-lisp (ptr type count)
  (alexandria:with-gensyms (i v)
    (alexandria:once-only (ptr count)
      (let ((foreign-type (autowrap:find-type type)))
        `(c-let ((,v ,type :ptr ,ptr))
           (loop for ,i from 0 below ,count
                 collect ,(if (and (not (typep foreign-type 'autowrap:foreign-pointer))
                                   (not (typep foreign-type 'autowrap:foreign-alias))
                                   (autowrap:foreign-scalar-p (autowrap:find-type 'llvm.ffi:value-ref)))
                              `(,v ,i)
                              `(autowrap:make-wrapper-instance ',type :ptr (,v ,i &)))))))))

(defmacro with-pointer-to-list  ((pointer-var type length) &body body)
  (alexandria:once-only (length)
    `(c-with ((,pointer-var ,type :count ,length))
       ,@body
       (carray-to-lisp (,pointer-var &) ,type ,length))))

(defmacro create-or-error ((var error-var ptr-var type)
                           creation-form cleanup-form)
  "Produce a wrapper for `TYPE` if `CREATION-FORM` returns 0;
otherwise produce an `LLVM-ERROR`.  `VAR` and `ERROR-VAR` are
temporary pointers provided to `CREATION-FORM` as per `C-WITH`.

`PTR-VAR` and `CLEANUP-FORM` are specified to dispose of the created
object."
  `(c-with ((,var :pointer)
            (,error-var :pointer))
     (if (/= 0 ,creation-form)
         (error 'llvm-error :message ,error-var)
         (making-autocollect-instance (,ptr-var ,type) ,var
           ,cleanup-form))))


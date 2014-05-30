(in-package :llvm)

(defmacro with-object ((var type &rest args) &body body)
  (let ((managers
          ;; Each element is of the form (designator constructor destructor)
          '((module make-module dispose-module)
            (builder make-builder dispose-builder)
            (memory-buffer make-memory-buffer)
            (type-handle create-type-handle dispose-type-handle)
            (context context-create dispose-context)
            (pass-manager create-pass-manager dispose-pass-manager)
            (function-pass-manager create-function-pass-manager-for-module dispose-pass-manager)
            (target-data create-target-data dispose-target-data)
            (generic-value-of-int create-generic-value-of-int dispose-generic-value)
            (generic-value-of-pointer create-generic-value-of-pointer dispose-generic-value)
            (generic-value-of-float create-generic-value-of-float dispose-generic-value)
            (execution-engine make-execution-engine)
            (interpreter make-interpreter)
            (jit-compiler make-jit-compiler))))
    (let ((type (assoc type managers :test 'string-equal)))
      `(let ((,var (,(second type) ,@args)))
         ,(if (third type)
              `(unwind-protect (progn ,@body)
                 (,(third type) ,var))
              `(progn ,@body))))))

(defmacro with-objects ((&rest bindings) &body body)
  (if (endp bindings)
    `(progn ,@body)
    `(with-object ,(car bindings)
       (with-objects ,(cdr bindings)
         ,@body))))

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

(defmacro carray-to-list (ptr type count)
  (alexandria:with-gensyms (i v)
    (alexandria:once-only (ptr count)
      (let ((foreign-type (autowrap:find-type type)))
        `(c-let ((,v :pointer :ptr ,ptr))
           (loop for ,i from 0 below ,count
                 collect ,(if (and (not (typep foreign-type 'autowrap:foreign-pointer))
                                   (not (typep foreign-type 'autowrap:foreign-alias))
                                   (autowrap:foreign-scalar-p (autowrap:find-type 'llvm.ffi:value-ref)))
                              `(,v ,i)
                              `(autowrap:make-wrapper-instance ',type :ptr (,v ,i)))))))))

(defmacro with-pointer-vector-to-carray ((array-var vector) &body body)
  (alexandria:once-only (vector)
    (alexandria:with-gensyms (i)
      `(c-with ((,array-var :pointer :count (length ,vector)))
         (loop for ,i from 0 below (length ,vector)
               do (setf (,array-var ,i) (ptr (aref ,vector ,i))))
         ,@body))))

(defmacro with-pointer-to-list ((pointer-var type length) &body body)
  (alexandria:once-only (length)
    `(c-with ((,pointer-var ,type :count ,length))
       ,@body
       (carray-to-list (,pointer-var &) ,type ,length))))

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


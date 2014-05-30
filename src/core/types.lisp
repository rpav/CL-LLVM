(in-package :llvm)


(defun int1-type (&key (context (global-context)))
  (int1-type-in-context context))

(defun int8-type (&key (context (global-context)))
  (int8-type-in-context context))

(defun int16-type (&key (context (global-context)))
  (int16-type-in-context context))

(defun int32-type (&key (context (global-context)))
  (int32-type-in-context context))

(defun int64-type (&key (context (global-context)))
  (int64-type-in-context context))

(defun int-type (num-bits &key (context (global-context)))
  (int-type-in-context context num-bits))

(defun float-type (&key (context (global-context)))
  (float-type-in-context context))

(defun double-type (&key (context (global-context)))
  (double-type-in-context context))

(defun x86-fp80-type (&key (context (global-context)))
  (x86-fp80-type-in-context context))

(defun fp128-type (&key (context (global-context)))
  (fp128-type-in-context context))

(defun ppc-fp128-type (&key (context (global-context)))
  (ppc-fp128-type-in-context context))

(defun function-type (return-type param-types &key var-arg-p)
  (with-pointer-vector-to-carray (c-param-types param-types)
    (%function-type return-type (c-param-types &) (length param-types)
                    (if var-arg-p 1 0))))

(defun param-types (function-ty)
  (with-pointer-to-list (pointer llvm.ffi:type-ref (count-param-types function-ty))
    (%get-params function-ty pointer)))

(defun struct-type (element-types packed &key (context (global-context)))
  (struct-type-in-context context element-types (length element-types) packed))

(defun struct-element-types (struct-ty)
  (with-pointer-to-list (pointer llvm.ffi:type-ref (count-struct-element-types struct-ty))
    (%struct-element-types struct-ty pointer)))

(defun struct-set-body (struct-type element-types &optional (packed nil))
  (%struct-set-body struct-type element-types (length element-types) packed))

(defun pointer-type (element-type &optional (address-space 0))
  (%pointer-type element-type address-space))

(defun void-type (&key (context (global-context)))
  (void-type-in-context context))

(defun label-type (&key (context (global-context)))
  (label-type-in-context context))

(defun opaque-type (&rest dummy)
  (declare (ignore dummy))
  (error "LLVMGetOpaqueTypeInContext no longer exists"))

(in-package :llvm)

(defun (setf value-name) (name val)
  (set-value-name val name)
  name)

(defun value-name (val)
  (%get-value-name val))

(defun dump-value (val)
  (finish-output *error-output*)
  (%dump-value val))

(defun const-int (int-ty value &optional radix)
  (if (typep value 'string)
      (const-int-of-string int-ty value radix)
      (let* ((+max-primitive-width+ 64)
             (bitmask (1- (expt 2 +max-primitive-width+)))
             (width (width int-ty)))
        (if (<= width +max-primitive-width+)
            (%const-int int-ty (logand value bitmask) 0)
            (let* ((length (ceiling width +max-primitive-width+))
                   (words (loop for num = value
                             then (ash num (- +max-primitive-width+))
                             while (< 0 num)
                             collect (logand num bitmask))))
              (const-int-of-arbitrary-precision int-ty length words))))))

(defun const-real (real-ty value)
  (if (typep value 'string)
      (const-real-of-string real-ty value)
      (%const-real real-ty value)))

;;; FIXME: is it right to hardcode dont-null-terminate here?
(defun const-string (str dont-null-terminate &key (context (global-context)))
  (const-string-in-context context str (length str) dont-null-terminate))

(defun const-struct (constant-vals packed &key (context (global-context)))
  (const-struct-in-context context constant-vals (length constant-vals) packed))

(defun const-array (element-ty constant-vals)
  (%const-array element-ty constant-vals (length constant-vals)))

(defun const-vector (scalar-constant-vals)
  (%const-vector scalar-constant-vals (length scalar-constant-vals)))


(defun const-gep (constant-val constant-indices)
  (%const-gep constant-val constant-indices (length constant-indices)))

(defun const-in-bounds-gep (constant-val constant-indices)
  (%const-in-bounds-gep constant-val
                        constant-indices (length constant-indices)))

(defun const-extract-value (agg-constant idx-list)
  (%const-extract-value agg-constant idx-list (length idx-list)))

(defun const-insert-value (agg-constant element-value-constant idx-list)
  (%const-insert-value agg-constant element-value-constant
                       idx-list (length idx-list)))

(defun (setf linkage) (linkage global)
  (set-linkage global linkage)
  linkage)

(defun (setf section) (section global)
  (set-section global section)
  section)

(defun (setf visibility) (viz global)
  (set-visibility global viz)
  viz)

(defun (setf alignment) (bytes global)
  (set-alignment global bytes)
  bytes)

(defun (setf initializer) (constant-val global-var)
  (set-initializer global-var constant-val)
  constant-val)

(defun (setf thread-local-p) (is-thread-local global-var)
  (set-thread-local global-var is-thread-local)
  is-thread-local)

(defun (setf global-constant-p) (is-constant global-var)
  (set-global-constant global-var is-constant)
  is-constant)


(defun (setf function-calling-convention) (cc fn)
  (set-function-call-conv fn cc)
  cc)

(defun (setf gc) (name fn)
  (set-gc fn name)
  name)

(defun add-function-attributes (fn &rest attributes)
  (add-function-attr fn attributes))

(defun get-function-attribute (function)
  (function-attr function))

(defun remove-function-attributes (fn &rest attributes)
  (remove-function-attr fn attributes))

(defun params (fn)
  (with-pointer-to-list (pointer llvm.ffi:value-ref (count-params fn))
    (%get-params fn (pointer &))))

(defun add-attributes (arg &rest attributes)
  (add-attribute arg attributes))

(defun remove-attributes (arg &rest attributes)
  (remove-attribute arg attributes))

(defun (setf param-alignment) (align arg)
  (set-param-alignment arg align)
  align)

(defun basic-blocks (fn)
  (with-pointer-to-list (pointer llvm.ffi:basic-block-ref (count-basic-blocks fn))
    (%get-basic-blocks fn pointer)))

(defun append-basic-block (fn name &key (context (global-context)))
  (append-basic-block-in-context context fn name))

(defun insert-basic-block (insert-block name &key (context (global-context)))
  (insert-basic-block-in-context context insert-block name))

(defun (setf instruction-calling-convention) (cc instr)
  (set-instruction-call-conv instr cc)
  cc)

(defun add-instruction-attributes (instr index &rest attributes)
  (add-instr-attribute instr index attributes))

(defun remove-instruction-attributes (instr index &rest attributes)
  (remove-instr-attribute instr index attributes))

(defun (setf instruction-param-alignment) (align instr index)
  (set-instr-param-alignment instr index align)
  align)

(defun (setf tail-call-p) (is-tail-call call-inst)
  (set-tail-call call-inst is-tail-call)
  is-tail-call)

(defun add-incoming (phi-node incoming-values incoming-blocks)
  (%add-incoming phi-node
                 incoming-values incoming-blocks (length incoming-values)))

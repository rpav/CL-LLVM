(in-package :llvm)

;;; NOTE: Add any new LLVM targets to the following list.
(eval-when (:compile-toplevel :load-toplevel)
  (defvar *targets* '("MBLAZE" "CPPBACKEND" "MSIL" "CBACKEND" "BLACKFIN"
                      "SYSTEMZ" "MSP430" "XCORE" "PIC16" "CELLSPU" "MIPS" "ARM"
                      "ALPHA" "POWERPC" "SPARC" "X86"))
  (defvar *target-info-functions* nil)
  (defvar *target-functions* nil))

(defmacro declare-targets ()
  `(progn ,@(mapcan (lambda (target)
                      `((push (intern ,(format nil "INITIALIZE-~A-TARGET-INFO"
                                               target))
                              *target-info-functions*)
                        (push (intern ,(format nil "INITIALIZE-~A-TARGET"
                                               target))
                              *target-functions*)))
                    *targets*)))

(declare-targets)
(export *target-info-functions*)
(export *target-functions*)

(defun initialize-all-target-infos ()
  (mapc #'funcall *target-info-functions*)
  (values))

(defun initialize-all-targets ()
  (mapc #'funcall *target-functions*)
  (values))

(defun initialize-native-target ()
  #+mips (progn (initialize-mips-target-info) (initialize-mips-target)
                (initialize-mips-target-mc) t)
  #+alpha (progn (initialize-alpha-target-info) (initialize-alpha-target) t)
  #+(or ppc ppc64)
  (progn (initialize-powerpc-target-info) (initialize-powerpc-target)
         (initialize-power-pc-target-mc) t)
  #+(or sparc sparc64)
  (progn (initialize-sparc-target-info) (initialize-sparc-target)
         (initialize-sparc-target-mc) t)
  #+(or x86 x86-64)
  (progn (initialize-x86-target-info) (initialize-x86-target)
         (initialize-x86-target-mc) t)
  #-(or mips alpha ppc ppc64 sparc sparc64 x86 x86-64) nil)

(defun string-representation (target-data)
  (multiple-value-bind (str ptr)
      (copy-string-rep-of-target-data target-data)
    (prog1 str
      (dispose-message ptr))))

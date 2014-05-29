(defpackage llvm-system
  (:use #:cl #:asdf))

(in-package :llvm-system)

(defsystem llvm
  :description "CFFI bindings to the LLVM libraries."
  :long-description "LLVM is a collection of modular and reusable compiler and
                     toolchain technologies. This library makes it easy (and
                     hopefully intuitive) to use them in Common Lisp."
  :license "MIT"
  :author "Greg Pfeil <greg@technomadic.org>"
  :depends-on (alexandria trivial-features trivial-garbage cl-autowrap cl-plus-c)
  :pathname "src/"
  :components
  ((:file "package")
   (:file "cffi" :depends-on ("package"))
   (:module "autowrap-spec"
            :pathname "spec"
            :components ((:static-file "llvm-3.4.h")))
   (:file "autowrap" :depends-on ("package" "cffi" "autowrap-spec"))
   (:module "core"
            :depends-on ("package" "cffi" "autowrap")
            :components ((:file "error-handling")
                         (:file "modules"
                                :depends-on ("error-handling"))
                         (:file "types"
                                :depends-on ("modules" "error-handling"))
                         (:file "values"
                                :depends-on ("modules" "error-handling"))
                         (:file "instruction-builders"
                                :depends-on ("error-handling"))
                         (:file "memory-buffers"
                                :depends-on ("error-handling"))))
   (:module "" :pathname ""
            :depends-on ("package" "cffi" "core")
            :components ((:file "analysis")
                         (:file "bit-reader")
                         (:file "bit-writer")
                         (:file "execution-engine")
                         (:file "target")))))

;;; NOTE: In order to load and run the Kaleidoscope tutorial, you first need to
;;;       run `./build-library.sh` in the tutorial subdirectory.

(defsystem kaleidoscope
    :description "A translation of the language created in the LLVM tutorial."
    :depends-on (llvm)
    :pathname "tutorial/"
    :components ((:file "cffi")
                 (:file "chapter2")
                 (:file "chapter3")
                 (:file "chapter4")
                 (:file "chapter5")
                 (:file "chapter6")
                 (:file "chapter7")))

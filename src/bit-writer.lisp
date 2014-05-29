(in-package :llvm)

(defun write-bitcode-to-file-handle (m handle)
  (if (%write-bitcode-to-file-handle m handle)
    (error "Failed writing bitcode to file handle.")))

(defun write-bitcode-to-file (m path)
  (if (%write-bitcode-to-file m path)
    (error "Failed writing bitcode to file.")))

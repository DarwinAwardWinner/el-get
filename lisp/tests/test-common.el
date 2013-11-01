(require 'ert)

(setq
 el-get-test-path
 (file-name-directory (or load-file-name (buffer-file-name)))
 el-get-lisp-path
 (file-name-directory el-get-test-path)
 el-get-async-path
 (expand-file-name "../contrib/async" el-get-lisp-path)
 load-path
 (nconc
  (list el-get-lisp-path el-get-async-path)
  load-path))

(ert-deftest load-el-get ()
  (require 'el-get-internals)
  (require 'el-get-lock)
  (require 'el-get-fetcher-registry)
  (require 'el-get-recipe-manip)
  (require 'el-get-recipe-io))

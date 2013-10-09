(require 'cl)
(require 'async)

;; TODO Move to appropriate place
(defconst el-get-emacs (concat invocation-directory invocation-name)
  "Path to find the currently running emacs.")

(defun* el-get--async (expr &key
                            finish-func
                            (subproc-default-directory
                             default-directory)
                            (subproc-load-path load-path)
                            require-features load-files
                            export-variables)
  "Eval EXPR asynchronously in a batch-mode Emacs subprocess,

TODO DOC"
  (let* (;; Make sure these are all lists
         (subproc-load-path
          (cond
           ((listp subproc-load-path)
            subproc-load-path)
           ((stringp subproc-load-path)
            (list subproc-load-path))
           ;; Fallback: use current `load-path'
           (t load-path)))
         (require-features
          (if (listp require-features)
              require-features
            (list require-features)))
         (load-files
          (if (listp load-files)
              load-files
            (list load-files)))
         (export-variables
          (cl-mapcan
           (lambda (var)
             (cond
              ((symbolp var)
               (cons var (eval var)))
              ((consp var)
               var)
              (t var)))
           export-variables))
         (full-expr
          `(progn
             (setq load-path ',subproc-load-path)
             (mapc #'require ',require-features)
             (mapc #'load ',load-files)
             (setq ,@export-variables
                   default-directory ,subproc-default-directory)
             ,expr)))
    (async-start `(lambda () ,full-expr) finish-func)))

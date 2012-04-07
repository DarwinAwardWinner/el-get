(require 'el-get)
(require 'cl)
(setq debugger-batch-max-lines (+ 50 max-lisp-eval-depth)
      debug-on-error t
      el-get-verbose t
      el-get-notify-type 'message)

(defmacro define-test-package (pkgname &optional code &rest additional-props)
  (condition-case nil
      (el-get-method :test :install)
    (error (el-get-register-method-alias :test :builtin)))
  (let* ((package-filename (format "%s.el" pkgname))
         (package-text
          (mapconcat #'pp-to-string
                     (cons `(provide ',pkgname) code)
                     "\n"))
         (package-def
          (append
           `(:name ,pkgname

                   :type test
                   :compile "."
                   :features ,pkgname
                   :build '(,(format "echo %s > %s"
                                     (shell-quote-argument package-text)
                                     (shell-quote-argument package-filename))))
           (mapcar 'eval additional-props))))
    (list 'quote package-def)))

;; https://github.com/dimitri/el-get/issues/693
;;
;; Literate user settings (.org config)

;; Make sure to test this with emacs 23 and 24
(assert (symbol-function 'define-test-package))
(message "HELLO2")

(setq debug-on-error t
      el-get-default-process-sync t
      el-get-verbose t
      el-get-is-lazy nil
      el-get-sources
      (loop for pkg in
            '(regular-config-pkg literate-config-pkg)
            collect (eval `(define-test-package ,pkg)))
      el-get-user-package-directory
      (file-name-as-directory
       (concat user-emacs-directory
               "el-get-package-configs")))

(pp el-get-sources)

;; Set up regular and literate init files
(make-directory el-get-user-package-directory 'recursive)
(with-temp-file (concat el-get-user-package-directory
                        "init-regular-config-pkg.el")
  (insert "
  (setq test:el-get-regular-config \"Success\")
  (el-get-notify \"Regular Config\" \"file loaded\")
"))
(with-temp-file (concat el-get-user-package-directory
                        "init-literate-config-pkg.org")
  (insert "
* Initialization test
Test to ensure that the file has loaded
#+name: org-mode-test-init
#+begin_src emacs-lisp
  (setq test:el-get-literate-config \"Success\")
  (el-get-notify \"Org Mode Config\" \"file loaded\")
#+end_src
"))

(el-get 'sync 'regular-config-pkg 'literate-config-pkg)
;; (require 'regular-config-pkg)
;; (require 'literate-config-pkg)
(assert (bound-and-true-p test:el-get-regular-config) nil
        "Regular user config file was loaded.")
(assert (bound-and-true-p test:el-get-literate-config) nil
        "Literate user config file was loaded.")

(require 'el-get)

(setq
 warning-minimum-level :debug
 warning-minimum-log-level :debug
 print-level nil
 print-length nil
 debugger-batch-max-lines (+ 50 max-lisp-eval-depth)
 ;; debug-on-error nil
 )


(setq
 load-path
 (nconc (mapcar (lambda (pth) (expand-file-name pth default-directory))
                (list "." "fetchers" "../contrib/async"))
        load-path))

;; (show-value load-path)

(defmacro show-value (object)
  `(message "Value of %S:\n%s"
            ',object
            ',(el-get-print-to-string (eval object) 'pretty)))

(show-value (el-get-fetcher-prop 'noop :fetch))
(show-value (el-get-resolve-recipe 'recipe1))
(show-value (el-get-resolve-recipe 'recipe2))
(show-value (el-get-resolve-recipe 'recipe2 :devirtualize t))
(show-value (el-get-resolve-recipe "recipe2" :devirtualize t))
(show-value (el-get-resolve-recipe '(:name recipe4 :type no-op)
              :devirtualize t))
(show-value (el-get-fetcher-prop
             (el-get-devirtualize-recipe-def '(:name recipe3 :type null))
             :fetch))

(let ((max-lisp-eval-depth 100))
  (show-value
   (el-get-hash-to-plist
    (el-get-dependency-graph 'a
      :overrides
      '((:name a :type noop :depends (b c d))
        (:name b :type noop)
        (:name c :type noop)
        (:name d :type noop)))))

  (show-value
   (el-get-hash-to-plist
    (el-get-dependency-graph (list 'a)
      :overrides
      '((:name a :type noop :depends (b c d))
        (:name b :type noop)
        (:name c :type noop)
        (:name d :type noop)))))

  (show-value
   (el-get-extract-dependency-list
    'a
    (make-hash-table)))

  (show-value
   (el-get-extract-dependency-list
    'a
    (el-get-dependency-graph 'a
      :overrides
      '((:name a :type noop :depends (b c d))
        (:name b :type noop)
        (:name c :type noop)
        (:name d :type noop)))))

  (show-value
   (el-get-dependency-list '(a)
     :overrides
     '((:name a :type noop :depends (b c d))
       (:name b :type noop)
       (:name c :type noop)
       (:name d :type noop)))))

(show-value (el-get-status-plist-valid
             '(:status removed)
             'b))
(show-value (el-get-status-plist-valid
             `(:status fetched :recipe ,(el-get-devirtualize-recipe-def
                                         '(:name b :type noop)))
             'b))

(show-value
(el-get-validate-recipe
 '(:name ack
   :type file
   :url "http://repo.or.cz/w/ShellArchive.git/blob_plain/HEAD:/ack.el")
 nil "ack"))

(el-get-delete-directory-contents el-get-install-dir 'force)
(el-get-fetch-package '(:name b :type noop))
(el-get-build-package 'b)
(el-get-remove-package 'b)
(el-get-fetch-package '(:name c :type null))
(el-get-build-package 'c)
(el-get-remove-package 'c)

(el-get-normalize-build-property
 '("echo HELLO FROM SINGLE-STRING COMMAND PACKAGE D"
   ("echo" "HELLO FROM PACKAGE D")))

(el-get-fetch-package
 '(:name
   d
   :type null
   :build
   ("echo HELLO FROM SINGLE-STRING COMMAND PACKAGE D"
    ("echo" "HELLO FROM PACKAGE D"))))

(el-get-fetch-package
 '(:name
   e
   :type null
   :build
   (lambda ()
     (message "HELLO FROM PACAKGE E"))))

(el-get-build-package 'd)
(el-get-build-package 'e)

(el-get-fetch-package (el-get-read-recipe 'ack))
(el-get-build-package 'ack)

;; Init ack and see if we can actually use the autoloads
(el-get-init-package 'ack)
(show-value (symbol-function 'ack))
(save-window-excursion
  (save-excursion
    (ack "hello")))

(el-get-fetch-package '(:name osx-pseudo-daemon
                              :type file
                              :url
                              "https://gist.github.com/DarwinAwardWinner/5882719/raw/ed9046cccaa78633793a407efd1888c7d4667ae4/emacs-osx-pseudo-daemon.el"
                              :file-name "osx-pseudo-daemon.el"
                              :autoloads t))
(el-get-build-package 'osx-pseudo-daemon)


(condition-case err
    (el-get-fetch-package
     '(:name nonexistent-url
             :type file
             :url
             "http://fdjksalfgdjkgfjldkfjdsklafsd.com/test.el"
             :autoloads t))
  (error (message "Got expected error: %S" err)))

(condition-case err
    (el-get-fetch-package (el-get-read-recipe 'this-is-not-a-package))
  (el-get-error (message "Got expected error: %S" err)))

(message "All tests finished successfully! :)")

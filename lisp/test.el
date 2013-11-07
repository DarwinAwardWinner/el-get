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

(el-get-delete-directory-contents el-get-install-dir 'force)
(el-get-fetch-package '(:name b :type noop))
(el-get-build-package 'b)
(el-get-remove-package 'b)
(el-get-fetch-package '(:name c :type null))
(el-get-build-package 'c)
(el-get-remove-package 'c)

(message "All tests finished successfully! :)")

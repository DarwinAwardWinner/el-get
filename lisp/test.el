(require 'el-get)

(setq
 ;; warning-minimum-level :debug
 ;; warning-minimum-log-level :debug
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

(show-value (el-get-fetcher-op 'noop :fetch))
(show-value (el-get-resolve-recipe 'recipe1))
(show-value (el-get-resolve-recipe 'recipe2))
(show-value (el-get-resolve-recipe 'recipe2 :devirtualize t))
(show-value (el-get-resolve-recipe "recipe2" :devirtualize t))
(show-value (el-get-resolve-recipe '(:name recipe4 :type no-op)
              :devirtualize t))
(show-value (el-get-fetcher-op
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

(message "All tests finished successfully! :)")

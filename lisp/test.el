(setq
 ;; warning-minimum-level :debug
 ;; warning-minimum-log-level :debug
 debug-on-error t)


(setq
 load-path
 (nconc (mapcar (lambda (pth) (expand-file-name pth default-directory))
                (list "." "fetchers" "../contrib/async"))
        load-path))

;; (show-value load-path)

(require 'el-get-internals)
(defmacro show-value (object)
  `(message "Value of %S:\n%s"
            ',object
            ',(el-get-print-to-string (eval object) 'pretty)))

(require 'el-get-recipe-io)
(require 'el-get-noop-fetcher)
(require 'el-get-testvirt-fetcher)


(show-value (el-get-fetcher-op 'noop :fetch))

(show-value (el-get-resolve-recipe 'recipe1))
(show-value (el-get-resolve-recipe 'recipe2))
(show-value (el-get-resolve-recipe 'recipe2 'devirtualized))
(show-value (el-get-resolve-recipe "recipe2" 'devirtualized))
(show-value (el-get-resolve-recipe '(:name recipe4 :type no-op) t))
(show-value (el-get-fetcher-op
             (el-get-devirtualize-recipe-def '(:name recipe3 :type null))
             :fetch))

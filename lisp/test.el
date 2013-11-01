(setq
 ;; warning-minimum-level :debug
 ;; warning-minimum-log-level :debug
 debug-on-error nil)


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

(require 'el-get-dependencies)
;; (show-value (el-get-fetcher-op 'noop :fetch))

;; (show-value (el-get-resolve-recipe 'recipe1))
;; (show-value (el-get-resolve-recipe 'recipe2))
;; (show-value (el-get-resolve-recipe 'recipe2 :devirtualize t))
;; (show-value (el-get-resolve-recipe "recipe2" :devirtualize t))
;; (show-value (el-get-resolve-recipe '(:name recipe4 :type no-op) t))
;; (show-value (el-get-fetcher-op
;;              (el-get-devirtualize-recipe-def '(:name recipe3 :type null))
;;              :fetch))

(show-value
 (el-get-dependency-graph '((:name a :type noop :depends (b c d))
                            (:name b :type noop)
                            (:name c :type noop)
                            (:name d :type noop))))

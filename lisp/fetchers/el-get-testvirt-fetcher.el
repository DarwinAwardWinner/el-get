;; TODO Delete this file
(require 'el-get-fetcher-registry)

(el-get-register-fetcher
 :type 'testvirt
 :filter
 (lambda (recipe)
   (el-get-merge-plists
    (list :type 'noop
          :originally-a-testvirt-type-recipe "yes")
    recipe)))

(provide 'el-get-testvirt-fetcher)

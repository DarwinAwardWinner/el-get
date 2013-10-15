(require 'el-get-fetcher-registry)

(el-get-register-virtual-fetcher 'testvirt
  (lambda (recipe)
    (el-get-recipe-put recipe :type 'noop)))

(provide 'el-get-testvirt-fetcher)

(require 'el-get-fetcher-registry)

(el-get-register-fetcher
  :type 'noop
  :fetch #'ignore
  :auto-property
  #'(lambda (recipe prop)
      (case prop
        (:compile 'none))))

(loop for alias in '(no-op null)
      do (el-get-register-fetcher-alias alias 'noop))

(provide 'el-get-noop-fetcher)

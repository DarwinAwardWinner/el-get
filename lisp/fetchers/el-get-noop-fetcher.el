(el-get-register-fetcher 'noop
  :fetch #'ignore)

(loop for alias in '(no-op null)
      do (el-get-register-fetcher-alias alias 'noop))

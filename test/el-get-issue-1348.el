;;; https://github.com/dimitri/el-get/issues/1348
;;; elpa recipes that depend on emacs

(setq el-get-sources
      '(package
        (:name org-agenda-property
               :type elpa
               :repo ("melpa" . "http://melpa.milkbox.net/packages/"))))

(el-get 'sync 'org-agenda-property)

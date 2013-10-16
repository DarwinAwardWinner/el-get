;; (defun el-get-fetch-file (recipe destdir)
;;   (el-get-plist-bind recipe
;;     )

;;   (let ((url (el-get-plist-get-nodef recipe :url))
;;         (destfile (plist-get recipe :)))))

;; (defun el-get-checksum-file (recipe destdir))

;; (defun el-get-guess-meta-from-file (&rest ignored)
;;   "TODO try to parse file header if require packages available"
;;   (ignore))

;; (el-get-register-fetcher 'file
;;   :fetch #'el-get-fetch-file
;;   :compute-checksum #'el-get-checksum-file
;;   :guess-metadata #'el-get-guess-meta-from-file
;;   :validate #'el-get-validate-file-recipe)

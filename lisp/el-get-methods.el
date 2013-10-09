(defvar el-get-fetchers (make-hash-table :size 20)
  "Hash table of registered package fetching methods.

TODO DOC")

(defsubst el-get--get-fetcher (type)
  "Retrive full fetcher definition for TYPE."
  (gethash type el-get-fetchers))

(defsubst el-get--set-fetcher (type def)
  "Set fetcher definition for TYPE to DEF."
  (puthash type def el-get-fetchers))

(defsubst el-get-fetcher-op (type operation)
  "Return the OPERATION function for fetcher type TYPE.

TYPE may either be a symbol that names a fetcher type or the
definition of one."
  (when type
    (if (hash-table-p type)
        (gethash operation type)
      (el-get-fetcher-op operation (el-get--get-fetcher type)))))

(defsubst el-get-fetcher-real-p (type)
  "Returns t if TYPE is a real fetcher type.

TYPE may either be a symbol naming a fetcher type or a hash table
that defines a fetcher type."
  (el-get-fetcher-op type :fetch))

(defsubst el-get-fetcher-virtual-p (type)
  "Returns t if TYPE is a virtual fetcher type.

TYPE may either be a symbol naming a fetcher type or a hash table
that defines a fetcher type."
  (el-get-fetcher-op type :filter))

(defsubst el-get-fetcher-defined-p (type)
  "Returns t if TYPE is a registered fetcher type."
  (or (el-get-fetcher-real-p type)
      (el-get-fetcher-virtual-p type)))

(defun* el-get-register-fetcher (type &key fetch update remove
                                     compute-checksum guess-metadata)
  "TODO DOC"
  ;; Sanity check
  (unless (and type (symbolp type))
    (error "TYPE must be a symbol"))
  ;; Warn about redefining fetchers
  (when (gethash type el-get-fetchers)
    (el-get-display-warning (format "Re-registering recipe type %s" type)
                            :debug))
  (let ((fetcher-def (make-hash-table :size 5)))
    (loop for required-key in '(fetch)
          for value = (eval required-key)
          unless value
          do (error "Missing required keyword argument: :%s" required-arg)
          do (puthash (intern (format ":%s" required-arg))
                      value fetcher-def))
    (loop for optional-key in '(update remove compute-checksum guess-metadata)
          for value = (eval optional-key)
          if value
          do (puthash (intern (format ":%s" optional-arg))
                      value fetcher-def))
    (puthash :type type fetcher-def)
    (el-get--set-fetcher type fetcher-def)))
(put 'el-get-register-fetcher 'lisp-indent-function 1)

(defun el-get-register-fetcher-alias (newtype oldtype)
  "TODO DOC"
  (el-get--set-fetcher
   newtype
   (el-get--get-fetcher oldtype)))

(defun el-get-register-virtual-fetcher (type filter)
  (el-get--set-fetcher
   type
   (el-get--plist-to-hash
    (list :type type
          :filter filter))))
(put 'el-get-register-virtual-fetcher 'lisp-indent-function 1))

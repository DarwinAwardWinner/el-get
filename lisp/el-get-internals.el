(defun el-get--print-to-string (object &optional pretty)
  "Return string representation of lisp object.

Unlike the Emacs builtin printing functions, this ignores
`print-level' and `print-length', ensuring that as much as
possible the returned string will be a complete representation of
the original object."
  (let (print-level print-length)
    (funcall (if pretty #'pp-to-string #'prin1-to-string)
             object)))

(fset 'el-get-display-warning (apply-partially #'display-warning 'el-get))
(put 'el-get-display-warning 'function-documentation
     (concat "`display-warning' with TYPE set to el-get

Original documentation for `display-warning' appears below:

------------------------------------------------------------------------

(display-warning TYPE MESSAGE &optional LEVEL BUFFER-NAME)

"
             (documentation 'display-warning)))

(defun el-get-verbose-message (format &rest arguments)
  (when el-get-verbose (apply 'message format arguments)))


(defun el-get--plist-to-hash (plist &rest make-hash-table-args)
  "Convert PLIST to an equivalent hash table.

Additional arguments are passed to `make-hash-table'.
Alternatively, an existing hash table may be passed as a second
argument. A hash table passed this way will be cleared before
adding the values from PLIST.

Typically there is no reason to pass more than one argument to
this function."
  (let ((hash (if (hash-table-p (car make-hash-table-args))
                  (clrhash (car make-hash-table-args))
                (apply #'make-hash-table make-hash-table-args))))
    (loop for (k v) on plist by #'cddr
          do (puthash k v hash))
    hash))

(defun el-get--hash-to-plist (hash)
  "Convert HASH to an equivalent plist."
  (let (plist)
    (maphash (lambda (k v) (plist-put plist k v)) hash)))

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

(defun el-get-plist-get-nodef (plist prop)
  "Like `plist-get' but signals an error unless PLIST contains PROP."
  (or (plist-get plist prop)
      (error "Property list does not contain property %s" prop)))

(defun el-get--substitute-keywords (plist expr)
  "Replace all keywords in EXPR with their values from PLIST.

If a keyword in EXPR is missing from PLIST, it will be replaced
with nil."
  (cond
   ((keywordp expr)
    (plist-get plist expr))
   ((consp expr)
    (cons (el-get--substitute-keywords plist (car expr))
          (el-get--substitute-keywords plist (cdr expr))))
   (expr)))

(defmacro el-get-plist-bind (plist &rest body)
  "Eval BODY after replacing all keywords with their values in PLIST.

Effectively, all keywords in BODY become variables that are
looked up in PLIST. Keywords not present in PLIST are replaced by nil"
  (let ((body (cons 'progn body)))
    (el-get--substitute-keywords (eval plist) body)))
(put 'el-get-plist-bind 'lisp-indent-function 1)

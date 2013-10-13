(defun el-get-print-to-string (object &optional pretty)
  "Return string representation of lisp object.

Unlike the Emacs builtin printing functions, this ignores
`print-level' and `print-length', ensuring that as much as
possible the returned string will be a complete representation of
the original object."
  (let (print-level print-length)
    (funcall (if pretty #'pp-to-string #'prin1-to-string)
             object)))
;; If pretty is a literal, we can pre-decide which printing function
;; to use at compile-time.
(cl-define-compiler-macro el-get-print-to-string
    (&whole form object &optional pretty)
  (condition-case nil
      (let* ((pretty-is-literal-or-quoted
              (or
               ;; t & nil are literals
               (memq pretty '(t nil))
               ;; Any non-symbol atom (number string, etc.)
               (and (atom pretty)
                    (not (symbolp pretty)))
               ;; Quoted form
               (and (listp pretty)
                    (= (length pretty) 2)
                    (eq (car pretty) 'quote))))
             (print-func
              (when pretty-is-literal-or-quoted
                (if (eval pretty) #'pp-to-string #'prin1-to-string))))
        (if print-func
            `(let (print-level print-length)
               (,print-func object))
          form))
    ;; Fallback to no expansion on error
    (error form)))

(defun* el-get-print-to-string-with-verification
    (object &optional pretty (test #'equal))
  "Return string representation after verifying.

To verify, the result string is read in using `read' and tested
for equivalence to OBJECT using TEST. The default TEST is
`equal'."
  (condition-case nil
      (let* ((result-string (el-get-print-to-string object pretty))
             (read-result (read result-string)))
        (assert (funcall test object read-result))
        result-string)
    (error (error "Object failed to serialize to string: %S" object))))

(fset 'el-get-display-warning (apply-partially #'display-warning 'el-get))
(put 'el-get-display-warning 'function-documentation
     (concat "`display-warning' with TYPE set to `el-get'.

Original documentation for `display-warning' appears below:

------------------------------------------------------------------------

(display-warning TYPE MESSAGE &optional LEVEL BUFFER-NAME)

"
             (documentation 'display-warning)))

(defun el-get-debug-message (format-string &rest args)
  "Record a debug message related to el-get."
  (el-get-display-warning (format format-string args) :debug))

(defun el-get-message (format-string &rest args)
  "Display a message related to el-get.

This is the same as `message', but the message is automatically
prefixed by \"el-get: \"."
  (apply #'message (concat "el-get: " format-string) args))

;;
;; "Fuzzy" data structure handling
;;
;; In el-get-sources, single elements are often allowed instead of a
;; list, and strings and symbols are often interchangeable.
;; Presumably it's easier for users who don't use the customization
;; interface to write such structures as raw elisp.
;;
(defsubst el-get-as-string (symbol-or-string)
  "If SYMBOL-OR-STRING is already a string, return it.  Otherwise
convert it to a string and return that."
  (if (stringp symbol-or-string)
      symbol-or-string
    (symbol-name symbol-or-string)))

(defsubst el-get-as-symbol (string-or-symbol)
  "If STRING-OR-SYMBOL is already a symbol, return it.  Otherwise
convert it to a symbol and return that."
  (if (symbolp string-or-symbol)
      string-or-symbol
    (intern string-or-symbol)))

(defsubst el-get-as-list (element-or-list)
  "If ELEMENT-OR-LIST is already a list, return it.  Otherwise
returning a list that contains it (and only it)."
  (if (listp element-or-list) element-or-list
      (list element-or-list)))

(defun el-get-list-of-strings-p (obj)
  (or (and (consp obj)
           (stringp (car obj))
           (el-get-list-of-strings-p (cdr obj)))
      (null obj)))

;; Property list/hash table interconversion
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
    (maphash (lambda (k v) (plist-put plist k v)) hash)
    plist))

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
looked up in PLIST. Keywords not present in PLIST are replaced by
nil.

For example, the following returns 3:

    (el-get-plist-bind '(:a 1 :b 2) (+ :a :b))"
  (let ((body (cons 'progn body)))
    (el-get--substitute-keywords (eval plist) body)))
(put 'el-get-plist-bind 'lisp-indent-function 1)

;;; el-get-internals.el --- Low-level functions required by el-get

;; Copyright (C) 2013  Ryan C. Thompson

;; Author: Ryan C. Thompson <rct@thompsonclan.org>

;; LICENSE???

;;; Commentary:

;;

;;; Code:

(require 'cl)

(defsubst el-get-arg-is-literal-or-quoted (arg)
  "Return non-nil if ARG is a quoted form or a literal.

Specifically, returns nil if ARG is a symbol representing a
variable (i.e. any symbol other than `t', `nil', or a keyword, or a
function call (i.e. an unquoted list).)"
  ;; If an error is encountered, give up and assume it's not a
  ;; literal.
  (ignore-errors
    (or
     ;; t & nil are literals
     (memq arg '(t nil))
     ;; keywords are literals
     (keywordp arg)
     ;; Any non-symbol atom (number, string, hash table, etc.)
     (and (atom arg)
          (not (symbolp arg)))
     ;; Quoted form (i.e. `(quote ARG)' or `(function ARG)'
     (and (listp arg)
          (= (length arg) 2)
          (memq (car arg) '(quote function)))
     ;; Functions are literals, technically. They are guaranteed to be
     ;; non-nil.
     (functionp arg))))

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
      (let ((print-func
             (when (el-get-arg-is-literal-or-quoted pretty)
               (if (eval pretty) #'pp-to-string #'prin1-to-string))))
        (if print-func
            `(let (print-level print-length)
               (,print-func ,object))
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

(defsubst el-get-display-warning (message &optional level buffer-name)
  "Same as `display-warning' with TYPE set to `el-get'."
  (display-warning 'el-get message level buffer-name))

(defun el-get-debug-message (format-string &rest args)
  "Record a debug message related to el-get."
  (el-get-display-warning (apply #'format format-string args) :debug))

(defun el-get-message (format-string &rest args)
  "Display a message related to el-get.

This is the same as `message', but the message is automatically
prefixed by \"el-get: \"."
  (apply #'message (concat "el-get: " format-string) args))

;; Define `el-get-error' as an error symbol.
(put 'el-get-error 'error-conditions
     '(el-get-error error))
(put 'el-get-error 'error-message
     "el-get error")

(defun el-get-error (string &rest args)
  "Raise an error related to el-get.

This is the same as `error', but the error message is
automatically prefixed by \"el-get error: \".

Also, the raised error has an additional condition
`el-get-error'."
  ;; Definition copied from `error'.
  (while t
    (signal 'el-get-error (list (apply 'format string args)))))

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
(defun el-get-plist-to-hash (plist &rest make-hash-table-args)
  "Convert PLIST to an equivalent hash table.

Additional arguments are passed to `make-hash-table'.
Alternatively, an existing hash table may be passed as a second
argument. A hash table passed this way will be cleared before
adding the values from PLIST.

Typically there is no reason to pass more than one argument to
this function."
  (let ((hash
         (if (hash-table-p (car make-hash-table-args))
             ;; Use provided hash table
             (clrhash (car make-hash-table-args))
           ;; Remaining arguments are args to `make-hash-table'. Add
           ;; an appropriate `:size' based on plist's size if not
           ;; provided.
           (progn
             (unless (plist-get make-hash-table-args :size)
               (plist-put make-hash-table-args
                          :size (/ (length plist) 2)))
             (apply #'make-hash-table make-hash-table-args)))))
    (loop for (k v) on plist by #'cddr
          do (puthash k v hash))
    hash))

(defun el-get-hash-to-plist (hash)
  "Convert HASH to an equivalent plist."
  (let (plist)
    (maphash
     (lambda (k v)
       (setq plist (plist-put plist k v)))
     hash)
    plist))

(defun el-get-plist-get-nodef (plist prop)
  "Like `plist-get' but signals an error unless PLIST contains PROP."
  (or (plist-get plist prop)
      (error "Property list does not contain property %s" prop)))

(defun el-get-substitute-keywords (plist expr)
  "Replace all keywords in EXPR with their values from PLIST.

If a keyword in EXPR is missing from PLIST, it will be replaced
with nil. Quoted forms are not modified."
  (cond
   ((keywordp expr)
    (plist-get plist expr))
   ((and (consp expr)
         ;; Don't recurse into quoted forms
         (not (eq (car expr) 'quote)))
    (cons (el-get-substitute-keywords plist (car expr))
          (el-get-substitute-keywords plist (cdr expr))))
   (expr)))

(defmacro el-get-plist-bind (plist &rest body)
  "Eval BODY after replacing all keywords with their values in PLIST.

Effectively, all keywords in BODY become variables that are
looked up in PLIST. Keywords not present in PLIST are replaced by
nil. Quote forms are not modified, keywords may be protected from
substitution by quoting.

For example, the following returns 3:

    (el-get-plist-bind '(:a 1 :b 2) (+ :a :b))

Note that the lookups in PLIST happen before BODY is evaluated,
so if BODY modifies PLIST, those modifications will not be
reflected in the substituted values."
  (declare (indent 1))
  (let ((body (cons 'progn body)))
    (el-get-substitute-keywords (eval plist) body)))

(defun el-get-read-from-file (filename)
  "Read a single lisp form from FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (read (current-buffer))))

(defun el-get-clean-plist (plist)
  "Remove any duplicate properties from PLIST."
  (loop with result = nil
        with seen-props = nil
        for (prop val) on plist by #'cddr
        unless (memq prop seen-props)
        nconc (list prop val) into result
        and collect prop into seen-props
        finally return result))

(defun el-get-merge-plists (&rest plists)
  "Merge PLISTS into a single property list.

Properties from plists earlier in the argument list take
precedence over the same properties in later plists.

The returned value is passed through `el-get-clean-plist'."
  (el-get-clean-plist (apply #'append plists)))

(defsubst el-get-plist-keys (plist)
  "Return a list of all keys in PLIST.

Duplicates are removed."
  (cl-remove-duplicates
   (loop for (k _) on plist by #'cddr
         collect k)
   :test #'eq))

(defun* el-get-plists-equal (plist1 plist2 &optional (value-test #'equal))
  "Return non-nil if PLIST1 and PLIST2 have the same properties and values.

Keys are compared with `eq' since they are expected to be
keywords. Values are compared with VALUE-TEST, which defaults to
`equal'.

Note that this function makes no distinction between an unset
property and a property that is set to nil."
  (loop for key in (el-get-plist-keys (append plist1 plist2))
        unless (funcall value-test
                        (plist-get plist1 key)
                        (plist-get plist2 key))
        return nil
        finally return t))

(defun el-get-random-hash-key (hash-table)
  "Return an arbitrary key from HASH-TABLE.

Specifically, the key returned is the first key iterated over by
`maphash'.

Note that this returns nil if HASH-TABLE is empty, but it might
also return nil if nil is a key in HASH-TABLE, so if nil is a
valid key, you can use `hash-table-count' to determine whether
the hash actually contains any elements."
  (cl-block GETKEY
    (maphash (lambda (k v) (cl-return-from GETKEY k)) hash-table)))

(defun el-get-bindable-symbol-p (object)
  "Like `symbolp' but excludes literal symbols.

Specifically, t, nil, and keywords are excluded."
  (when (symbolp object)
    ;; Try to bind the symbol using `let' and then retrieve its
    ;; value. If this fails, the symbol is not bindable.
    (ignore-errors
      (eval
       `(let ((,object t))
          ,object)))))

(defun el-get-validate-plist
  (plist required-props &optional optional-props allow-extra)
  "Validate the values of property list PLIST.

Returns nil for a successful validation or a list of errors if
validation fails.

REQUIRED-PROPS and OPTIONAL-PROPS are each property lists with
values being predicates that the corresponding properties must
satisfy. The propertiesof PLIST listed in REQUIRED-PROPS must
satisfy their associated predicates, while the properties in
OPTIONAL-PROPS must either satisfy their associated predicates or
be nil (or absent). In addition, if any properties in PLIST are
not listed in either REQUITED-PROPS or OPTIONAL-PROPS, an error
is raised, unless ALLOW-EXTRA is non-nil.

Due to the implementation of this function, multiple predicates
can be provided with the same property, e.g. `(:prop
#'pred1 :prop #'pred2)', in which case PLIST's value of `:prop'
will have to satisfy both `pred1' and `pred2'."
  (declare (indent defun))
  ;; Allow prop specs to be passed as hash tables as well as plists
  ;; (mostly for the benefit of `el-get-validate-hash-table').
  (when (hash-table-p required-props)
    (setq required-props (el-get-hash-to-plist required-props)))
  (when (hash-table-p optional-props)
    (setq optional-props (el-get-hash-to-plist optional-props)))
  (let ((errors nil))
    (cl-loop for (prop pred) on required-props by #'cddr
             for value = (plist-get plist prop)
             unless (funcall pred value)
             do (push (format "Value of required property %s fails predicate %s"
                              prop pred)
                      errors))
    (cl-loop for (prop pred) on optional-props by #'cddr
             for value = (plist-get plist prop)
             unless (or (null value) (funcall pred value))
             do (push (format "Value of optional property %s fails predicate %s"
                               prop pred)
                      errors))
    (let ((extra-props
           (cl-set-difference
            (el-get-plist-keys plist)
            (nconc (el-get-plist-keys required-props)
                   (el-get-plist-keys optional-props)))))
      (when extra-props
        (push "Extra properties provided but not allowed: %S" extra-props)))
    ;; If no errors, this will be nil
    (nreverse errors)))

(defsubst el-get-validate-hash-table
  (table required-keys optional-keys &optional allow-extra)
  "Same as `el-get-validate-plist' but for hash tables.

REQUIRED-KEYS and OPTIONAL-KEYS may be passed as either hash
tables or property lists."
  (el-get-validate-plist (el-get-hash-to-plist table)
                         required-keys optional-keys allow-extra))

(defun* el-get-make-lookup-table
    (objects &key key-func (value-func #'identity) init-hash allow-nil-key)
  "Make a lookup table for list OBJECTS.

For each object in OBJECTS, a key is computed using KEY-FUNC and
a value is computed using VALUE-FUNC (default value is the object
itself), and that value is associated with that key in the
returned hash table.

Optional keyword arg INIT-HASH, if provided, will be used as the
hash table into which the key-value pairs will be inserted. If
not provided, a new empty hash table will be used.

If keyword arg ALLOW-NIL-KEY is nil (the default), then values
for which KEY-FUNC returns nil will not be inserted at
all. Otherwise, they will be inserted as normal, with a key of
nil."
  (declare (indent 1))
  (loop with table = (or init-hash
                         (make-hash-table :size (length objects)))
        for obj in objects
        for key = (funcall key-func obj)
        for val = (funcall value-func obj)
        do (when (or key allow-nil-key)
             (puthash key obj table))
        finally return table))

(defun el-get-nonempty-string-p (str)
  "Return STR if it is a string of length 1 or more, nil otherwise."
  (and (stringp str)
       (> (length str) 0)
       str))

(defun el-get-ensure-suffix (str suffix)
  "If STR does not end in SUFFIX, append it."
  (if (string-match-p
       (concat (regexp-quote suffix) "\\'")
       str)
      str
    (concat str suffix)))

(provide 'el-get-internals)
;;; el-get-internals.el ends here

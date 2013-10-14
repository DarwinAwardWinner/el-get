;;; el-get-internals.el --- Low-level functions required by el-get

;; Copyright (C) 2013  Ryan C. Thompson

;; Author: Ryan C. Thompson <rct@thompsonclan.org>

;; LICENSE???

;;; Commentary:

;;

;;; Code:

(require 'cl)

(defconst el-get-base-directory
  ;; This should give the right path whether this file is being
  ;; loaded, or this form is being evalled via e.g. C-x C-e.
  (expand-file-name
   ".."
   (file-name-directory
    (or load-file-name
        (locate-library "el-get-internals")
        (when (string-match-p "el-get-internals.el\\'"
                              (buffer-file-name))
          (buffer-file-name))
        (error "Cannot determine path to el-get."))))
  "Base directory of el-get installation.")

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

(defsubst el-get-display-warning (message &optional level buffer-name)
  "Same as `display-warning' with TYPE set to `el-get'."
  (display-warning 'el-get message level buffer-name))

(defun el-get-debug-message (format-string &rest args)
  "Record a debug message related to el-get."
  (el-get-display-warning (format format-string args) :debug))

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
    (signal 'el-get-error  (apply 'format args))))

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

    (el-get-plist-bind '(:a 1 :b 2) (+ :a :b))

Note that the lookups in PLIST happen before BODY is evaluated,
so if BODY modifies PLIST, those modifications will not be
reflected in the substituted values."
  (let ((body (cons 'progn body)))
    (el-get--substitute-keywords (eval plist) body)))
(put 'el-get-plist-bind 'lisp-indent-function 1)

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
precedence over the same properties in later plists."
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

(provide 'el-get-internals)
;;; el-get-internals.el ends here

(defun el-get-print-to-string (object &optional pretty)
  "Return string representation of lisp object.

Unlike the Emacs builtin printing functions, this ignores
`print-level' and `print-length', ensuring that as much as
possible the returned string will be a complete representation of
the original object."
  (let (print-level print-length)
    (funcall (if pretty #'pp-to-string #'prin1-to-string)
             object)))

(defun el-get-verbose-message (format &rest arguments)
  (when el-get-verbose (apply 'message (concat "el-get: " format) arguments)))

;;
;; "Fuzzy" data structure handling
;;
;; In el-get-sources, single elements are often allowed instead of a
;; list, and strings and symbols are often interchangeable.
;; Presumably it's easier for users who don't use the customization
;; interface to write such structures as raw elisp.
;;
;;;  "Fuzzy" data structure conversion utilities
(defun el-get-as-string (symbol-or-string)
  "If STRING-OR-SYMBOL is already a string, return it.  Otherwise
convert it to a string and return that."
  (if (stringp symbol-or-string) symbol-or-string
    (symbol-name symbol-or-string)))

(defun el-get-as-symbol (string-or-symbol)
  "If STRING-OR-SYMBOL is already a symbol, return it.  Otherwise
convert it to a symbol and return that."
  (if (symbolp string-or-symbol) string-or-symbol
      (intern string-or-symbol)))

(defun el-get-as-list (element-or-list)
  "If ELEMENT-OR-LIST is already a list, return it.  Otherwise
returning a list that contains it (and only it)."
  (if (listp element-or-list) element-or-list
      (list element-or-list)))

(defun el-get-list-of-strings-p (obj)
  (or (null obj)
      (and (consp obj)
           (stringp (car obj))
           (el-get-list-of-strings-p (cdr obj)))))

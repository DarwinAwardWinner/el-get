(require 'el-get-fetcher-registry)

(defsubst el-get-recipe-autoget (recipe prop)
  "Get auto-generated value of PROP for RECIPE.

TODO More info"
  ;; :name and :type may be auto-generated
  (unless (memq prop '(:name :type))
    (funcall (el-get-fetcher-op recipe :auto-property) recipe prop)))
;; Optimize to nil for explicit-only properties
(cl-define-compiler-macro el-get-recipe-autoget (&whole form recipe prop)
  (if (memq prop '(:name :type))
      nil
    form))

(defsubst el-get-recipe-get (recipe prop)
  "Like `plist-get' but includes auto-generated recipe properties."
  (or
   (plist-get recipe prop)
   (el-get-recipe-autoget recipe prop)))
;; Optimize to just `plist-get' for explicit-only properties
(cl-define-compiler-macro el-get-recipe-get (&whole form recipe prop)
  (if (memq prop '(:name :type))
      `(plist-get ,recipe ,prop)
    form))

(defsubst el-get-recipe-name (recipe)
  "TODO DOC"
  (el-get-plist-get recipe :name))

(defsubst el-get-recipe-type (recipe)
  "TODO DOC"
  (el-get-plist-get recipe :type))

(defun el-get-validate-recipe (recipe &optional noerror)
  "Checks if recipe looks valid.

If the recipe is valid, this returns nil. If the recipe is
invalid, this throws an error, unless NOERROR is non-nil, in
which case it simply returns the list of validation errors
encountered."
  (let ((errors nil))
    (condition-case nil
        (progn
          ;; Type-independent validation
          (unless (el-get-recipe-name recipe)
            (add-to-list 'errors "Recipe has no name" 'append))
          (unless (el-get-recipe-type recipe)
            (add-to-list 'errors "Recipe has no type" 'append))
          ;; Type-specific validation, only if we passed the above
          (unless errors
            (setq errors
                  (el-get--as-list
                   (funcall (or (el-get-fetcher-op recipe :validate)
                                #'ignore)
                            recipe)))))
      ;; If the above code actually throws any errors, record that in
      ;; the error list.
      (error (add-to-list 'errors
                          "Encountered a lisp error while validating recipe"
                          'append)))
    (when errors
      (if noerror
          ;; Just return the error list
          errors
        ;; Raise an error
        (error "Errors encountered while validating recipe:\n%s"
               (mapconcat #'identity errors "\n"))))))

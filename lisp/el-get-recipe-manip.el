;;; el-get-recipe-manip.el --- Functions for manipulating recipe forms

;; Copyright (C) 2013  Ryan C. Thompson

;; Author: Ryan C. Thompson <rct@thompsonclan.org>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl)
(require 'el-get-internals)

;; Provide is up here to break recursive require
(provide 'el-get-recipe-manip)
(require 'el-get-fetcher-registry)

(defsubst el-get-recipe-autoget (recipe prop)
  "Get auto-generated value of PROP for RECIPE.

Some recipe types provide auto-generated property values for some
properties, which are used if a recipe does not explicitly
specify those properties. For example, file-type recipes will
auto-generate `:file-name' and `:website' properties.

Note that the `:name' and `:type' properties can never have
auto-generated values."
  ;; :name and :type may be auto-generated
  (unless (or (memq prop '(:name :type))
              (null (el-get-fetcher-prop recipe :auto-property)))
    (funcall (el-get-fetcher-prop recipe :auto-property) recipe prop)))
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

(defalias 'el-get-recipe-put 'plist-put
  "Set a property of a recipe.

This is equivalent to `plist-put', since recipes are implemented
as property lists.")

(defsubst el-get-recipe-name (recipe)
  "Return the `:name' property of RECIPE."
  (el-get-recipe-get recipe :name))

(defsubst el-get-recipe-type (recipe)
  "Return the `:type' property of RECIPE."
  (el-get-recipe-get recipe :type))

(defsubst el-get-recipe-auto-generated-p (recipe)
  "Returns non-nil if RECIPE was auto-generated."
  (el-get-recipe-get recipe :auto-generated))

(defun el-get-validate-recipe-properties
  (recipe required-props &optional optional-props)
  "Validate the values of properties in RECIPE.

Returns nil for a successful validation or a list of errors if
validation fails.

This is equivalent to `el-get-validate-plist' except that it can
also validate automatically-generated recipe properties, and
ALLOW-EXTRA is always true since recipes always allow arbitrary
additional properties."
  (declare (indent defun))
  (el-get-validate-collection recipe #'ignore #'el-get-recipe-get
    required-props optional-props t))

(defun el-get-validate-recipe (recipe &optional noerror expected-name)
  "Checks if recipe looks valid.

If the recipe is valid, this returns nil. If the recipe is
invalid, this throws an error, unless NOERROR is non-nil, in
which case it simply returns the list of validation errors
encountered.

If optional arg EXPECTED-NAME is provided, then the recipe must
have that name in order to validate."
  (let ((errors nil))
    (condition-case err
        (progn
          ;; Type-independent validation
          (setq errors
                (el-get-validate-recipe-properties recipe
                  `(:name #'el-get-bindable-symbol-p
                    :name
                    ,(if expected-name
                         (lambda (name)
                           (string= (el-get-as-string expected-name)
                                    (el-get-as-string name)))
                       #'identity)
                    :type #'el-get-bindable-symbol-p
                    :compile
                    ,(el-get-combine-predicates #'or
                       #'null
                       (lambda (val) (memq val '(auto none all)))
                       #'stringp
                       #'el-get-list-of-strings-or-nils-p)
                    :autoloads
                    ,(el-get-combine-predicates #'or
                       #'stringp
                       (lambda (val) (memq val '(nil t)))
                       #'el-get-list-of-strings-p))
                  nil))
          ;; Type-specific validation, only if we passed the above
          (unless errors
            (setq errors
                  (el-get-as-list
                   (funcall (or (el-get-fetcher-prop recipe :validate)
                                #'ignore)
                            recipe)))))
      ;; If the above code actually throws an error, record that in
      ;; the error list.
      (error (add-to-list
              'errors
              (format "Encountered a lisp error while validating recipe: %S" err)
              'append)))
    (when errors
      (if noerror
          ;; Just return the error list
          errors
        ;; Raise an error
        (error "Errors encountered while validating recipe:\n%s"
               (mapconcat #'identity errors "\n"))))))

(defsubst el-get-recipe-valid-p (recipe &optional expected-name)
  "Standard predicate form of `el-get-validate-recipe'.

Returns t for a valid recipe and nil otherwise. The list of
specific validation errors is discarded."
  (not (el-get-validate-recipe recipe 'noerror expected-name)))

(defun el-get-merge-partial-recipe
  (partial-recipe full-recipe &optional nocheck)
  "Like `el-get-merge-plists', but recipes are validated.

Before and after merging, FULL-RECIPE is validated using
`el-get-validate-recipe' unless NOCHECK is non-nil. If NOCHECK is
non-nil, this is exactly equivalent to (and compiles directly to)
`el-get-merge-plists'."
  ;; Verify that arguments are as expected
  (unless
      (and (not nocheck)
           (eq (el-get-recipe-name partial-recipe)
               (el-get-recipe-name full-recipe))
           (null (el-get-recipe-type partial-recipe))
           (null (el-get-validate-recipe full-recipe 'noerror)))
    (el-get-error "Need one partial recipe and one full recipe."))
  (let ((result (el-get-merge-plists partial-recipe full-recipe)))
    (if (or nocheck
            (null (el-get-validate-recipe result)))
        result
      (el-get-error "Merged recipe is invalid: %S" result))))
;; Skip the check at compile time if `nocheck' is a non-nil literal
(cl-define-compiler-macro el-get-merge-partial-recipe
    (&whole form partial-recipe full-recipe &optional nocheck)
  (condition-case nil
      (if (and (el-get-arg-is-literal-or-quoted nocheck)
               (eval nocheck))
          `(el-get-merge-plists ,partial-recipe ,full-recipe)
        form)
    (error form)))

(defun el-get-apply-recipe-filter (recipe filter)
  "Apply FILTER to RECIPE and return the result.

This assumes that appropriate checking has been done to ensure
that FILTER is the correct filter for RECIPE's type.

RECIPE may also be modified by side effect of this function.

After filtering, if the result does not have an `:orig-recipe'
property, this property is set to a copy of RECIPE.

If FILTER does not alter RECIPE, raise an error."
  (el-get-debug-message "Filtering recipe: %S" recipe)
  (el-get-debug-message "Pretty recipe:\n%s" (el-get-print-to-string recipe t))
  (let ((orig-recipe (copy-list recipe))
        (result (funcall filter recipe)))
    (when (el-get-plists-equal orig-recipe result)
      (el-get-error "Recipe was not altered by filter:\n%s\n%s"
                    (el-get-print-to-string orig-recipe 'pretty)
                    (el-get-print-to-string result 'pretty)))
    ;; Store the original recipe as a property in the new recipe
    (when (not (plist-get result :orig-recipe))
      (plist-put result :orig-recipe orig-recipe))
    result))

(defun* el-get-devirtualize-recipe-def (recipe &key (recursive t) (validate t))
  "If RECIPE is of a virtual type, filter it until it is real.

If keyword argument RECURSIVE is set to nil (default is t), only
one round of filtering will be carried out.

If keyword argument VALIDATE is set to nil (default is t), the
recipe will not be validated after each filtering step.

If RECIPE is already of a real type, is is returned unchanged."
  (loop while (el-get-fetcher-virtual-p recipe)
        do (setq recipe
                 (el-get-apply-recipe-filter
                  recipe
                  (el-get-fetcher-prop recipe :filter)))
        if validate
        do (el-get-validate-recipe recipe)
        if (not recursive) return recipe
        finally return recipe))

(defsubst el-get-make-recipe-override-table (recipes)
  "Convert a recipe list into an override table.

An override table is a hash table mapping recipe names to full or
partial recipe definitions.

If a hash table is passed to this function, it is returned
unchanged."
  (cond
   ((hash-table-p recipes)
    recipes)
   ((listp recipes)
    (el-get-make-lookup-table overrides
            :key-func (lambda (r) (el-get-recipe-name r))))
   (t
    (el-get-error "Not a recipe list: %S" recipes))))

(defun el-get-substitute-recipe-keywords (recipe expr)
  "Replace all keywords in EXPR with their values from RECIPE.

Note that auto-generated values are used too.

If a keyword in EXPR is missing from RECIPE, it will be replaced
with nil. Quoted forms are not modified."
  (cond
   ((keywordp expr)
    (el-get-recipe-get recipe expr))
   ((and (consp expr)
         ;; Don't recurse into quoted forms
         (not (eq (car expr) 'quote)))
    (cons (el-get-substitute-keywords recipe (car expr))
          (el-get-substitute-keywords recipe (cdr expr))))
   (expr)))

(defmacro el-get-recipe-bind (recipe &rest body)
  "Eval BODY after replacing all keywords with their values in RECIPE.

This is identical to `el-get-plist-bind' except that
automatically generated recipe properties are also included."
  (declare (indent 1))
  (let ((body (cons 'progn body)))
    (el-get-substitute-recipe-keywords (eval recipe) body)))

;; TODO: Recipe documentation set/get

(provide 'el-get-recipe-manip)
;;; el-get-recipe-manip.el ends here

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

(defalias 'el-get-recipe-put 'plist-put
  "Set a property of a recipe.

This is equivalent to `plist-put', since recipes are implemented
as property lists.")

(defsubst el-get-recipe-name (recipe)
  "TODO DOC"
  (el-get-recipe-get recipe :name))

(defsubst el-get-recipe-type (recipe)
  "TODO DOC"
  (el-get-recipe-get recipe :type))

(defsubst el-get-recipe-auto-generated-p (recipe)
  "Returns non-nil if RECIPE was auto-generated.

TODO link to additional doc"
  (el-get-recipe-get recipe :auto-generated))

(defun el-get-validate-recipe (recipe &optional noerror expected-name)
  "Checks if recipe looks valid.

If the recipe is valid, this returns nil. If the recipe is
invalid, this throws an error, unless NOERROR is non-nil, in
which case it simply returns the list of validation errors
encountered.

If optional arg EXPECTED-NAME is provided, then the recipe must
have that name in order to validate.

"
  (let ((errors nil))
    (condition-case nil
        (progn
          ;; Type-independent validation
          (unless (el-get-recipe-name recipe)
            (add-to-list 'errors "Recipe has no name" 'append))
          (unless (eq (symbol-name (el-get-recipe-name recipe))
                      (el-get-as-symbol expected-name))
            (add-to-list 'errors
                         (format "Recipe has name %s but expected name was %s"
                                 (el-get-recipe-name recipe) expected-name)
                         'append))
          (unless (el-get-recipe-type recipe)
            (add-to-list 'errors "Recipe has no type" 'append))
          ;; Type-specific validation, only if we passed the above
          (unless errors
            (setq errors
                  (el-get-as-list
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

(defun el-get-merge-partial-recipe
  (partial-recipe full-recipe &optional nocheck)
  "TODO DOC"
  ;; Verify that arguments are as expected
  (unless
      (and (not nocheck)
           (eq (el-get-recipe-name partial-recipe)
               (el-get-recipe-name full-recipe))
           (null (el-get-recipe-type partial-recipe))
           (null (el-get-validate-recipe full-recipe 'noerror)))
    (error "Need one partial recipe and one full recipe."))
  (el-get-merge-plists partial-recipe full-recipe))
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

After filtering, if the result does not have an `:orig-recipe'
property, this property is set to RECIPE.

If FILTER does not alter RECIPE, raise an error."
  (let ((result (funcall filter recipe)))
    (when (el-get-plists-equal recipe result)
      (el-get-error "Recipe was not altered by filter:\n%s"
                    (el-get-print-to-string recipe 'pretty)))
    (when (not (plist-get result :orig-recipe))
      (plist-put result :orig-recipe recipe))
    result))

(defun el-get-devirtualize-recipe-def (recipe &optional non-recursive)
  "If RECIPE is of a virtual type, filter it until it is real.

With optional arg NON-RECURSIVE, only do one round of filter
expansion. If RECIPE is already of a real type, return it unchanged."
  (when
      (when (el-get-fetcher-virtual-p recipe)
        (setq recipe
              (el-get-apply-recipe-filter
               recipe
               (el-get-fetcher-op recipe :filter))))
    ;; This only executes if the above
    (when (not non-recursive)
      (while (el-get-fetcher-virtual-p recipe)
        (setq recipe
              (el-get-apply-recipe-filter
               recipe
               (el-get-fetcher-op recipe :filter))))))
  recipe)

(provide 'el-get-recipe-manip)
;;; el-get-recipe-manip.el ends here

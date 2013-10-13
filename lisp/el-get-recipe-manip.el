;;; el-get-recipe-manip.el --- Functions for manipulating recipe forms

;; Copyright (C) 2013  Ryan C. Thompson

;; Author: Ryan C. Thompson(require 'el-get-fetcher-registry) <rct@thompsonclan.org>
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

(defsubst el-get-recipe-name (recipe)
  "TODO DOC"
  (el-get-recipe-get recipe :name))

(defsubst el-get-recipe-type (recipe)
  "TODO DOC"
  (el-get-recipe-get recipe :type))

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

(provide 'el-get-recipe-manip)
;;; el-get-recipe-manip.el ends here

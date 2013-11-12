;;; el-get-recipe-io.el --- Reading & writing recipes to/from files

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
(require 'el-get-recipe-manip)

(defconst el-get-builtin-recipe-dir
  (expand-file-name "builtin-recipes" el-get-base-directory)
  "Directory where el-get stores its built-in recipes.")

(defconst el-get-elpa-recipe-dir
  (expand-file-name "auto-elpa-recipes" el-get-base-directory)
  "Directory where el-get stores its auto-generated ELPA recipes.")

(defconst el-get-emacswiki-recipe-dir
  (expand-file-name "auto-emacswiki-recipes" el-get-base-directory)
  "Directory where el-get stores its auto-generated Emacswiki recipes.")

(defcustom el-get-recipe-path
  (list el-get-builtin-recipe-dir
        el-get-elpa-recipe-dir
        el-get-emacswiki-recipe-dir)
  "List of directories where el-get will search for recipes.

TODO DOC Explain rules for conflicting recipes"
  :group 'el-get
  :type '(repeat directory))

(defun el-get-read-recipe-file (filename)
  "Read and return the recipe in FILENAME, after validating it."
  (let ((expected-name
         (file-name-sans-extension (file-name-nondirectory filename)))
        (recipe (el-get-read-from-file filename)))
    (el-get-validate-recipe recipe nil expected-name)
    recipe))

(defun el-get-write-recipe-file (recipe filename)
  "Write RECIPE to FILENAME."
  (let ((expected-name
         (file-name-sans-extension (file-name-nondirectory filename))))
    (el-get-validate-recipe recipe nil expected-name)
    (with-temp-buffer
      (insert (format ";; Recipe for %s -*- mode: emacs-lisp -*-\n"
                      (file-name-nondirectory filename))
              (el-get-print-to-string recipe 'pretty))
      ;; We don't use `el-get-write-file' here because if the recipe
      ;; gets corrupted, we do want the backup file to exist.
      (write-file filename))))

(defun el-get-read-recipe (name)
  "Find and read recipe NAME from dirs in `el-get-recipe-path'.

Returns first non-auto recipe found, or else first auto recipe
found. TODO DOC"
  (loop
   with auto-recipe = nil
   with next-recipe = nil
   for dir in el-get-recipe-path
   for rfile = (expand-file-name (concat (el-get-as-string name) ".rcp")
                                 dir)
   do (el-get-debug-message "Checking for recipe %s in file %s" name rfile)
   do (setq
       next-recipe
       (when (file-exists-p rfile)
         (el-get-read-recipe-file rfile)))
   if next-recipe
   if (el-get-recipe-auto-generated-p next-recipe)
   do (unless auto-recipe (setq auto-recipe next-recipe))
   else return next-recipe
   finally return (or auto-recipe
                      (el-get-error "Could not find a recipe named %s" name))))

(defun* el-get-resolve-recipe (recipe &key overrides devirtualize)
  "Return full definition for RECIPE.

RECIPE can be a recipe name, a partial (typeless) recipe
definition, or a complete recipe definition (with a type). A
complete recipe is returned unmodified, while for other types the
complete recipe is read from the appropriate file (see
`el-get-recipe-path') and merged into RECIPE.

If keyword arg OVERRIDES is provided, it must be a hash table of
with keys being recipe names and values being partial or full
recipe definitions that should be used to override the
definitions in `el-get-recipe-path' for those recipes. OVERRIDES
may also be a list of just the recipe overrides, from which the
hash table will be generated, but this is less efficient.

With keyword arg DEVIRTUALIZE, call
`el-get-devirtualize-recipe-def' on the result before returning
it."
  (declare (indent 1))
  (let* (;; Convert OVERRIDES to a hash table if necessary
         (overrides
          (el-get-make-recipe-override-table overrides))
         (resolved-recipe
          (cond
           ((stringp recipe)
            ;; Recipe name as string; convert to symbol and recurse
            (el-get-resolve-recipe (el-get-as-symbol recipe)
              :overrides overrides
              :devirtualize devirtualize))
           ((symbolp recipe)
            ;; Just a recipe name; lookup in overrides or else convert
            ;; to minimal list '(:name NAME) and re-call
            (el-get-resolve-recipe
                (gethash recipe overrides `(:name ,recipe))
              :overrides overrides
              :devirtualize devirtualize))
           ((and (listp recipe) (el-get-recipe-get recipe :type))
            ;; Full recipe; return as as
            recipe)
           ((listp recipe)
            (progn
              ;; Partial recipe (no :type); merge from overrides and
              ;; then merge from file if needed
              (setq recipe
                    (el-get-merge-plists
                     recipe
                     (gethash (el-get-recipe-name recipe)
                              overrides)))
              ;; If override didn't provide a type, we still need to
              ;; read from disk
              (if (el-get-recipe-get recipe :type)
                  recipe
                (el-get-merge-plists
                 recipe
                 (el-get-read-recipe (el-get-recipe-name recipe))))))
           ;; Anything other than a symbol or a plist is an error
           (t
            (el-get-error "Invalid recipe definition: %S" recipe)))))
    (el-get-debug-message "Resolved recipe: %S" resolved-recipe)
    (if devirtualize
        (el-get-devirtualize-recipe-def resolved-recipe)
      resolved-recipe)))

(provide 'el-get-recipe-io)
;;; el-get-recipe-io.el ends here

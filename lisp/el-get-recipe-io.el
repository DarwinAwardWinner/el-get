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
      (insert (format ";; %s\n" (file-name-nondirectory filename))
              ";; -*- mode: emacs-lisp -*-\n"
              (el-get-print-to-string recipe 'pretty))
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
         (condition-case nil
             (el-get-read-recipe-file rfile)
           (error nil))))
   if next-recipe
   if (el-get-recipe-auto-generated-p next-recipe)
   do (unless auto-recipe (setq auto-recipe next-recipe))
   else return next-recipe
   finally return (or auto-recipe
                      (error "Could not find a recipe named %s" name))))

(defun el-get-resolve-recipe (recipe &optional devirtualize)
  "Return full definition for RECIPE.

RECIPE can be a recipe name, a partial (typeless) recipe
definition, or a complete recipe definition (with a type). A
complete recipe is returned unmodified, while for other types the
complete recipe is read from the appropriate file.

With optional arg DEVIRTUALIZE, call
`el-get-devirtualize-recipe-def' on the result before returning
it."
  (let ((recipe
         (if (listp recipe)
             ;; Recipe definition, possibly partial
             (if (el-get-recipe-get recipe :type)
                 ;; Complete recipe def; return as is
                 recipe
               ;; Partial recipe def; read full def from file and merge
               (el-get-merge-plists
                recipe
                (el-get-read-recipe (el-get-recipe-name recipe))))
           ;; Just a recipe name
           (el-get-read-recipe recipe))))
    (el-get-debug-message "Resolved recipe: %S" recipe)
    (if devirtualize
        (el-get-devirtualize-recipe-def recipe)
      recipe)))

(provide 'el-get-recipe-io)
;;; el-get-recipe-io.el ends here

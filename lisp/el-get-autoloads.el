;;; el-get-autoloads.el --- Facilities for generating autoloads for el-get packages

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

(require 'autoload)
(require 'el-get-internals)
(require 'el-get-package-internals)

(defsubst el-get-package-autoload-file (package)
  "Return the path to the autoload file for PACKAGE."
  (el-get-expand-package-file-name "loaddefs.el" package))

(defun el-get-generate-package-autoloads (package)
  "Generate autoloads file for PACKAGE.

If a pacakge's `:autoloads' property is t, then autoloads will be
generated from all Emacs Lisp files in the package's
load-path. If it is nil, then no autoloads will be generated. If
it is a string or a list of strings, these will be taken as paths
to one or more pre-built autoload files and their contents will
be copied (possibly by symlinking) to the package's main autoload
file."
  (el-get-with-package-lock package
    (let* ((recipe (el-get-package-recipe package t))
           (install-dir (el-get-package-install-directory package))
           (autoload-prop (el-get-recipe-get recipe :autoloads))
           (lpath-prop (el-get-package-load-path package)))
      (cond
       ((null autoload-prop)
        (el-get-debug-message "Not generating autoloads for package %s"
                              package))
       ((eq autoload-prop t)
        ;; Generate autoloads from all elisp files in package's load-path
        (el-get-debug-message "Generating autoloads for package %s"
                              package)
        (let ((generated-autoload-file
               (el-get-package-autoload-file package)))
          (el-get-debug-message
           "Load path is %S"
           (loop for dir in lpath-prop
                 collect (expand-file-name dir install-dir)))
          (apply
           #'update-directory-autoloads
           (loop for dir in lpath-prop
                 collect (expand-file-name dir install-dir)))))
       ((stringp autoload-prop)
        ;; Copy or symlink named file to autoload file location
        (el-get-debug-message "Using pre-built autoload file for package %s: %s"
                              package autoload-prop)
        (el-get-link-or-copy
         (expand-file-name autoload-prop install-dir)
         (el-get-package-autoload-file package)))
       ((el-get-list-of-strings-p autoload-prop)
        ;; Concatenate listed files into autoload file
        (el-get-debug-message
         "Collecing pre-built autoload files for package %s: %S"
         package autoload-prop)
        (let ((generated-autoload-file (el-get-package-autoload-file package))
              (feature-name (format "%s-autoloads" package)))
         (with-temp-file generated-autoload-file
           ;; Set up the initial autoload rubric
           (insert (autoload-rubric (buffer-file-name) nil feature-name))
           (goto-char (point-min))
           (unless (search-forward "\f\n" nil 'noerror)
             (el-get-error
              "Failed to set up autoload rubric for package %s in %s"
              package generated-autoload-file))
           ;; Now insert each of the autoload files listed along with
           ;; some enapsulating comments, putting a formfeed in
           ;; between each one.
           (loop for path in autoload-prop
                 ;; This is necessary to make sure the file gets ".el"
                 ;; but not ".elc" appended as necessary
                 for abspath =
                 (let ((load-suffixes (remove ".elc" load-suffixes)))
                   (locate-library
                    (expand-file-name path install-dir)))
                 for relpath = (file-relative-name abspath install-dir)
                 do (insert
                     (format ";;;; Contents of el-get package file %s\n\n"
                             relpath))
                 do (insert-file-contents abspath)
                 ;; Ensure a newline after file contents
                 unless (looking-back "\n")
                 do (insert
                     (format
                      "\n;;;; End contents of el-get package file %s\n\f\n"
                      package))))))
       (t (el-get-error "Unrecognized :autoloads property: %S"
                        autoload-prop))))))

(provide 'el-get-autoloads)
;;; el-get-autoloads.el ends here

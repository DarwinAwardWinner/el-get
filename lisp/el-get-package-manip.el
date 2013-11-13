;;; el-get-package-manip.el --- Functions for manipulating packages

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

(require 'el-get-internals)
(require 'el-get-variables)
(require 'el-get-recipe-manip)
(require 'el-get-bytecomp)
(require 'el-get-package-internals)

(defun el-get-remove-package (package)
  "Uninstall PACKAGE.

This just removes PACKAGE's directory and all its contents.

PACKAGE may also be a recipe, in which case `el-get-recipe-name'
will be used to get name of the package to be removed."
  (when (listp package)
    (el-get-warning-message
     "Recipe passed to `el-get-remove' instead of package name: %S"
     package)
    (setq package (el-get-recipe-name package)))
  (el-get-debug-message "Removing package %s" package)
  (el-get-with-package-lock package
    ;; First write a removed status to the status file, so that if the
    ;; deletion is interrupted it can be resumed later.
    (el-get-write-status-plist package
      '(:status removed))
    (delete-directory (el-get-package-base-directory package) 'recursive)))

(defun el-get-fetch-package (recipe)
  "Fetch package described by RECIPE.

Upon success, this sets the package status to \"fetched\".

If successful, this function returns a symbol describing what it
did. Possible values are `fetched' or `skipped'."
  (el-get-warn-unless-in-subprocess 'el-get-fetch-package)
  (let ((package (el-get-recipe-name recipe)))
    (el-get-debug-message "Fetching package %s" package)
    (el-get-with-package-lock package
      (case (el-get-package-status package)
        (removed
         (el-get-do-fetch recipe)
         'fetched)
        ((fetched installed)
         (el-get-message "Package %s is already %s. Skipping fetch."
                         package (el-get-package-status package))
         'skipped)
        (t (el-get-error "Invalid status: %s"
                         (el-get-package-status package)))))))

(defun el-get-build-package (package)
  "Build PACKAGE."
  (el-get-warn-unless-in-subprocess 'el-get-build-package)
  (el-get-debug-message "Building package %s" package)
  (el-get-with-package-lock package
    (let* ((status (el-get-package-status package))
           (recipe (el-get-package-recipe package))
           (buildprop (el-get-normalize-build-property
                       (el-get-recipe-get recipe :build)))
           (install-dir (el-get-package-install-directory package)))
      ;; Check fetched status
      (case status
        (removed (el-get-error "Package %s must be fetched before building"
                               package))
        (installed (el-get-error "Package %s is already built"
                                 package))
        (fetched (ignore))
        (otherwise (el-ger-error "Invalid status: %S" status)))
      (el-get-do-build buildprop package)
      (el-get-byte-compile-package package)
      ;; Build autoloads
      ;; TODO!
      ;; Build info
      ;; TODO!
      )))

(provide 'el-get-package-manip)
;;; el-get-package-manip.el ends here

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

(require 'el-get-variables)
(require 'el-get-recipe-manip)
(require 'el-get-fetcher-registry)

(put ':status 'lisp-indent-function)

(defconst el-get-package-statuses
  '(removed installed fetched)
  "List of possible statuses a package can have.")

(defvar el-get-package-status-cache nil
  "Cache to speed up package status lookups.

It is only set while a package is locked, and if it is set, it is
set to `(cons PACKAGE STATUS-PLIST)', where PACKAGE is the name
of the package and STATUS-PLIST is the contents of the status
file for that package.")

(defun el-get-package-base-directory (package)
  "Return the base directory for PACKAGE.

Note that this just returns the path to the directory that will
be used for PACKAGE. It does not check whether that directory
exists.

See also `el-get-package-install-directory'."
  (let ((package (el-get-as-string package)))
    (unless (el-get-file-basename-p package)
      (el-get-error "Package name %S contains a directory separator" package))
    (expand-file-name package el-get-install-dir)))

(defsubst el-get-package-install-directory (package)
  "Return the path where PACKAGE's files will be installed.

This is always the \"pkg\" subdirectory of PACKAGE's base
directory."
  (expand-file-name "pkg" (el-get-package-base-directory package)))

(defsubst el-get-holding-package-lock (package)
  (el-get-holding-file-lock (el-get-package-base-directory package)))

(defmacro el-get-with-package-lock (package &rest body)
  "Execute BODY while holding the lock on PACKAGE.

PACKAGE should be a symbol naming a package. This actually locks
the package directory using `el-get-with-file-lock'.

Note that while this is a macro, PACKAGE is evaluated normally."
  (declare (indent 1))
  (let* ((package (eval package)))
    (if (el-get-holding-package-lock package)
        ;; We already have the lock, just execute BODY
        `(progn ,@body)
      ;; We need to acquire the lock, and release it when done
      `(let ((package-dir ,(el-get-package-base-directory package)))
         ;; Make sure the parent directory exists so we can create the
         ;; lock
         (el-get-ensure-directory (file-name-directory package-dir))
         (el-get-with-file-lock package-dir
           (unwind-protect (progn ,@body)
             ;; Clear the status cache before releasing the lock.
             (setq el-get-package-status-cache nil)))))))

(defsubst el-get-status-file (package)
  "Return the path to the status file for PACKAGE.

This file is not guaranteed to exist."
  (expand-file-name ".status" (el-get-package-base-directory package)))

(defsubst el-get-status-plist-valid-p (package plist)
  "Return non-nil if PLIST is a valid status plist for PACKAGE."
  (declare (indent 1))
  (case (plist-get plist :status)
    ;; If status is removed (or nil), we don't need a recipe
    (removed t)
    ;; If status is fetched or installed, the `:recipe' property must
    ;; be a valid recipe, and it must not be a virtual recipe.
    ((fetched installed)
     (and (el-get-recipe-valid-p (plist-get plist :recipe))
          (el-get-fetcher-real-p (plist-get plist :recipe))))
    (otherwise nil)))

(defun el-get-status-plist (package)
  "Read and return the list from status file for PACKAGE.

If the status file does not exist, or the contents appear
corrupted, returns nil. A valid status plist must include two
entries: `:status', whose value is an element of
`el-get-package-statuses', and `:recipe', which must be a valid
recipe."
  (if (eq package (car el-get-package-status-cache))
      ;; Use the cache
      (cdr el-get-package-status-cache)
    ;; Read the file
    (let ((plist
           (el-get-with-package-lock package
             (let ((sfile (el-get-status-file package)))
               (when (file-exists-p sfile)
                 (let ((plist (ignore-errors (el-get-read-from-file sfile))))
                   (when (el-get-status-plist-valid-p plist package)
                     plist)))))))
      ;; Set the cache if we are holding the package lock
      (when (el-get-holding-package-lock package)
        (setq el-get-package-status-cache
              (cons package plist)))
      plist)))

(defun el-get-write-status-plist (package plist)
  "Write PLIST to PACKAGE's status file.

PLIST is validated before attempting to write it."
  (declare (indent 1))
  (unless (el-get-status-plist-valid-p plist package)
    (el-get-error
     "Not a valid status plist for package %s:\n%S"
     package plist))
  ;; Invalidate the cache
  (setq el-get-package-status-cache nil)
  ;; Write the status
  (el-get-with-package-lock package
    (with-temp-buffer
      (insert (format ";; Status for %s -*- mode: emacs-lisp -*-\n"
                      package)
              (el-get-print-to-string plist 'pretty))
      (write-file (el-get-status-file package))))
  ;; Update the cache if we have the lock
  (when (el-get-holding-package-lock package)
    (setq el-get-package-status-cache
          (cons package plist))))

(defun el-get-package-status (package)
  "Return the status of PACKAGE.

The return value is an element of `el-get-package-statuses'. Note
that a package with an invalid or nonexistent status file (or
nonexistent package directory) is considered to have \"removed\"
status."
  (or (plist-get (el-get-status-plist package) :status)
      'removed))

(defun el-get-package-recipe (package)
  "Return the recipe used to fetch/install PACKAGE.

Returns nil if PACKAGE is not installed."
  (el-get-with-package-lock package
    (el-get-plist-bind (el-get-status-plist package)
      (when (memq :status '(installed fetched))
        :recipe))))

(defun el-get-remove-package (package)
  "Uninstall PACKAGE.

This just removes PACKAGE's directory and all its contents.

PACKAGE may also be a recipe, in which case `el-get-recipe-name'
will be used to get name of the package to be removed."
  (when (listp package)
    (setq package (el-get-recipe-name package)))
  (el-get-with-package-lock package
    ;; First write a removed status to the status file, so that if the
    ;; deletion is interrupted it can be resumed later.
    (el-get-write-status-plist package
      '(:status removed))))
    (delete-directory (el-get-package-base-directory package))))

(defun el-get-fetch-package (recipe)
  "Fetch package described by RECIPE.

Upon success, this sets the package status to \"fetched\".

If successful, this function returns a symbol describing what it
did. Possible values are `fetched' or `skipped'."
  (el-get-warn-unless-in-subprocess 'el-get-fetch-package)
  (let ((package (el-get-recipe-name recipe)))
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

(defun el-get-do-fetch (recipe)
  "Fetch package described by RECIPE unconditionally.

If the package directory already exists, its contents are deleted
and replaced with the fetched contents."
  (let ((package (el-get-recipe-name recipe))
        ;; We need to devirtualize the recipe now and store the
        ;; devirtualized version, so that if its devirtualized
        ;; representation changes later, the installed package
        ;; continues using the old representation that was used to
        ;; install it.
        (recipe (el-get-devirtualize-recipe-def recipe)))
    ;; Ensure the directory is empty
    (el-get-debug-message
     "Ensuring empty starting directory for package %s before fetching."
     package)
    (el-get-delete-directory-contents
     (el-get-package-base-directory package))
    (el-get-ensure-directory
     (el-get-package-install-directory package))
    ;; Fetch the package
    ;; TODO: Fetch in temp dir?
    (funcall (el-get-fetcher-op recipe :fetch)
             recipe (el-get-package-install-directory package))
    ;; Set status to fetched, and record the recipe
    (el-get-write-status-plist package
                               `(:status fetched
                                         :recipe ,recipe))))

(provide 'el-get-package-manip)
;;; el-get-package-manip.el ends here

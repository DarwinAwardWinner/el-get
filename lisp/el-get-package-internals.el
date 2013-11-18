;;; el-get-package-internals.el --- Low-level pacakge operation functions

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
(require 'el-get-variables)
(require 'el-get-recipe-manip)
(require 'el-get-dependencies)

(defconst el-get-package-statuses
  '(removed installed fetched)
  "List of possible statuses a package can have.")

(defvar el-get-package-status-cache nil
  "Cache to speed up package status lookups.

It is only set while a package is locked, and if it is set, it is
set to `(cons PACKAGE STATUS-PLIST)', where PACKAGE is the name
of the package and STATUS-PLIST is the contents of the status
file for that package.")

(defsubst el-get-package-base-directory (package)
  "Return the base directory for PACKAGE.

Note that this just returns the path to the directory that will
be used for PACKAGE. It does not check whether that directory
exists.

See also `el-get-package-install-directory'."
  (let ((package (el-get-as-string package)))
    (unless (el-get-file-basename-p package)
      (el-get-error "Package name %S contains a directory separator" package))
    (expand-file-name package el-get-install-dir)))

(defsubst el-get-expand-package-file-name (name package)
  "Expand NAME relative to base directory for PACKAGE."
  (expand-file-name
   name
   (el-get-package-base-directory package)))

(defsubst el-get-package-install-directory (package)
  "Return the path where PACKAGE's files will be installed.

This is always the \"pkg\" subdirectory of PACKAGE's base
directory."
  (el-get-expand-package-file-name "pkg" package))

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
  (el-get-expand-package-file-name ".status" package))

(defsubst el-get-status-plist-valid (plist package)
  "Return non-nil if PLIST is a valid status plist for PACKAGE."
  (declare (indent 1))
  (case (plist-get plist :status)
    ;; If status is removed (or nil), we don't need a recipe
    (removed
     t)
    ;; If status is fetched or installed, the `:recipe' property must
    ;; be a valid recipe, and it must not be a virtual recipe.
    ((fetched installed)
     (and (el-get-recipe-valid-p (plist-get plist :recipe) package)
          (el-get-fetcher-real-p (plist-get plist :recipe))))
    (otherwise
     (el-get-debug-message "Invalid status %s: %S"
                           (plist-get plist :status) plist)
     nil)))

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
                   (when (el-get-status-plist-valid plist package)
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
  (unless (el-get-status-plist-valid plist package)
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
      (el-get-write-file (el-get-status-file package))))
  ;; Update the cache if we have the lock
  (when (el-get-holding-package-lock package)
    (setq el-get-package-status-cache
          (cons package plist))))

(defsubst el-get-package-status (package)
  "Return the status of PACKAGE.

The return value is an element of `el-get-package-statuses'. Note
that a package with an invalid or nonexistent status file (or
nonexistent package directory) is considered to have \"removed\"
status."
  (or (plist-get (el-get-status-plist package) :status)
      'removed))

;; TODO Update all calls to use the second argument if needed
(defun el-get-package-recipe (package &optional must-exist)
  "Return the recipe used to fetch/install PACKAGE.

Returns nil if PACKAGE is not installed, unless optional second
arg is non-nil, in which case it throws an error."
  (el-get-with-package-lock package
    (el-get-plist-bind (el-get-status-plist package)
      (if (memq :status '(installed fetched))
          :recipe
        (when must-exist
          (el-get-error "Package %s has no recipe. Status plist was: %S"
                        package (el-get-status-plist package)))))))

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
    (el-get-with-package-lock package
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
      (funcall (el-get-fetcher-prop recipe :fetch)
               recipe (el-get-package-install-directory package))
      ;; Set status to fetched, and record the recipe
      (el-get-write-status-plist package
                                 `(:status fetched
                                   :recipe ,recipe)))))

(defun el-get-do-build (buildprop package)
  "Perform build instrutions BUILDPROP in install dir of PACKAGE.

BUILDPROP should already be normalized."
  (el-get-with-package-lock package
    (el-get-with-cd-to-dir (el-get-package-install-directory package)
      ;; TODO: Figure out where to send the output of commands/functions
      (if (functionp buildprop)
          (progn
            (el-get-debug-message
             "Calling build function for package %s: %S"
             package buildprop)
            (funcall buildprop))
        (loop
         for cmd in buildprop
         do (el-get-debug-message
             "Running build command for package %s: %S"
             package cmd)
         do (let ((exitcode
                   (apply #'call-process
                          (car cmd) nil nil nil
                          (cdr cmd))))
              (if (ignore-errors (= exitcode 0))
                  (el-get-debug-message
                   "Build command succeeded for package %s: %S"
                   package cmd)
                (el-get-error
                 "Build command for package %s failed with exit code %s: %S"
                 package exitcode cmd))))))))

(defsubst el-get-package-load-path (package)
  "Return the `:load-path' property of PACKAGE's recipe.

This always returns a list."
  (el-get-recipe-get-list
   (el-get-package-recipe package t)
   :load-path))

(defun el-get-package-dependencies (package)
  "Return dependencies for PACKAGE.

These are read from PACKAGE's status recipe, so this only works
on fetched or installed packages."
  (let ((recipe (el-get-package-recipe package)))
    (if recipe
        (el-get-recipe-dependencies recipe)
      (el-get-error "Package recipe not available for %s" package))))

(defun el-get-package-after-load-target (package)
  "Returns the file or feature against which to register `:after-load'."
  (let ((target (el-get-recipe-after-load-target
                 (el-get-package-recipe package))))
    (if (symbolp target)
        target
      (expand-file-name target
                        (el-get-package-install-directory package)))))

(provide 'el-get-package-internals)
;;; el-get-package-internals.el ends here

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

(require 'el-get-internals)
(require 'el-get-variables)
(require 'el-get-recipe-manip)

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

(defsubst el-get-status-plist-valid (plist package)
  "Return non-nil if PLIST is a valid status plist for PACKAGE."
  (declare (indent 1))
  (case (plist-get plist :status)
    ;; If status is removed (or nil), we don't need a recipe
    (removed
     (el-get-debug-message "Package is removed")
     t)
    ;; If status is fetched or installed, the `:recipe' property must
    ;; be a valid recipe, and it must not be a virtual recipe.
    ((fetched installed)
     (el-get-debug-message "Package is fetched or installed")
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

(defun el-get-normalize-build-command (cmd &optional package)
  "Normalize a build command CMD.

If CMD is a list of strings, return it. If it is a string, return
a list of strings representing the same command. For any other
value, an error is returned.

For a CMD that is a string, a warning is issued if it contains
any characters with special meaning to the shell (including
spaces). Such commands should be converted to the list-of-strings
form.

With optional argument PACKAGE, it will be included in any error
or warning messages."
  (cond
   ((stringp cmd)
    ;; Convert to `("sh" "-c" ,cmd) or equivalent
    (prog1 (list shell-file-name
                 shell-command-switch
                 cmd)
      (when (not (string= cmd (shell-quote-argument cmd)))
        (el-get-display-warning "Build command %S%s will be shell-interpolated. To bypass shell interpolation, the command should be written as a list of strings instead."
                                cmd (if package
                                        (concat " in package " package)
                                      "")))))
   ((listp cmd)
    (prog1 cmd
      (loop for s in cmd
            unless (stringp s)
            do (el-get-error "Build command%s contains non-string arguments: %S"
                             (if package
                                 (concat " for package " package)
                               "")
                             cmd))))
   (t (el-get-error "Invalid build command%s: %S"
                    (if package
                        (concat " for package " package)
                      "")
                    cmd))))

(defun el-get-normalize-build-property (buildprop &optional package)
  "Properly normalize a `:build' property of a recipe.

A recipe's `:build' property can be any one of the following:

* A zero-argument function, which will be called with
  `default-directory' set to the directory in which the package
  was fetched. This function should perform the build.

* A list of build commands, where each build command is a list of
  strings or a single string. The list-of-strings form is
  preferred because it will avoid shell interpolation.

In either case, BUILDPROP will be comma-interpolated as if it
were backquoted, unless it is prefixed with a normal quote. (The
backquote may also be included explicitly.)

Any other form will result in an error.

This function exists mainly for error-checking and converting
single-string build commands into list-of-string build commands.

With optional argument PACKAGE, it will be included in any error
messages."
  ;; Unquote or eval as needed
  (when (listp buildprop)
    (setq buildprop
          (case (car buildprop)
            (quote (cdr buildprop))
            (\` (eval buildprop))
            ;; No quote, so use backquote implicitly.
            (otherwise
             (eval (list '\` buildprop))))))
  (cond
   ;; Nil: return it
   ((null buildprop) nil)
   ;; Function: just return it
   ((functionp buildprop)
    buildprop)
   ;; List of commands: normalize them
   ((listp buildprop)
    (mapcar (lambda (bc) (el-get-normalize-build-command bc package))
            buildprop))
   ;; Anything else: invalid
   (t (el-get-error "Invalid `:build' property%s: %S"
                    (if package (concat " for package " package) "")
                    buildprop))))

(defun el-get-do-build (buildprop package)
  "Perform build instrutions BUILDPROP in install dir of PACKAGE.

BUILDPROP should already be normalized."
  (el-get-with-cd-to-dir (el-get-package-install-directory package)
    ;; TODO: Figure out where to send the output of commands/functions
    (if (functionp buildprop)
        (progn
          (el-get-debug-message "Calling build function for package %s: %S"
                                package buildprop)
          (funcall buildprop))
      (loop
       for cmd in buildprop
       do (el-get-message "Running build command for package %s: %S"
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
               package exitcode cmd)))))))

(provide 'el-get-package-internals)
;;; el-get-package-internals.el ends here

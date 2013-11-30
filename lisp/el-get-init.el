;;; el-get-init.el --- Package initialiation

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
(require 'el-get-async)
(require 'el-get-recipe-manip)
(require 'el-get-package-internals)
(require 'el-get-dependencies)

(defsubst el-get-package-init-file (package)
  (el-get-expand-package-file-name "init.el" package))

(defun el-get-package-init-form (package &optional force-load)
  "Return a lisp form to init PACKAGE.

Evalutaing the return value of this function should initialize
the package.

With optional arg FORCE-LOAD, the returned form will, if
possible, skip autoloads and simply `require' or `load' the
package's `:feature'/`:load' properties as appropriate. If the
package's recipe does not have either of these properties, a
warning will be issued and this will return the same value it
would have returned if FORCE-LOAD was nil.

This function should only be called on packages that are fully
built (other than generation of their init file), since it checks
for the existence of certain files in the package directory."
  (el-get-with-package-lock package
    (let* ((recipe (el-get-package-recipe package t))
           (install-dir (el-get-package-install-directory package))

           ;; Get the required properties from the recipe
           (package-deps (el-get-recipe-dependencies recipe))
           (package-load-path (el-get-package-load-path package))
           (package-load-path-abs
            (mapcar (lambda (f) (expand-file-name f install-dir))
                    package-load-path))
           (autoload-file (el-get-package-autoload-file package))
           (immediate-load (or force-load
                               (not (file-exists-p autoload-file))))
           (package-features (el-get-recipe-get-list recipe :features))
           (load-files (el-get-recipe-get-list recipe :load))
           (load-files-fullpath
            (mapcar (lambda (f) (expand-file-name f install-dir))
                    load-files))
           ;; Executed before setting up package
           (init-form
            (el-get-normalize-recipe-init-form
             (el-get-recipe-get recipe :init)))
           ;; Executed after setting up package
           (after-init-form
            (el-get-normalize-recipe-init-form
             (el-get-recipe-get recipe :after-init)))
           ;; Executed after loading package's files (which may be
           ;; autoloaded later)
           (after-load-body
            (el-get-normalize-recipe-init-form
             (el-get-recipe-get recipe :after-load)))
           (after-load-target
            (el-get-package-after-load-target package))
           (after-load-form
            (when after-load-body
              `(eval-after-load ,after-load-target
                 ,after-load-body)))
           (suppress-warning
            ;; TODO Make sure e.g. builtin/null type sets this property
            (el-get-recipe-get recipe :no-warn-missing-init))

           ;; Now set up individual forms. This is done here to avoid
           ;; the complexity of nested backquotes.
           (load-deps-form
            (when package-deps
              `(el-get-error "TODO init deps")))
           (load-path-form
            (when package-load-path-abs
              `(setq load-path
                     (nconc ',package-load-path-abs
                            load-path))))
           (autoload-form
            (when (file-exists-p autoload-file)
              `(load-file ,autoload-file)))
           (feature-forms
            (when immediate-load
              (mapcar (lambda (f) `(require ,f)) package-features)))
           (load-file-forms
            (when immediate-load
              (mapcar (lambda (f) `(load-file ,f)) load-files-fullpath)))
           (after-load-form
            (when after-load-body
              `(eval-after-load ,after-load-target
                 ,after-load-body)))
           (warning-form
            (unless suppress-warning
              (cond
               ((not (or autoload-form feature-forms
                         load-file-forms))
                `(display-warning
                  'el-get
                  (format
                   "Recipe for package %s has no autoloads, features, or load files. Initializing it may have no effect."
                   ',package)
                  :warning))
               ((and force-load
                     (null load-files)
                     (null features))
                `(display-warning
                  'el-get
                  (format
                   "Recipe for package %s has no features or load files, so it cannot be loaded eagerly."
                   ',package)
                  :warning))
               (t nil)))))
      (remove-if
       #'null
       `(progn
          ,load-deps-form
          ,warning-form
          ,load-path-form
          ,autoload-form
          ,init-form
          ,after-load-form
          ,@load-file-forms
          ,@feature-forms
          ,after-init-form)))))

(defsubst el-get-build-package-init-file (package &optional test)
  "Generate the init file for PACKAGE.

This should only be called after all other package build steps
have been completed.

With optional arg TEST, evaluate the init form in a clean Emacs
before writing the file. If the init form encounters an error,
then the init file will not be written and an error will be
raised"
  (let ((init-form (el-get-package-init-form package))
        (init-file (el-get-package-init-file package)))
    (when test
      (condition-case err
          (el-get-sandbox-eval
           `(progn
              ,init-form
              ;; Also test eager loading, which should test actually
              ;; loading the package's files and the after-load form.
              ,(el-get-package-init-form package :eager)))
        (error
         (el-get-error "Error in test evaluation of init form for package %s: %S"
                       package err))))
    (with-temp-buffer
      (insert
       (el-get-print-to-string init-form
                               :pretty))
      (write-file (el-get-package-init-file package)))))

;; TODO check status is installed, load init file instead of
;; generating init form
(defsubst el-get-init-package (package)
  "Initialize PACKAGE."
  (unless (eq (el-get-package-status package)
              'installed)
    (el-get-error "Cannot init non-installed package %s" package))
  (load-file (el-get-package-init-file package)))

(provide 'el-get-init)
;;; el-get-init.el ends here

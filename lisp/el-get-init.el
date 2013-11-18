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
(require 'el-get-recipe-manip)
(require 'el-get-package-internals)
(require 'el-get-dependencies)

(defun el-get-package-init-form (package &optional force-load)
  "Return a lisp form to init PACKAGE.

Evalutaing the return value of this function should initialize
the package.

With optional arg FORCE-LOAD, the returned form will, if
possible, skip autoloads and simply `require' or `load' the
package's `:feature'/`:load' properties as appropriate. If the
package's recipe does not have either of these properties, a
warning will be issued and this will return the same value it
would have returned if FORCE-LOAD was nil."
  (el-get-with-package-lock package
    (let* ((recipe (el-get-package-recipe package))
           (install-dir (el-get-package-install-directory package))
           (_ (unless recipe
                (el-get-error "Recipe not available for package %s" package)))

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
           ;; TODO Recipe validation: need an after-load-target if we
           ;; have an after-load form
           (after-load-target
            (el-get-package-after-load-target package))
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
            (unless (or autoload-form feature-forms
                        load-file-forms suppress-warning)
              `(el-get-warning-message
                "Recipe for package %s has no autoloads, features, or load files. Initializing it may have no effect."
                ,package)
              )))
      ;; TODO load eager if there is no autoloads
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

(defsubst el-get-init-package (package &optional force-load)
  "Initialize PACKAGE."
  (let ((init-form
         (el-get-package-init-form package force-load)))
    (el-get-debug-message "Init form for package %s: %S"
                          package init-form)
    (eval (el-get-package-init-form package force-load))))

(provide 'el-get-init)
;;; el-get-init.el ends here

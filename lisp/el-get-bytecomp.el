;;; el-get-bytecomp.el --- Functions for byte-compiling package files

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
(require 'el-get-async)
(require 'el-get-package-internals)

(defun el-get-byte-compile-files (files &rest async-props)
  "Byte-compile each of FILES in separate clean Emacs subprocesses.

Each file is loaded after compilation to make sure it is
loadable. Each file is processed in a separate subprocess from
each other file, so each compilation happens with a clean Emacs
subprocess (i.e. no unnecessary libraries have been loaded).

Note that this function *always* recompiles each file, regardless
of whether that files appears to be up to date or not.

Any additional arguments are passed as keyword arguments to
`el-get-async'.

Note: For convenience, if FILES contains nil or the empty string,
they will be skipped. However, any non-nil non-string value will
result in an error."
  (loop for f in files
        if (null f)
        do (ignore)
        else if (stringp f)
        do (when (> (length f) 0)
             (el-get-sandbox-eval
              `(progn (require 'bytecomp)
                      (byte-recompile-file ,f 'force 0 'load))))
        else do (el-get-error "Not a string: %S" f)))

;; TODO add compile prop to recipe validator
(defun el-get-assemble-files-for-byte-compilation (package)
  "Return the list of Emacs Lisp files to byte-compile for PACKAGE.

The returned paths will be absolute.

PACKAGE's recipe may have a `:compile' property, which is
interpreted as follows:

* If it is a string, it will be taken as a single path to
  compile. The path should be relative to the package's base
  directory. The path can name either a file or a directory. If
  it is a directory, all Emacs Lisp files in that directory will
  be compiled.

* If it is a list, it will be taken as a list of paths to
  compile, each interpreted as above. Note that an empty list
  does not mean to compile nothing, but is equivalent to
  `auto' (see below).

* If it is the symbol `all', then all Emacs Lisp files in
PACKAGE's load-path will be compiled.

* If it is the symbol `none', then nothing will be compiled. This
  is useful if the build process already compiles everything.

* If it is the symbol `auto' or nil, then it will be treated as
  `none' if the recipe has a `:build' property and `all' if it
  doesn't. This is the default behavior."
  (unless (memq (el-get-package-status package) '(fetched installed))
    (el-get-error "Cannot find lisp files for unfetched package: %s" package))
  (let* ((recipe
          (el-get-package-recipe package t))
         (compile-prop (el-get-recipe-get recipe :compile))
         (build-prop (el-get-recipe-get recipe :build))
         (lpath-prop (el-get-recipe-get recipe :load-path))
         (install-dir (el-get-package-install-directory package)))
    ;; Convert bare string to list of one string
    (when (stringp compile-prop)
      (setq compile-prop (list compile-prop)))
    ;; Expand symbol into approprate list
    (when (symbolp compile-prop)
      ;; Handle auto by choosing all or none
      (when (memq compile-prop '(auto nil))
        (el-get-debug-message "Auto compile-prop for package %s." package)
        (setq compile-prop
              (if build-prop 'none 'all)))
      (setq compile-prop
            (case compile-prop
              (all (el-get-as-list lpath-prop))
              ;; We use '(nil) instead of the empty list because nil
              ;; is equivalent to `auto'. Nil elements of list are
              ;; silently skipped, so this is ok.
              (none '(nil))
              (otherwise (el-get-error "Unrecognized compile property: %S"
                                       compile-prop)))))
    ;; compile-prop must now be a list of relative paths
    (unless (listp compile-prop)
      el-get-error ("Unrecognized compile property: %S" compile-prop))
    ;; Resolve paths relative to package install dir
    (setq compile-prop
          (loop for p in compile-prop
                if p collect (expand-file-name p install-dir)))
    ;; Search each path for elisp files and collect into a single list
    (el-get-debug-message "Searching for files to compile for package %s in: %S"
                          package compile-prop)
    (mapcan #'el-get-find-all-elisp-files
            (remove-if-not #'stringp compile-prop))))

;; TODO: Add package and dependencies' load paths to load-path before compiling
(defsubst el-get-byte-compile-package (package)
  "Do byte-compilation for PACKAGE."
  (let ((bytecomp-files (el-get-assemble-files-for-byte-compilation package)))
    (if bytecomp-files
        (progn
          (el-get-debug-message "Byte-compileing files for package %s: %S"
                                package bytecomp-files)
          (el-get-byte-compile-files bytecomp-files))
      (el-get-debug-message "Nothing to byte-compile for package %s."
                            package))))

(provide 'el-get-bytecomp)
;;; el-get-bytecomp.el ends here

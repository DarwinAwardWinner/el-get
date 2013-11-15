;;; el-get-info.el --- Facilities for building package Info files

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

(defun el-get-resolve-info-file (package)
  "Find the info file for PACKAGE.

This exists to normalize the `:info' property of a package's
recipe so that it points to the info file, not the containing
directory.

If PACKAGE does not provide an info file"
  (let* ((recipe (el-get-package-recipe package))
         (info-prop (el-get-recipe-get recipe :info)))
    (when info-prop
      (let ((info-prop-abs
             (expand-file-name
              info-prop
              (el-get-package-install-directory package))))
        (cond
              ;; Info property is a directory => find the info file in
              ;; that directory
              ((file-directory-p info-prop-abs)
               (let ((expected-info-file
                      (format "%s.info" package))
                     (info-files-in-dir
                      (directory-files info-prop-abs
                                       nil "\\.info\\'")))
                 (cond
                  ;; Found info file with expected name
                  ((member expected-info-file
                          info-files-in-dir)
                   (expand-file-name expected-info-file
                                     info-prop-abs))
                  ;; Found only one info file, but with unexpected
                  ;; name. Use it with a warning.
                  ((= (length info-files-in-dir 1))
                   (expand-file-name (car info-files-in-dir)
                                     info-prop-abs))
                  ;; Found multiple info files, and none of them has
                  ;; the expected name. Warn and return no info.
                  (t
                   (el-get-warning-message
                    "Info dir \"%s\" for package %s contains multiple info files. Please modify the recipe's `:info' property to name one of the following files: %S"
                    info-prop package
                    (mapcar (lambda (f) (expand-file-name f info-prop))))
                   nil))))
              ;; Info property is a file => use it
              ((file-exists-p info-prop-abs)
               info-prop-abs)
              ;; Info property doesn't point to an existing file or
              ;; directory => error.
              (t
               (el-get-error
                "Info property \"%s\" for package %s points to a nonexistent path."
                info-prop package)))))))

(defsubst el-get-package-info-directory (package)
  "Return the info directory for PACKAGE.

This directory will be added to `Info-directory-list' when the
package is initialized."
  (file-name-directory (el-get-resolve-info-file package)))

(defun el-get-build-package-info (package)
  "Build info dir for PACKAGE.

If PACKAGE's recipe has no `:info' property, this does nothing."
  (when (el-get-recipe-get (el-get-package-recipe package) :info)
    (let* ((info-file (el-get-resolve-info-file package))
           ;; This is a file with the name "dir" in the same directory
           ;; as the info file.
           (info-dir-file
            (expand-file-name
             "dir" (file-name-directory info-file))))
      ;; TODO: Customizable path to install-info
      (if (executable-find "install-info")
          (call-process "install-info" nil nil nil info-file info-dir-file)
        (el-get-warning-message
         "Not building info for package %s because `install-info' is not in your $PATH"
         package)))))

(provide 'el-get-info)
;;; el-get-info.el ends here

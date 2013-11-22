;;; el-get-autoload-generation.el --- Facilities for generating autoloads for el-get packages

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
  (expand-file-name
   (format "%s-autoloads.el" package)
   (el-get-package-install-directory package)))

(defun el-get-check-autoload-file-has-content (&optional file)
  "Check that FILE has \"content\".

\"Content\" means a top-level lisp form that is not call to
`provide'."
  (setq file
        (or file generated-autoload-file
            (el-get-error "No FILE argument provided and `generated-autoload-file' is not set.")))
  (and
   (file-exists-p file)
   (el-get-with-file-lock file
     ;; It's not an error if there is a buffer with pending changes to
     ;; FILE, but it is highly suspect.
     (when (find-buffer-visiting file #'buffer-modified-p)
       (el-get-warning-message
        "A buffer is currently visiting file %s: %s"
        file (buffer-file-name
              (find-buffer-visiting file #'buffer-modified-p))))
     (with-temp-buffer
       (insert-file-contents file)
       (goto-char (point-min))
       (condition-case err
           (loop for form = (read (current-buffer))
                 unless (eq (car form) 'provide)
                 return t)
         ;; If we hit the end of the file, there is no content.
         (end-of-file nil))))))

(defun el-get-cat-files-into-autoload-file (package files)
  "Concatenate FILES into PACKAGE's autoload file.

FILES should be a list of relative paths to files starting from
PACKAGE's install directory.

The autoload file will have the expected header & footer, and
each FILE's content will be flanked by beginning and ending
delimiting comments, and separeted from other files by formfeed
characters, as is typical for autoload files. The generated
autoload file will provide the feature `PACKAGE-autoloads'."
  (let ((generated-autoload-file
         (el-get-package-autoload-file package))
        (install-dir (el-get-package-install-directory package)))
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
      (loop
       for path in files
       ;; This is necessary to make sure that "foo" and "foo.el"
       ;; resolve to the same path, and that we disallow
       ;; "foo.elc".
       for abspath =
       (let ((load-suffixes (remove ".elc" load-suffixes)))
         (locate-library
          (expand-file-name path install-dir)))
       ;; Record relative path for the comments
       for relpath = (file-relative-name abspath install-dir)
       ;; Insert leading comment, file contents, trailing
       ;; commend, and formfeed line.
       do (progn
            (insert
             (format ";;;; Contents of el-get package file %s\n\n"
                     relpath))
            (insert-file-contents abspath)
            ;; Ensure a newline after file contents
            (unless (looking-back "\n")
              (insert "\n"))
            (insert
             (format
              "\n;;;; End contents of el-get package file %s\n\f\n"
              package)))))))

;; TODO: Clarify the difference between "generate autoloads from FILE"
;; and "Use FILE as an autoload file"
(defun el-get-generate-package-autoloads (package)
  "Generate autoloads file for PACKAGE.

If a pacakge's `:autoloads' property is t, then autoloads will be
generated from all Emacs Lisp files in the package's
load-path. If it is nil, then no autoloads will be generated. If
it is a string or a list of strings, these will be taken as paths
to one or more pre-built autoload files and their contents will
be copied (possibly by symlinking) to the package's main autoload
file.

If the generated autoload file doesn't actually autoload
anything, it will be deleted. The absence of an autoload file
indicates that the package does not provide autoloads and cannot
be loaded lazily.

This returns non-nil if an autoload file was generated, nil if
not."
  (el-get-with-package-lock package
    (let* ((recipe (el-get-package-recipe package t))
           (install-dir (el-get-package-install-directory package))
           (autoload-prop (or autoload-prop
                              (el-get-recipe-get recipe :autoloads)))
           (lpath-prop (el-get-package-load-path package))
           (pkg-autoload-file (el-get-package-autoload-file package)))
      (cond
       ;; `:autoloads nil' => Don't generate anything
       ((null autoload-prop)
        (el-get-debug-message "Not generating autoloads for package %s"
                              package)
        nil)
       ;; `:autoloads t' => Generate autoloads from all elisp files in
       ;; package's load-path
       ((eq autoload-prop t)
        (el-get-debug-message "Generating autoloads for package %s"
                              package)
        (when (file-exists-p pkg-autoload-file)
          (el-get-warning-message
           "Overwriting pre-built package-autoload file for %s: %S"
           package (file-relative-name pkg-autoload-file install-dir)))
        (let ((generated-autoload-file pkg-autoload-file))
          (apply
           #'update-directory-autoloads
           (loop for dir in lpath-prop
                 collect (expand-file-name dir install-dir)))
          (if (el-get-check-autoload-file-has-content
               generated-autoload-file)
              t
            (el-get-debug-message
             "Deleting autoload file for package %s because it does not provide any autoloads."
             package)
            (delete-file generated-autoload-file)
            nil)))
       ;; `:autoloads STRING' => STRING names a pre-built autoload
       ;; file; copy or symlink.
       ((stringp autoload-prop)
        (el-get-debug-message "Using pre-built autoload file for package %s: %s"
                              package autoload-prop)
        ;; TODO cleanup
        (if (el-get-same-files
             (expand-file-name autoload-prop install-dir)
             pkg-autoload-file)
            (el-get-debug-message
             "Pre-built autoload file already has desired name: %s"
             (file-relative-name pkg-autoload-file install-dir))
          (el-get-link-or-copy
           (expand-file-name autoload-prop install-dir)
           pkg-autoload-file))
        t)
       ;; `:autoloads (STRING STRING ...)' => Multiple pre-built
       ;; autoload files; Concatenate them into autoload file.
       ((el-get-list-of-strings-p autoload-prop)
        ;; Make sure autoload-prop doesn't contain the destination
        (let ((autoload-prop-abs
               (mapcar (lambda (f) (expand-file-name f install-dir)))))
          (when (member* pkg-autoload-file autoload-prop-abs
                         :test #'el-get-same-files)
            (el-get-warning-message
             "Autoloads property for package %s contains the destination file for generated autoloads (%s). This file will be overwritten with the combined contents of all specified autoload files. This operation is not idempotent. The recipe probably needs to be fixed."
             package (file-relative-name pkg-autoload-file install-dir)))
          (el-get-debug-message
           "Collecing pre-built autoload files for package %s: %S"
           package autoload-prop)
          (when (file-exists-p pkg-autoload-file)
            (el-get-warning-message
             "Overwriting existing autoload file for %s: %S"
             package (file-relative-name pkg-autoload-file install-dir)))
          ;; TODO Check for autoloads file in autoload-prop
          (el-get-cat-files-into-autoload-file package autoload-prop)
          t)
        (t (el-get-error "Unrecognized :autoloads property: %S"
                         autoload-prop))))))

(provide 'el-get-autoload-generation)
;;; el-get-autoload-generation.el ends here

;;; el-get --- Manage the external elisp bits and pieces you depend upon
;;
;; Copyright (C) 2010-2011 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: http://www.emacswiki.org/emacs/el-get
;; GIT: https://github.com/dimitri/el-get
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.
;;
;; Install
;;     Please see the README.asciidoc file from the same distribution


(defcustom el-get-protect-package-files t
  "If t, prompt the user before saving files in package directories.

El-get generally does not expect users to modify package files
directly. In particular, if the user modifies a package file and
then tries to update that package using `el-get-update', any
combination of the following could occur:

  - the update could erase all the user's changes;
  - the update could fail with an error;
  - the package could be left in an unusable state;

To prevent this, the user is prompted when trying to save files
in the directory of a package that is managed by
el-get. Regardless, some users may know and accept these risks
and still find it convenient to edit package files in
place. Setting this variable to nil will disable the prompt when
saving package files.

As an alternative to editing files in place, consider one of the
following options, all of which should interact nicely with
package updates: [TODO INSERT RECOMMENDATIONS HERE]"
  :group 'el-get
  :type 'boolean)

(defun el-get-file-protected-p (file)
  "Return t if FILE is managed by el-get.

These include any package files, as well as the status file. It
is recommended that users not manually edit such files in most
cases.

See `el-get-protect-package-files'."
  (string-prefix-p (file-name-as-directory el-get-dir)
                   (expand-file-name file)))

(defun el-get-buffer-protected-p (&optional buffer)
  "Return t if BUFFER's file is managed by el-get.

See `el-get-protect-package-files'."
  (el-get-file-protected-p (buffer-file-name buffer)))

(defun el-get-protect-before-save-hook ()
  "Prompt before saving files managed by el-get.

See `el-get-protect-package-files'."
  (when (and el-get-protect-package-files
             (el-get-buffer-protected-p))
    (if (yes-or-no-p (format "%s is managed by el-get. Are you sure you want to modify this file (see `el-get-protect-package-files' for more info)?" (buffer-file-name))))
        ;; If yes, do nothing
        nil
      ;; If no, abort the save
      (error "Refusing to save el-get-managed file %s" (buffer-file-name))))

(add-hook before-save-hook 'el-get-protect-before-save-hook)

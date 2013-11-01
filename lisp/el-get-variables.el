;;; el-get-variables.el ---

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

(defconst el-get-base-directory
  ;; This should give the right path whether this file is being
  ;; loaded, or this form is being evalled via e.g. C-x C-e.
  (expand-file-name
   ".."
   (file-name-directory
    (or load-file-name
        (locate-library "el-get-internals")
        (when (string-match-p "el-get-internals.el\\'"
                              buffer-file-name)
          buffer-file-name)
        (error "Cannot determine path to el-get."))))
  "Base directory of el-get installation.")

;; TODO: Write function to copy the install dir to a new location,
;; then change this variable, then delete the old location.
(defvar el-get-install-dir
  (expand-file-name "el-get" user-emacs-directory)
  "Directory where El-get will install packages.

Generally you should not change this, unless you also remember to
move all your installed packages to the new location.")

(defvar el-get-in-subprocess nil
  "This is non-nil in a subprocess started by El-get.

It should always be nil in the main Emacs process.")

(provide 'el-get-variables)
;;; el-get-variables.el ends here

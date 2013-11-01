;;; el-get-path-manip.el ---

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

(defun el-get-join-two-paths (path1 path2)
  (if path2
      (if (file-name-absolute-p path2)
          path2
        (concat (file-name-as-directory path1) path2))
    path1))

(defun el-get-join-path-1 (paths)
  (when paths
    (el-get-join-two-paths
     (car paths)
     (el-get-join-path-1 (cdr paths)))))

(defun el-get-join-path (&rest paths)
  "Join multiple path components.

This function takes any number of string arguments and repeatedly
joins successive arguments with `expand-file-name'. Arguments may
also be lists of strings.

If this function receives no arguments or only empty lists as
arguments, it will raise an error."
  (or (el-get-join-path-1 (message-flatten-list paths))
      (el-get-error "No paths passed to `el-get-join-path'.")))

(provide 'el-get-path-manip)
;;; el-get-path-manip.el ends here

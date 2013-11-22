;;; el-get-protect-package-files.el --- Prevent accidental user modification of package files

;; Copyright (C) 2013  Ryan C. Thompson

;; Author: Ryan C. Thompson <rct@thompsonclan.org>
;; Keywords: convenience

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

;; This package attempts to warn the user away from modifying files
;; that belong to el-get packages. This is a bad idea because el-get
;; can always modify or delete any file in its package directories, so
;; there is no guarantee that user modifications will be preserved.

;;; Code:



(provide 'el-get-protect-package-files)
;;; el-get-protect-package-files.el ends here

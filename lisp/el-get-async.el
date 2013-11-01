;;; el-get-async.el --- el-get interface to async.el

;; Copyright (C) 2013  Ryan C. Thompson

;; Author: Ryan C. Thompson(require 'cl) <rct@thompsonclan.org>
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
(require 'async)
(require 'el-get-variables)

;; TODO Move to appropriate place
(defconst el-get-emacs (concat invocation-directory invocation-name)
  "Path to find the currently running emacs.")

(defun* el-get--async (expr &key
                            finish-func
                            (subproc-default-directory
                             default-directory)
                            (subproc-load-path load-path)
                            require-features load-files
                            export-variables)
  "Eval EXPR asynchronously in a batch-mode Emacs subprocess,

TODO DOC"
  (let* (;; Make sure these are all lists
         (subproc-load-path
          (cond
           ((listp subproc-load-path)
            subproc-load-path)
           ((stringp subproc-load-path)
            (list subproc-load-path))
           ;; Fallback: use current `load-path'
           (t load-path)))
         (require-features
          (if (listp require-features)
              require-features
            (list require-features)))
         (load-files
          (if (listp load-files)
              load-files
            (list load-files)))
         (export-variables
          (cl-mapcan
           (lambda (var)
             (cond
              ((symbolp var)
               (cons var (eval var)))
              ((consp var)
               var)
              (t var)))
           export-variables))
         (full-expr
          `(progn
             (setq load-path ',subproc-load-path)
             (mapc #'require ',require-features)
             (mapc #'load ',load-files)
             (setq ,@export-variables
                   default-directory ,subproc-default-directory
                   el-get-in-subprocess t)
             ,expr)))
    (async-start `(lambda () ,full-expr) finish-func)))

(provide 'el-get-async)
;;; el-get-async.el ends here

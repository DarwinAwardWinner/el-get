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
(require 'el-get-internals)

;; TODO Move to appropriate place
(defconst el-get-emacs (concat invocation-directory invocation-name)
  "Path to find the currently running emacs.")

(defun* el-get-async-start (expr &key
                            finish-func
                            (subproc-default-directory
                             default-directory)
                            (subproc-load-path load-path)
                            require-features load-files
                            export-variables)
  "Eval EXPR asynchronously in a batch-mode Emacs subprocess.

The returned value is the future returned by `async-start'. It
can be manipulated with all the normal async functions, such as
`async-get'.

The following keyword arguments are available:

* :finish-func - Same as the FINISH-FUNC argument to
  `async-start'.

* :export-variables - This is a list of variables to export to
  the subprocess. It should be a list in which each item is
  either a symbol or a cons cell whose car is a symbol. For an
  item that is a symbol, that variable will be exported to the
  subprocess with its current value in the parent Emacs. For an
  item that is a cons cell, the variable named in the car will be
  exported to the subprocess with the value given in the cdr. In
  all cases, the exported value must be printable.

* :subproc-default-directory - Subprocess will `cd' to this
  directory before evaluating EXPR. By default this is the
  `default-directory' of the parent Emacs.

* :subproc-load-path - Subprocess will set `load-path' to this
  value before evaluating EXPR. By default this is the
  `load-path' of the parent Emacs.

* :require-features - Subprocess will `require' each of these
  features before evaluating EXPR.

* :load-files - Subprocess will `load' each of these files before
  evaluating EXPR."
  (let* ((export-variables-setq-list
          (loop
           for item in export-variables
           ;; Convert symbol to `(cons symbol (eval symbol))'
           if (symbolp item)
           do (setq item (cons item (eval item)))
           ;; Must be a cons
           unless (consp item)
           do (error "Unknown :export-variables variable specification: %S"
                     item)
           ;; Car must be a bindable symbol
           unless (el-get-bindable-symbol-p (car item))
           do (error "Car of :export-variables item is not a variable name: %S"
                     item)
           ;; Cdr must be printable
           unless (async--value-printable-p (cdr item))
           do (error "Value of variable %s is not printable: %S"
                     (car item) (cdr item))
           ;; Collect as one big list, the arg list to `setq'
           nconc (list (car item) (cdr item))))
         (full-expr
          `(progn
             (setq load-path ',(el-get-as-list subproc-load-path))
             (mapc #'require ',(el-get-as-list require-features))
             (mapc #'load ',(el-get-as-list load-files))
             (setq ,@export-variables
                   el-get-in-subprocess t)
             (cd-absolute ,subproc-default-directory)
             ,expr)))
    (async-start `(lambda () ,full-expr) finish-func)))

(defun el-get-sandbox-eval (expr &rest kwargs)
  "Eval EXPR in a clean batch-mode Emacs subprocess.

Unlike `el-get-async-start', this runs synchronously and returns
the result of evaluating EXPR. Args after the first are keyword
args to `el-get-async-start' (but you may not specify FINISH-FUNC
since that is already taken)."
  (async-get (apply #'el-get-async-start
                    expr :finish-func nil
                    kwargs)))

(provide 'el-get-async)
;;; el-get-async.el ends here

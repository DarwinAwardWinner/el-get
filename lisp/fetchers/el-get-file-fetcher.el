;;; el-get-file-fetcher.el --- Fetcher for single-file packages

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


(require 'url-parse)
(require 'el-get-internals)
(require 'el-get-recipe-manip)
(require 'el-get-download)

(defconst el-get-file-valid-url-protocols
  '("file" "http" "https" "ftp")
  "List of URL protocols that el-get knows how to download.")

(defun el-get-file-url-valid-p (url)
  "Returns non-nil if URL looks like a valid URL that el-get can handle.

See `el-get-file-valid-url-protocols' for a list of URL types
that el-get can handle. The return value is actually the protocol
that was detected.

URL can be either a string or a url struct (see url-parse.el)."
  (car (member (url-type (el-get-as-url-struct url))
               el-get-file-valid-url-protocols)))

(defun el-get-elisp-filename-p (fname)
  "Return non-nil if FNAME could be the name of an elisp file."
  (el-get-string-suffix-p ".el" fname))

(defun el-get-file-validate-recipe (recipe)
  ;; A file recipe just has to have a `:url' property that looks like
  ;; a URL.
  (el-get-validate-recipe-properties recipe
    '(:url #'el-get-file-url-valid-p
      :file-name #'el-get-elisp-filename-p)))

(defsubst el-get-file-make-local-name (recipe)
  "Try to choose an appropriate local file name based on RECIPE's `:url'.

RECIPE should be a recipe of type `file'.

If no file name can be determined from the URL, an error is signaled."
  (let ((candidate-name
         (file-name-nondirectory
          (url-filename
           (el-get-as-url-struct
            (el-get-recipe-get recipe :url))))))
    (if (el-get-nonempty-string-p candidate-name)
        (el-get-ensure-suffix candidate-name ".el")
      (el-get-error
       "Could not determine file name for recipe %s from URL %S. Try specifying a `:file-name' property explicitly in the recipe."
       (el-get-recipe-name recipe)
       (el-get-as-url-string
        (el-get-recipe-get recipe :url))))))

(defun el-get-file-fetch (recipe destdir)
  (el-get-download-file
   (el-get-recipe-get recipe :url)
   (expand-file-name (el-get-recipe-get recipe :file-name)
                     destdir)
   ;; TODO Make this number a preference
   3)
  ;; TODO check the checksum immediately after fetching  and abort on
  ;; mismatch
  )

(defun el-get-file-compute-checksum (&rest WHAT_SHOULD_ARGS_BE?)
  (el-get-error "UNIMPLEMENTED"))

(el-get-register-fetcher
  :type 'file
  :fetch #'el-get-file-fetch
  :validate #'el-get-file-validate-recipe
  ;; :compute-checksum #'el-get-file-compute-checksum
  :auto-property
  (lambda (recipe prop)
    (case prop
      (:file-name (el-get-file-make-local-name recipe))
      (:website (el-get-recipe-get recipe :url))
      (:load-path ".")
      (:autoloads t))))

(provide 'el-get-file-fetcher)
;;; el-get-file-fetcher.el ends here

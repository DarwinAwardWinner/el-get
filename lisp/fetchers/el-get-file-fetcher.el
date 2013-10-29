(require 'cl)
(require 'url-parse)
(require 'el-get-internals)
(require 'el-get-recipe-manip)

(defconst el-get-file-valid-url-protocols
  '("file" "http" "https" "ftp")
  "List of URL protocols that el-get knows how to download.")

(defun el-get-as-url-struct (url)
  "Parse URL into a URL struct, or return it if it already is one."
  (if (url-p url)
        url
      (url-generic-parse-url url)))

(defun el-get-as-url-string (url)
  "Convert a URL struct into a string, or return a string as is.

Throws an error for all other inputs."
  (cond
   ((url-p url)
    (url-recreate-url url))
   ((stringp url)
    url)
   (t
    (el-get-error "Not a URL: %S" url))))

(defun el-get-file-validate-url (url)
  "Returns non-nil if URL looks like a valid URL that el-get can handle.

See `el-get-file-valid-url-protocols' for a list of URL types
that el-get can handle. The return value is actually the protocol
that was detected.

URL can be either a string or a url struct (see url-parse.el)."
  (car (member (url-type (el-get-as-url-struct url))
               el-get-file-valid-url-protocols)))

(defun el-get-file-validate-recipe (recipe)
  ;; A file recipe just has to have a `:url' property that looks like
  ;; a URL.
  (el-get-validate-recipe-properties recipe
    (list :url #'el-get-file-valididate-url)))

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
  (url-copy-file
   (el-get-recipe-get recipe :url)
   (expand-file-name (el-get-recipe-get recipe :file-name)
                     destdir))
  ;; TODO check the checksum immediately after fetching  and abort on
  ;; mismatch
  )

(defun el-get-file-compute-checksum (&rest WHAT_SHOULD_ARGS_BE?)
  el-get-error "UNIMPLEMENTED")

(el-get-register-fetcher
  :type 'file
  :fetch #'el-get-file-fetch
  :validate #'el-get-file-validate-recipe
  ;; :compute-checksum #'el-get-file-compute-checksum
  :auto-property
  #'(lambda (recipe prop)
      (case prop
        (:file-name (el-get-file-make-local-name recipe)))
        (:website (el-get-recipe-get recipe :url)))))

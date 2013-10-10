(defvar el-get-fetchers (make-hash-table :size 20)
  "Hash table of registered package fetching methods.

TODO DOC")

(defsubst el-get--get-fetcher (type)
  "Retrive full fetcher definition for TYPE.

Throws an error if the requested fetcher does not exist"
  (or (gethash type el-get-fetchers)
      ;; TODO: el-get-error macro
      (error "Fetcher type %s not registered" type)))

(defsubst el-get--set-fetcher (type def)
  "Set fetcher definition for TYPE to DEF.

Performs basic validation ot DEF before setting it."
  ;; Validate definition
  (unless (and
           ;; Must be a hash table
           (hash-table-p def)
           ;; Must have a :fetch or :filter
           (or (gethash :fetch def)
               (gethash :filter def))
           ;; All properties must be functions
           (condition-case nil
               (prog1 t
                 (maphash
                  (lambda (k v)
                    (or (functionp v)
                        (error "Not a function")))
                  def))
             (error nil)))
    (error "Invalid fetcher definition"))
  (puthash type def el-get-fetchers))

(defun el-get-fetcher-op (type operation)
  "Return the OPERATION function for fetcher type TYPE.

TYPE may either be the name of a fetcher type, the definition of
one, or a recipe whose type property will be used. If the
operation is not defined for the given type, returns nil. If the
given type is not a recognized recipe type, throws an error. TODO"
  (cond
   ;; Fetcher definition
   ((hash-table-p type)
    (gethash operation type))
   ;; Symbol naming a fetcher
   ((symbolp type)
    (el-get-fetcher-op (el-get--fetcher type) operation))
   ;; String naming a fetcher
   ((stringp type)
    (el-get-display-warning "String used instead of symbol to name a type")
    (el-get-fetcher-op (intern type) operation))
   ;; Recipe
   ((listp type)
    (el-get-fetcher-op (el-get-recipe-type type) operation))
   ;; Unrecognized fetcher type
   ((null type)
    (error ))))

(defsubst el-get-fetcher-real-p (type)
  "Returns t if TYPE is a real fetcher type.

TYPE may either be a symbol naming a fetcher type or a hash table
that defines a fetcher type."
  (el-get-fetcher-op type :fetch))

(defsubst el-get-fetcher-virtual-p (type)
  "Returns t if TYPE is a virtual fetcher type.

TYPE may either be a symbol naming a fetcher type or a hash table
that defines a fetcher type."
  (el-get-fetcher-op type :filter))

(defsubst el-get-fetcher-defined-p (type)
  "Returns t if TYPE is a registered fetcher type."
  (or (el-get-fetcher-real-p type)
      (el-get-fetcher-virtual-p type)))

(defun* el-get-register-fetcher (type &key fetch update remove
                                     compute-checksum guess-metadata)
  "TODO DOC

A fetcher must have at least a `:fetch' attribute; all others are
optional. The values of each attribute should be functions that
each take the same two arguments RECIPE and DESTDIR (unless noted
otherwise below) and must conform to the following:

* `:fetch': This function should fetch the file(s) for the
  package described by RECIPE into DESTDIR, such that the package
  is ready to be built. This function is the only required
  function to define a fetcher.

* `:update': Same as `:fetch', but it should assume that DESTDIR
  already contains a previously-installed version of the
  package. The intended use of this is for VCS-based fetchers
  that can simply pull changes instead of re-downloading the
  entire package. If this is not provided, the package will be
  updated by deleting it and then calling the fetch function
  again.

* `:remove': This function should uninstall the package located
  in DESTDIR. If this is not provided, the default is to simply
  delete the directory and all its contents. This should only be
  provided if deleting the package directory is not sufficient to
  uninstall the package.

* `:compute-checksum': This will be called only on an installed
  package in order to compute a checksum. TODO

* `:auto-property': This function takes two arguments, RECIPE and
  PROPERTY, and should return an auto-generated value for that
  property. It will be called to in order to fill in various
  metadata fields about the package. For example, this could be
  used to supply-auto-generated `:description' or `:website'
  properties. This function will only be called for properties
  not explicitly specified by a recipe, so there is no need to
  check if the requested property already exists.

* `:validate': This function only takes a RECIPE argument and
  should verify that the recipe is valid. It should return nil
  for a valid recipe and for an invalid recipe, it should return
  either a string or a list of strings that describe all the
  problems found with the recipe. For example, if the fetcher
  requires the recipe to have a `:url' property, then the
  `:validate' function should verify that the recipe has this
  property. If not, it could return the string \"recipe must
  have :url property\". The validation function may assume that
  it is being called o a recipe with a `:name' property and the
  correct `:type' property.
"
  ;; Sanity check
  (unless (and type (symbolp type))
    (error "TYPE must be a symbol"))
  ;; Warn about redefining fetchers
  (when (gethash type el-get-fetchers)
    (el-get-display-warning (format "Re-registering recipe type %s" type)
                            :debug))
  (let ((fetcher-def (make-hash-table :size 5)))
    (loop for required-key in '(fetch)
          for value = (eval required-key)
          unless value
          do (error "Missing required keyword argument: :%s" required-arg)
          do (puthash (intern (format ":%s" required-arg))
                      value fetcher-def))
    (loop for optional-key in '(update remove compute-checksum guess-metadata)
          for value = (eval optional-key)
          if value
          do (puthash (intern (format ":%s" optional-arg))
                      value fetcher-def))
    (puthash :type type fetcher-def)
    (el-get--set-fetcher type fetcher-def)))
(put 'el-get-register-fetcher 'lisp-indent-function 1)

(defun el-get-register-fetcher-alias (newtype oldtype)
  "TODO DOC"
  (el-get--set-fetcher
   newtype
   (el-get--get-fetcher oldtype)))

(defun el-get-register-virtual-fetcher (type filter)
  (el-get--set-fetcher
   type
   (el-get--plist-to-hash
    (list :type type
          :filter filter))))
(put 'el-get-register-virtual-fetcher 'lisp-indent-function 1)

(defsubst el-get-recipe-type (recipe)
  "TODO DOC"
  (el-get-plist-get recipe :type))

(defsubst el-get-recipe-autoget (recipe prop)
  "Get auto-generated value of PROP for RECIPE.

TODO More info"
  ;; :name and :type may be auto-generated
  (unless (memq prop '(:name :type))
    (funcall (el-get-fetcher-op recipe :auto-property) recipe prop)))

(defsubst el-get-recipe-get (recipe prop)
  "Like `plist-get' but includes auto-generated recipe properties."
  (or
   (plist-get recipe prop)
   (el-get-recipe-autoget recipe prop)))

(defun el-get-validate-recipe (recipe)
  "TODO"
  (let ((errors nil))
    (unless (el-get-recipe-get recipe :name)
      (push "Recipe has no name" errors))
    (unless (el-get-recipe-get recipe :type)
        (push "Recipe has no type" errors))
    (or errors
        (funcall (el-get-fetcher-op recipe :validate)
                 recipe))))

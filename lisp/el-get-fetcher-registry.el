;;; el-get-fetcher-registry.el --- Facilities for registering and accessing package fetchers

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

(require 'cl)
(require 'el-get-internals)
(require 'el-get-recipe-manip)

(defconst el-get-fetcher-required-props
  (list :type #'el-get-bindable-symbol-p
        :fetch #'functionp
        ;; Must not have a :filter
        :filter #'null)
  "Plist of required fetcher properties.

Each property in this list is a property that must be provided to
`el-get-register-fetcher', and that property's value in this list
is a predicate for the provided value to satisfy. (Null means
that the property must *not* be provided.)")

(defconst el-get-fetcher-optional-props
  (list :update #'functionp
        :remove #'functionp
        :compute-checksum #'functionp
        :auto-property #'functionp
        :validate #'functionp
        :documentation #'stringp)
  "Plist of optional fetcher properties.

Each property in this list is a property that may be provided to
`el-get-register-fetcher', and that property's value in this list
is a predicate for the provided value to satisfy.")

(defconst el-get-virtual-fetcher-required-props
  (list :type #'el-get-bindable-symbol-p
        :filter #'functionp
        ;; Must not have a :fetch, :update, or :remove
        :fetch #'null
        :update #'null
        :remove #'null)
  "Plist of required fetcher properties.

Each property in this list is a property that must be provided to
`el-get-register-fetcher', and that property's value in this list
is a predicate for the provided value to satisfy. (Null means
that the property must *not* be provided.)")

(defconst el-get-virtual-fetcher-optional-props
  (list :auto-property #'functionp
        :validate #'functionp
        :documentation #'stringp)
  "Plist of optional fetcher properties.

Each property in this list is a property that may be provided to
`el-get-register-fetcher', and that property's value in this list
is a predicate for the provided value to satisfy.")

(defun el-get-validate-fetcher-def (def)
  "Throw an error if DEF does not look like a valid fetcher definition."
  ;; We validate it as a plist
  (when (hash-table-p def)
    (setq def (el-get-hash-to-plist def)))
  (let ((errors
         (cond
          ((plist-get def :filter)
           (el-get-validate-plist def
                                  el-get-virtual-fetcher-required-props
                                  el-get-virtual-fetcher-optional-props))
          (t
           (el-get-validate-plist def
                                  el-get-fetcher-required-props
                                  el-get-fetcher-optional-props)))))
    (when errors
     (el-get-error
      "Errors encountered during fetcher validation:\n%s\n\nFetcher definition was:\n%S"
      (mapconcat #'identity errors "\n")
      def))))

(defvar el-get-fetchers (make-hash-table :size 20)
  "Hash table of registered package fetching methods.

TODO link to docs")

(defsubst el-get-get-fetcher (type)
  "Retrive full fetcher definition for TYPE.

Throws an error if the requested fetcher does not exist."
  (or (gethash type el-get-fetchers)
      (el-get-error "Fetcher type %s not registered" type)))

(defun el-get-set-fetcher (type def)
  "Set fetcher definition for TYPE to DEF.

Validates DEF before setting it, raising an error if it fails to
validate."
  ;; Validate fetcher definition
  (el-get-validate-fetcher-def def)
  ;; Make sure TYPE is the same as DEF's value for :type
  (when (not (eq type (plist-get def :type)))
    (el-get-error "Tried to register fetcher named %s under the name %s"
                  (plist-get def :type) type))
  ;; Don't use `el-get-fetcher-registered-p' here because it isn't
  ;; defined yet.
  (when (gethash type el-get-fetchers)
    (el-get-display-warning (format "Re-registering recipe type %s" type)
                            :debug))
  ;; If DEF was passed as a plist, convert to hash table
  (when (listp def)
    (setq def (el-get-plist-to-hash def)))
  (puthash type def el-get-fetchers))

(defun el-get-fetcher-op (type operation)
  "Return the OPERATION function for fetcher type TYPE.

TYPE may either be the name of a fetcher type, the definition of
one, or a recipe, whose type property will be used. If the
operation is not defined for the given type, this returns nil. If
the given type is not a recognized recipe type, throws an
error. TODO"
  ;; Multipe dispatch via recursion
  (cond
   ;; String naming an operation
   ((stringp operation)
    (el-get-display-warning "String used instead of symbol to name an operation")
    (el-get-fetcher-op type (intern operation)))
   ;; Hash table = Fetcher definition
   ((hash-table-p type)
    (gethash operation type))
   ;; Symbol naming a fetcher
   ((symbolp type)
    (el-get-fetcher-op (el-get-get-fetcher type) operation))
   ;; String naming a fetcher
   ((stringp type)
    (el-get-display-warning "String used instead of symbol to name a type")
    (el-get-fetcher-op (intern type) operation))
   ;; List = Recipe definition
   ((listp type)
    (el-get-fetcher-op (el-get-recipe-type type) operation))
   ;; Unrecognized fetcher type
   ((null type)
    (el-get-error "Unrecognized fetcher type"))))

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

(defsubst el-get-fetcher-registered-p (type)
  "Returns t if TYPE is a registered fetcher type."
  (or (el-get-fetcher-real-p type)
      (el-get-fetcher-virtual-p type)))

;; TODO: add a DOC argument and make it available via
;; `el-get-describe-package-type' or similar function.
(defun el-get-register-fetcher (&rest def)
  "Register a fetcher method for el-get.

There are two kinds of fetchers, \"real\" and \"virtual\". A real
fetcher defines a type of package and describes how to fetch,
update, remove, and perform other actions on packages of that
type. A virtual fetcher instead just defines a transformation
from one type into another.

A real fetcher is registered by providing, at a minimum, `:type'
and `:fetch' properties, while a virtual fetcher is registered by
providing `:type' and `:filter' properties. These are described
below.

All fetchers must have a `:type' property, whose value must be a
symbol that is the name of the fetcher type. Possible examples
include `git', `file', `emacswiki', `github', and `svn'. All
fetchers *should* also provide a `:doc' property that is a
documentation string describing how to write recipes of that
type.

Real fetchers can additionally provide any of the below:

* `:fetch': This should be a function that takes two arguments,
  RECIPE and DESTDIR. It should fetch the file(s) for the package
  described by RECIPE into DESTDIR, such that the package is
  ready for building. This function is the only required function
  to define a real fetcher; all others are optional.

* `:update': Same as `:fetch', but it should assume that DESTDIR
  already contains a previously-installed version of the
  package. The intended use of this is for VCS-based fetchers
  that can simply pull changes instead of re-downloading the
  entire package. If this is not provided, the package will be
  updated by deleting it and then calling the fetch function
  again. TODO what if the recipe changes?

* `:remove': This function should take a single argument,
  DESTDIR, and uninstall the package located in DESTDIR. It can
  be assumed that the package in DESTDIR is of the type that this
  fetcher is responsible for. If this is not provided, the
  default remove operation is to simply delete the directory and
  all of its contents. This should only be provided if deleting the
  package directory is not sufficient to uninstall the package.

* `:compute-checksum': TODO What arguments does it receive? When
  is it called?

Virtual fetchers need only define a `:filter' property, whose
value should be a function that takes one argument, RECIPE, and
returns a recipe of a different type. For example, the `github'
fetcher could be a virtual fetcher that simply transforms
`github' recipes into `git' recipes, and the `emacswiki' fetcher
could be a virtual fetcher that transforms `emacswiki' recipes
into `file' recipes with the appropriate `:url' property.

In addition, the following additional properties may be provided
for both real and virtual fetchers:

* `:auto-property': This function takes two arguments, RECIPE and
  PROPERTY, and should return an auto-generated value for that
  property. If no value can be auto-generated, it should return
  nil. It will be called to in order to fill in various metadata
  fields about the package. For example, this could be used to
  supply auto-generated `:description' or `:website'
  properties. This function will only be called for a property if
  a recipe does not explicitly specify that property, so there is
  no need to check if the requested property already exists.

* `:validate': This function only takes a RECIPE argument and
  should verify that the recipe is valid. It should return nil
  for a valid recipe. For an invalid recipe, it should return
  either a string or a list of strings that describe all the
  problems found with the recipe. For example, if the fetcher
  requires the recipe to have a `:url' property, then the
  `:validate' function should verify that the recipe has this
  property. If not, it could return the string \"recipe must
  have :url property\". The validation function may assume that
  the `:name' and `:type' properties have already been validated."
  (declare (indent defun))
  (el-get-set-fetcher (plist-get def :type) def))

(defun el-get-register-fetcher-alias (newtype oldtype)
  "Register NEWTYPE as an alias for OLDTYPE.

Any recipe whose `:type' property is NEWTYPE will then be treated
as it it was OLDTYPE instead.

This function actually registers NEWTYPE as a virtual fetcher
type whose filter function just changes the recipe's `:type'
property to OLDTYPE and leaves the rest of the recipe alone."
  (el-get-register-fetcher
   :type newtype
   :filter
   `(lambda (recipe)
      (el-get-recipe-put recipe :type ',oldtype))))

(provide 'el-get-fetcher-registry)
;;; el-get-fetcher-registry.el ends here

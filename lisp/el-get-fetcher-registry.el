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

(defvar el-get-fetchers (make-hash-table :size 20)
  "Hash table of registered package fetching methods.

TODO DOC")

(defun el-get-validate-fetcher-def (def)
  "Throw an error if DEF does not look like a valid fetcher definition."
  ;; Must be a hash table
  (unless (hash-table-p def)
    (error "Fetcher must be a hash table"))
  ;; Must have a :fetch or :filter
  (unless (or (gethash :fetch def)
              (gethash :filter def))
    (error "Fetcher must have a :fetch of :filter property"))
  ;; All properties must be functions
  (maphash
   (lambda (k v)
     (or (functionp v)
         (error "Fetcher's %s property must be a function" k)))
   def)
  t)

(defsubst el-get--get-fetcher (type)
  "Retrive full fetcher definition for TYPE.

Throws an error if the requested fetcher does not exist."
  (or (gethash type el-get-fetchers)
      ;; TODO: el-get-error macro
      (error "Fetcher type %s not registered" type)))

(defsubst el-get--set-fetcher (type def)
  "Set fetcher definition for TYPE to DEF.

Performs basic validation ot DEF before setting it."
  ;; Validate fetcher definition
  (el-get-validate-fetcher-def def)
  (puthash type def el-get-fetchers))

(defun el-get-fetcher-op (type operation)
  "Return the OPERATION function for fetcher type TYPE.

TYPE may either be the name of a fetcher type, the definition of
one, or a recipe, whose type property will be used. If the
operation is not defined for the given type, this returns nil. If
the given type is not a recognized recipe type, throws an
error. TODO"
  (cond
   ;; Fetcher definition
   ((hash-table-p type)
    (gethash operation type))
   ;; Symbol naming a fetcher
   ((symbolp type)
    (el-get-fetcher-op (el-get--get-fetcher type) operation))
   ;; String naming a fetcher
   ((stringp type)
    (el-get-display-warning "String used instead of symbol to name a type")
    (el-get-fetcher-op (intern type) operation))
   ;; Recipe
   ((listp type)
    (el-get-fetcher-op (el-get-recipe-type type) operation))
   ;; Unrecognized fetcher type
   ((null type)
    (error ""))))

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
  property. If no value can be auto-generated, it should return
  nil. It will be called to in order to fill in various metadata
  fields about the package. For example, this could be used to
  supply-auto-generated `:description' or `:website'
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
          do (error "Missing required keyword argument: :%s" required-key)
          do (puthash (intern (format ":%s" required-key))
                      value fetcher-def))
    (loop for optional-key in '(update remove compute-checksum guess-metadata)
          for value = (eval optional-key)
          if value
          do (puthash (intern (format ":%s" optional-key))
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

(provide 'el-get-fetcher-registry)
;;; el-get-fetcher-registry.el ends here

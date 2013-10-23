;;; el-get-dependencies.el --- Functions for computing package dependency lists

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

(require 'el-get-internals)
(require 'el-get-recipe-manip)
(require 'el-get-recipe-io)

(defun el-get-package-dependencies (recipe)
  "Return direct dependencies of RECIPE, a package definition."
  ;; TODO clarify via documentation and argnames which functions take
  ;; only full recipe definitions and which take whatever.
  (append (el-get-as-list (el-get-recipe-get recipe :depends))
          ;; TODO: make sure fetchers implement this
          (el-get-as-list (el-get-recipe-get recipe :auto-depends))))

(defun* el-get-dependency-graph (recipes &key (overrides nil) (skip-p #'ignore))
  "Return the dependency graph of RECIPES.

The dependency graph is a hash table where each key is a recipe
name and each value is the list of recipe names on which that
recipe depends.

OVERRIDES is an optional list of partial or full recipes meant to
override the default definitions.

SKIP-P is an optional predicate that should return non-nil for
any recipe that should be skipped while walking the dependency
graph. For example, if you only want a dependency graph of
not-yet-installed pacakges, you could use
`el-get-package-installed-p'."
  (let* (;; Convert OVERRIDES to a lookup (hash) table mapping
         ;; recipe names to full definitions.
         (overrides (el-get-make-recipe-override-table overrides))
         ;; Fully resolve RECIPES, taking overrides into account
         (recipes
          (loop for r in recipes
                collect (el-get-resolve-recipe r
                          :overrides overrides)))
         ;; Initialize dephash with RECIPES
         (dephash
          (el-get-make-lookup-table recipes
            :key-func #'el-get-recipe-name
            :value-func #'el-get-package-dependencies))
         (remaining-deps
          (mapcan #'el-get-package-dependencies recipes)))
    (loop while remaining-deps
          for next-pkg = (pop remaining-deps)
          if (not (or (gethash next-pkg dephash)
                      (funcall skip-p next-pkg))) do
                      (let* ((next-recipe
                              (el-get-resolve-recipe next-pkg
                                :overrides overrides))
                             (next-recipe-deps
                              (el-get-package-dependencies next-recipe)))
                        (puthash next-pkg
                                 next-recipe-deps
                                 dephash)
                        (setq remaining-deps
                              (append next-recipe-deps remaining-deps)))
                      finally return dephash)))

(defun el-get-extract-dependency-list (start graph)
  "Extract the full linear dependency list of START in GRAPH.

The list is returned, and all the elements of that list are
removed from GRAPH."
  (let ((deps (gethash start graph :notfound)))
    (prog1
        (cond
         ;; If START isn't present in GRAPH, return empty list (not even
         ;; just START). A package without no dependencies would be
         ;; present with a nil dependency list, not absent entirely.
         ((eq deps :notfound)
          nil)
         ;; Empty dependency list: return just start
         ((null deps)
          (list start))
         ;; Non-empty dependency list: recurse into all dependencies
         ((consp deps)
          (nconc
           (nconc
            ;; Recurse into all dependencies (if any).
            (mapcan (lambda (dep)
                      (el-get-extract-dependency-list dep graph))
                    deps)
            ;; And add START to the list as well, after its deps.
            (list start)))))
      ;; finally delete START from GRAPH
      (remhash start graph))))

(defun el-get-linearize-dependency-graph (graph &optional copy)
  "Convert a dependency graph into a linear list of dependencies.

The return list will be ordered such that each package's
dependencies all appear before it in the list. This means that if
the packages are installed in the order given by the return list,
there will be no dependency issues.

By default, GRAPH is emptied by a call to this function. With
optional arg COPY, the graph is instead copied and the original
is left intact."
  (when copy
    (setq graph (copy-hash-table graph)))
  (loop with deplist = nil
        while (> (hash-table-count graph) 0)
        for start = (el-get-random-hash-key graph)
        if start
        nconc (el-get-extract-dependency-list start graph)
        into deplist
        finally return deplist))

(defun el-get-dependency-list (recipes &rest el-get-dep-graph-args)
  "Return a list of all dependencies for RECIPES.

The return list will be ordered such that each package's
dependencies all appear before it in the list. This means that if
the packages are installed in the order given by the return list,
there will be no dependency issues.

Additional keyword arguments are passed to `el-get-dependency-graph'."
  (el-get-linearize-dependency-graph
   (apply #'el-get-dependency-graph recipes el-get-dep-graph-args)))

(provide 'el-get-dependencies)
;;; el-get-dependencies.el ends here

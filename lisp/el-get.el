;; Possible statuses: uninstalled, fetched, installed

;; Fetch queue: All fetches are independent but the queue should be
;; initialized in the order that we want to install so that
;; earlier-installed packages will not have to wait for later ones to
;; be fetched. Should have an option to rate-limit fetches and limit
;; the number of concurrent fetches. Also maybe limit to one fetch per
;; domain? If a fetch fails, it should be able to retry by adding
;; itself back to the front of the queue with an additional property
;; holding the number of retries. However, if too many fetches (to
;; different domains) in a row fail, el-get should assume there is no
;; internet connection and abort all fetches. Fetch will abort unless
;; status is uninstalled.

;; Install queue: Initialize queue in linearized dependency
;; order. Each package install will abort unless all its dependencies
;; are installed first. Install will abort if package is not already
;; fetched. Part of the install process should be checking if
;; el-get-init succeeds on this package in a subprocess. When a
;; package finishes installing, if it is in the init queue it should
;; be initialized.

;; Any time a package lacks a status file or otherwise has some
;; inconsistency, its status should be treated as
;; uninstalled. However, the package directory's contents should not
;; be cleaned until right before re-fetching.

;; Error messages should be collected in a dedicated hidden buffer
;; (like the *Messages* buffer) and at the end of the install process
;; el-get should print a summary of results to this buffer (e.g. how
;; many and which pacakges are in which status) and then display a
;; message telling user the command to show the buffer.

;; Init queue: There should be a "request init" function that either
;; inits an installed package immediately or adds an uninstalled
;; package to this queue so that it will be initialized after
;; installation. Only explicitly-requested pacakges are added to the
;; queue, not their dependencies. Of course, deps are initialized
;; before initializing a package.

;; Bonus points: implement a "trash" bin for uninstalled packages, and
;; a "trashed" status. Installing a trashed package will simply
;; un-trash it (configurable option).

;; For "synchronous" installation, just install pacakges one-by-one
;; using the async method and wait for each one before starting the
;; next. There is no "in-process" installation method.

;; System pacakge manager methods like apt, pacman, etc. will no
;; longer be supported, although they could be supported by preparing
;; an install script ("apt-get install A B C ...") and giving the user
;; a shortcut to run it.





;; Idea: async processes return a plist describing what they did and
;; whether they were successful, etc.; handler function parses the
;; resulting plist and takes appropriate action, e.g. after successful
;; fetch, move package from "install-blocked" queue to "install-ready"
;; queue. Write the handler functions first.


;; Terminology:

;; * Package:

;; * Recipe:

;; *

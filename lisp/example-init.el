;; This will serve as an example of how el-get might be used in a
;; user's init file, in such a way that it will auto-install el-get if
;; it isn't already installed, and then install and init all requested
;; packages. If there is no internet connection, it will just init the
;; currently-installed packages. If there is no internet and el-get
;; isn't installed, then just do nothing.

;; The idea is to write the new el-get to support the usage here.


;; Steps:

;; 1. Create ~/.emacs.d/

;; 2. Check for existence of ~/.emacs.d/el-get-setup.el and download
;; it if needed. If we don't already have it and can't download it,
;; warn and do nothing.

;; 3. Load the setup file, which will install el-get if it isn't
;; installed, and then load just the subset required to init packages.

;; 4. The user can then have a call to the main el-get function in
;; their init file to install and init their packages, or instead they
;; can customize a variable and have the requested packages installed
;; by the setter for that custom variable.

;; 5. If any packages need to be installed during #4, load the rest of
;; el-get so we can fetch and install packages.

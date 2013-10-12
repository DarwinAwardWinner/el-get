;;; el-get-lock.el --- Implements file and directory locking

;; Copyright (C) 2013  Ryan C. Thompson

;; Author: Ryan C. Thompson <rct@thompsonclan.org>
;; Keywords: extensions, files, processes, tools

;; LICENSE???

;;; Commentary:

;; This uses Emacs' file locking primitives to implement file-based
;; mutexes. It doesn't really have anything to do with the rest of
;; el-get, and other pacakges could conceivably use it. I may split it
;; into a separate package at some point.

;;; Code:

(require 'cl)

(defvar el-get-active-locks ()
  "List of buffers currently locking files.")

(defsubst el-get-locking-buffer-name (filename)
  "Return the buffer name used to lock FILENAME.

If FILENAME is not locked, the buffer name that would be used to
lock it is returned."
  (format " * El-get-lock: %s*" filename))

(defsubst el-get-locking-buffer (filename)
  "Return the buffer used to lock FILENAME."
  (get-buffer (el-get-locking-buffer-name filename)))

(defun el-get-setup-locking-buffer (filename)
  "Set up locking buffer for FILENAME and return it.

If a locking buffer already exists for FILENAME, return it.

This does not actually lock the file."
  (with-current-buffer (get-buffer-create (el-get-locking-buffer-name filename))
    ;; Set visited file name without
    (let ((bufname (buffer-name))
          (change-major-mode-with-file-name nil))
      (set-visited-file-name filename 'noquery)
      (rename-buffer bufname))
    (current-buffer)))

(defun el-get-lock-buffer-if-unlocked (&optional file)
  "Like `lock-buffer', but always yields without asking."
  (cl-flet ((ask-user-about-lock
             (file opponent)
             (signal 'file-locked (list file opponent))))
    (lock-buffer file)))

(defun el-get-locking-buffer-filename (&optional buf)
  "Return the filename that BUF would be used for locking.

If BUF is not an el-get locking buffer, return nil. If BUF is
nil, use current buffer. Note that a non-nil return value does
not imply that the buffer is *currently* locking the the file. It
only tells which file it *would* lock.

BUF can also be the name of a buffer."
  (save-match-data
    (let ((bufname
           (if (stringp buf)
               buf
             (buffer-name (or buf (current-buffer)))))
          (regexp
           (concat "\\`" ;start
                   (regexp-quote " *El-get-lock: ")
                   "\\(.*\\)" ; filename
                   (regexp-quote "*")
                   "\\'")))
      (when (string-match regexp bufname)
        (match-string 1 bufname)))))
(defalias 'el-get-locking-buffer-p 'el-get-locking-buffer-filename)

(defun* el-get-acquire-file-lock (filename &optional wait (interval 0.1))
  "Attempt to acquire the lock on FILENAME.

This returns the buffer used to lock FILENAME.

If the file is already locked by the current process, this does
nothing and returns the locking buffer.

If the file is already locked by another process, the behavior
depends on the value of WAIT:

  * If WAIT is nil, this signals a `file-locked' error.

  * If WAIT is numeric, this waits for that number of seconds for
    the lock to become available. If it becomes available in that
    time, this will lock the file and return. Otherwise it
    signals a `file-locked' error.

  * If WAIT is any other non-nil value, this waits indefinitely
    for the lock to become available, and then returns.

For any non-nil value of WAIT, INTERVAL determines how frequently
Emacs checks the lock for availability. The default is 0.1
seconds between checks."
  (if wait
      (condition-case nil
          ;; First try at locking the file
          (el-get-acquire-file-lock filename)
        ;; If it's already locked, begin the waiting cycle.
        (file-locked
         (if (numberp wait)
             ;; Wait specified number of seconds, checking every
             ;; INTERVAL seconds.
             (let* ((wait-until-time
                     (time-add (current-time)
                               (seconds-to-time wait))))
               (loop while (time-less-p (current-time) wait-until-time)
                     do (sit-for interval)
                     for lockbuf =
                     (condition-case nil
                         (el-get-acquire-file-lock filename)
                       (file-locked nil))
                     if lockbuf return lockbuf
                     ;; Try one last time, without catching the error
                     finally return (el-get-acquire-file-lock filename)))
           ;; Wait non-numeric non-nil; wait forever
           (loop do (sit-for interval)
                 for lockbuf =
                 (condition-case nil
                     (el-get-acquire-file-lock filename)
                   (file-locked nil))
                 if lockbuf
                 return lockbuf))))
    ;; No waiting, try to lock the locking buffer.
    (with-current-buffer (el-get-setup-locking-buffer filename)
      (set-buffer-modified-p t)
      (el-get-lock-buffer-if-unlocked filename)
      (setq el-get-active-locks (cons (current-buffer) el-get-active-locks))
      (current-buffer))))

(defun el-get-release-file-lock (filename)
  "Release any lock held on FILENAME.

If FILENAME is not locked by the current Emacs process, this does
nothing.

FILENAME can also be a locking buffer, in which case the
asociated file will be unlocked."
  (let ((lockbuf
         (if (bufferp filename)
             (when (el-get-locking-buffer-p filename) filename)
           (el-get-locking-buffer filename))))
    (when lockbuf
      (with-current-buffer lockbuf
        (unlock-buffer)
        (set-buffer-modified-p nil)
        (setq el-get-active-locks (delq (current-buffer) el-get-active-locks))
        (kill-buffer (current-buffer))))))

(defun el-get-unlock-all-files ()
  "Unlock all files locked by this process.

Use only in emergencies."
  ;; TODO lock-related debug messages
  (while el-get-active-locks
    (el-get-release-file-lock (car el-get-active-locks))))

(defun el-get-holding-file-lock (filename)
  "Returns t if current Emacs holds the lock to FILENAME.

If FILENAME is not locked or is locked by a different Emacs
process, returns nil."
  (eq (file-locked-p filename) t))

(defmacro el-get-with-file-lock (filename &rest body)
  "Execute BODY while holding the lock on FILENAME.

FILENAME may be string, or it may be a list of arguments
`(FILENAME &optional wait interval)' that will be passed directly
to `el-get-acquire-file-lock', which can be used to specify a
timeout.

Signals a `file-locked' error if the lock cannot be acquired.

If current process already holds the lock to FILENAME, this is
equivalent to `(progn BODY)' (i.e. it will *not* release the lock
after executing BODY).

BODY should not invoke any functions that might cause the lock on
FILENAME to be released."
  (let* ((filename (eval filename))
         (arglist (if (listp filename) filename (list filename)))
         (filename (car arglist)))
    (unless (stringp filename)
      (error "Filename must be a string"))
    (if (el-get-holding-file-lock filename)
        `(progn ,@body)
      `(unwind-protect
           (progn
             (el-get-acquire-file-lock ,@arglist)
             ,@body)
         (el-get-release-file-lock ,filename)))))
(put 'el-get-with-file-lock 'lisp-indent-function 1)

(provide 'el-get-lock)
;;; el-get-lock.el ends here

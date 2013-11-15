;;; el-get-download.el --- Facilities for downloading politely

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

;; This code primarily provides a function `el-get-download-file'
;; which downloads a file, but attempts to avoid overloading hosts by
;; preventing multiple simultaneous downloads from the same host.

;;; Code:

(require 'el-get-internals)
(require 'el-get-lock)

(defcustom el-get-host-timestamp-directory
  (file-name-as-directory
   (concat user-emacs-directory "el-get-host-timestamp"))
  "Directory used to store download timestamp files for el-get."
  :group 'el-get)

(defconst el-get-download-default-wait 2
  "Number of seconds to wait between downloads from the same host.

This is used for hosts that have no entry in
`el-get-download-wait-alist'.")

(defcustom el-get-download-wait-alist
  '(("repo\\.or\\.cz" . 2)
    ("emacswiki\\.org" . 2)
    ;; Github can handle it (we're still not downloading multiple
    ;; things concurrently).
    ("github\\.com" . 0))
  "Wait times between successive downloads from specific hosts.

This is an alist with regexps as keys and numbers as values. Any
host that matches the regexp in a key uses the value as its
inter-download wait time.

Generally users should not need to customize this."
  :type '(alist :key-type (regexp :tag "Host regexp")
                :value-type (number :tag "Seconds to wait between downloads"
                                    :value 2.0)))

(defun el-get-wait-time-for-host (host)
  "Return the time to wait between successive downloads from HOST.

This is looked up in `el-get-download-wait-alist', using the
default of `el-get-download-default-wait' for hosts that don't
match any key."
  (or (cdr (el-get-assoc-regexp host el-get-download-wait-alist))
      el-get-download-default-wait))

(defsubst el-get-url-host (url)
  "Return host for URL.

This is like `url-host', but it also works on strings, which are
parsed into url structs."
  (url-host (el-get-as-url-struct url)))

(defsubst el-get-host-timestamp-file (host)
  "Return download timestamp file for HOST.

This file is used to record the first time at which the next
download may be initiated from HOST."
  (concat (file-name-as-directory el-get-host-timestamp-directory)
          (format "timestamp-%s.txt" (file-name-nondirectory host))))

(defun el-get-read-timestamp-file (filename)
  "Read timestamp from FILENAME.

The time stamp is returned as a float number of seconds since the
epoch, like what `float-time' returns. It represents the first
time at which a download

If FILENAME does not exist or does not appear to contain a time
stamp, return nil."
  (let ((timestamp
         (condition-case nil
             (el-get-read-from-file filename)
           (error 'no-timestamp))))
    (cond
     ;; No/invalid timestamp file
     ((eq timestamp 'no-timestamp)
      nil)
     ;; Float
     ((numberp timestamp)
      timestamp)
     ;; List of integers (like `(current-time)'
     ((consp timestamp)
      (float-time timestamp))
     ;; Anything else is invalid
     (t
      (el-get-warning-message
       "File %s does not contain a valid time specification: %S"
       filename timestamp)
      nil))))

(defun el-get-write-timestamp-file (filename &optional timestamp)
  "Write TIMESTAMP to FILENAME. Default timestamp is current time."
  (when timestamp
    (unless (numberp timestamp)
      (el-get-error
       "Timestamp must be a floating point value.")))
  (with-temp-buffer
    (insert
     (el-get-print-to-string
      (or timestamp)
      (float-time)))
    (el-get-write-file filename)))

(defun el-get-download-file (url newname &optional num-attempts predicate)
  "Download URL to NEWNAME synchronously.

This function attempts to prevent multiple simultaneous downloads
from the same host. If two Emacs processes call this function on
files from the same host, one of them will wait until a set
amount of time after the other one finishes its download (see
`el-get-download-wait-alist').

With optional argument NUM-ATTEMPTS, if the download fails, it
will be attempted up to that many times before giving up. If
NUM-ATTEMPTS is provided, it must be a natural number.

With optional arg PREDICATE, a download is only considered
successful if PREDICATE returns non-nil when called on the
downloaded file."
  ;; Not our job to create the directory, and we don't want to do all
  ;; the work below if it doesn't exist.
  (unless (file-directory-p (file-name-directory newname))
    (el-get-error "Cannot download to nonexistent directory %s"
                  (file-directory-p (file-name-directory newname))))
  (let* ((host (el-get-url-host url))
         (timestamp-file (el-get-host-timestamp-file host))
         (wait-time (el-get-wait-time-for-host host)))
    (el-get-ensure-directory el-get-host-timestamp-directory)
    (el-get-with-file-lock timestamp-file
      ;; Read the previous timestamp, if any
      (let ((timestamp (el-get-read-timestamp-file timestamp-file)))
        ;; Wait for the required delay.
        (el-get-sleep-until-time timestamp)
        ;; Make sure num-attempts has a sane value
        (setq num-attempts (max (floor (or num-attempts 1)) 1))
        (unwind-protect
            (loop with success = nil
                  for attempt-num upfrom 1
                  for remaining-attempts = (- num-attempts attempt-num)
                  while (< attempt-num num-attempts)
                  ;; Try to fetch
                  do (condition-case err
                         (progn
                           (el-get-debug-message
                            "Attempt #%s to download %s to %s"
                            attempt-num url newname)
                           (url-copy-file url newname 'overwrite)
                           (setq success t))
                       ;; TODO: Figure out which error types indicate a
                       ;; persistent failure for which we should cancel
                       ;; all retries.
                       (error
                        (el-get-debug-message
                         "Error during attempt #%s to download %s to %s: %S"
                         attempt-num url newname err)
                        (setq success nil)))
                  if (and success predicate)
                  do (setq
                      success
                      (if (funcall predicate newname)
                          (prog1 t
                            (el-get-debug-message
                             "Predicate %s passed on file %s downloaded from URL %S."
                             predicate newname url))
                        (prog1 nil
                          (el-get-debug-message
                           "Predicate %s failed on file %s downloaded from URL %S."
                           predicate newname url)
                          (delete-file newname))))
                  if success return success
                  else do
                  (el-get-debug-message
                   "Failed attempt #%s to download %s to %s. Trying again %s times"
                   attempt-num url newname remaining-attempts)
                  ;; Sleep between attempts too
                  do (sleep-for wait-time)
                  ;; Final attempt: throws the error for real if it
                  ;; fails.
                  finally return
                  (progn
                    (el-get-debug-message
                     "Final attempt #%s to download %s to %s."
                     num-attempts url newname)
                    (url-copy-file url newname 'overwrite)
                    (when predicate
                      (unless (funcall predicate newname)
                        (el-get-error
                         "Predicate %s failed on file %s downloaded from URL %S."
                         predicate newname url))))))
        ;; Write the timestamp file whether we succeed or fail, so
        ;; other processes know how long to wait.
        (el-get-write-timestamp-file
         timestamp-file
         (+ (float-time) wait-time))))))

(provide 'el-get-download)
;;; el-get-download.el ends here

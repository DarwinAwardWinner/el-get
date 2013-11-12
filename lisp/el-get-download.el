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

(defcustom el-get-download-wait-alist nil
  "Wait times between successive downloads from specific hosts."
  :type '(alist :key-type (string :tag "Host")
                :value-type (number :tag "Wait time (seconds)"
                                    :value 2.0)))

(defun el-get-wait-time-for-host (host)
  "Return the time to wait between "
  (or (cdr (assoc host el-get-download-wait-alist))
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
          (format "timestamp-%s.el" (file-name-nondirectory host))))

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
      (el-get-display-warning
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

(defun el-get-download-file (url newname &optional num-attempts)
  "Download URL to NEWNAME synchronously.

This function attempts to prevent multiple simultaneous downloads
from the same host. If two Emacs processes call this function on
files from the same host, one of them will wait until a set
amount of time after the other one finishes its download (see
`el-get-download-wait-alist').

With optional argument NUM-ATTEMPTS, if the download fails, it
will be attempted up to that many times before giving up. If
NUM-ATTEMPTS is provided, it must be a natural number.

The return value is that of the last call to `url-copy-file',
successful or not."
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
            (loop with remaining-attempts = num-attempts
                  while (> remaining-attempts 1)
                  for attempt-num = (1+ (- num-attempts remaining-attempts))
                  do (el-get-debug-message
                      "Attempt #%s to download %s to %s"
                      attempt-num url newname)
                  ;; Try to fetch
                  for result =
                  (condition-case err
                      (url-copy-file url newname 'overwrite)
                    ;; TODO: Figure out which error types indicate a
                    ;; persistent failure for which we should cancel
                    ;; all retries.
                    (error
                     (el-get-debug-message
                      "Error during attempt #%s to download %s to %s: %S"
                      attempt-num url newname err)))
                  if result return result
                  ;; Sleep between attempts too
                  do (sleep-for wait-time)
                  do (decf remaining-attempts)
                  ;; Final attempt: throws the error for real if it
                  ;; fails.
                  finally return
                  (progn
                    (el-get-debug-message "Final attempt to download %s to %s"
                                          url newname)
                    (url-copy-file url newname 'overwrite)))
          ;; Write the timestamp file whether we succeed or fail, so
          ;; other processes know how long to wait.
          (el-get-write-timestamp-file
           timestamp-file
           (+ (float-time) wait-time)))))))

(provide 'el-get-download)
;;; el-get-download.el ends here

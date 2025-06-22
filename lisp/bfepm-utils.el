;;; bfepm-utils.el --- BFEPM Utility functions -*- lexical-binding: t -*-

;;; Commentary:

;; Generic utility functions for BFEPM.
;; This module contains only domain-agnostic utilities.

;;; Code:

(defun bfepm-utils-message (format-string &rest args)
  "Display a formatted message with BFEPM prefix.
FORMAT-STRING is the format string, ARGS are the arguments."
  (message "[BFEPM] %s" (apply #'format format-string args)))

(defun bfepm-utils-error (format-string &rest args)
  "Signal an error with BFEPM prefix.
FORMAT-STRING is the format string, ARGS are the arguments."
  (error "[BFEPM] %s" (apply #'format format-string args)))

(defun bfepm-utils-ensure-directory (dir)
  "Ensure directory DIR exists, creating it if necessary."
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun bfepm-utils-copy-file (source target)
  "Copy SOURCE file to TARGET."
  (copy-file source target t))

(defun bfepm-utils-file-checksum (file)
  "Calculate SHA256 checksum of FILE."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents-literally file)
      (secure-hash 'sha256 (current-buffer)))))

(defun bfepm-utils-verify-checksum (file expected-checksum)
  "Verify FILE against EXPECTED-CHECKSUM.
Returns t if checksum matches, nil otherwise."
  (when (and file expected-checksum (file-exists-p file))
    (let ((actual-checksum (bfepm-utils-file-checksum file)))
      (string= (downcase actual-checksum) (downcase expected-checksum)))))

(provide 'bfepm-utils)

;;; bfepm-utils.el ends here
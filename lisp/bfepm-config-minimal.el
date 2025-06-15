;;; bfepm-config-minimal.el --- BFEPM Configuration management (minimal version) -*- lexical-binding: t -*-

;;; Commentary:

;; Minimal configuration file handling for BFEPM without TOML dependency.

;;; Code:

(require 'bfepm-core)
(require 'bfepm-utils)

(defvar bfepm-config--default-sources
  `(("melpa" . ,(make-bfepm-source
                 :name "melpa"
                 :url "https://melpa.org/packages/"
                 :type "elpa"
                 :priority 10))
    ("gnu" . ,(make-bfepm-source
               :name "gnu"
               :url "https://elpa.gnu.org/packages/"
               :type "elpa"
               :priority 5))
    ("melpa-stable" . ,(make-bfepm-source
                        :name "melpa-stable"
                        :url "https://stable.melpa.org/packages/"
                        :type "elpa"
                        :priority 7)))
  "Default package sources.")

(defun bfepm-config-create-default ()
  "Create a default BFEPM configuration."
  (make-bfepm-config
   :packages nil
   :sources bfepm-config--default-sources))

(defun bfepm-config-validate (config)
  "Validate BFEPM configuration structure.
CONFIG is the configuration structure to validate."
  (unless (bfepm-config-p config)
    (bfepm-utils-error "Invalid configuration structure"))
  
  ;; Validate required fields
  (unless (bfepm-config-sources config)
    (bfepm-utils-error "No package sources defined"))
  
  t)

(defun bfepm-config-get-package (config package-name)
  "Get package specification for PACKAGE-NAME from CONFIG."
  (cl-find package-name (bfepm-config-packages config)
           :key #'bfepm-package-name :test #'string=))

(defun bfepm-config-get-source (config source-name)
  "Get source specification for SOURCE-NAME from CONFIG."
  (alist-get source-name (bfepm-config-sources config) nil nil #'string=))

(defun bfepm-config-load (file)
  "Load BFEPM configuration from FILE (minimal version with basic parsing)."
  (bfepm-utils-message "Loading configuration from %s (minimal parser)" file)
  (if (file-exists-p file)
      (let ((config (bfepm-config-create-default))
            (packages '()))
        ;; Basic parsing for git packages
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward "^\\([a-zA-Z0-9_-]+\\)\\s-*=\\s-*{\\s-*git\\s-*=\\s-*\"\\([^\"]+\\)\"\\(.*\\)}" nil t)
            (let* ((package-name (match-string 1))
                   (git-url (match-string 2))
                   (rest (match-string 3))
                   (branch nil)
                   (tag nil)
                   (ref nil))
              ;; Parse branch, tag, ref
              (when (string-match "branch\\s-*=\\s-*\"\\([^\"]+\\)\"" rest)
                (setq branch (match-string 1 rest)))
              (when (string-match "tag\\s-*=\\s-*\"\\([^\"]+\\)\"" rest)
                (setq tag (match-string 1 rest)))
              (when (string-match "ref\\s-*=\\s-*\"\\([^\"]+\\)\"" rest)
                (setq ref (match-string 1 rest)))
              
              ;; Create git source
              (let ((git-source (list :url git-url :type "git")))
                (when branch (setq git-source (plist-put git-source :ref branch)))
                (when tag (setq git-source (plist-put git-source :ref tag)))
                (when ref (setq git-source (plist-put git-source :ref ref)))
                
                ;; Create package entry
                (push (make-bfepm-package
                       :name package-name
                       :version (or branch tag ref "latest")
                       :source git-source
                       :status 'required)
                      packages)))))
        
        ;; Update config with parsed packages
        (setf (bfepm-config-packages config) packages)
        config)
    (bfepm-config-create-default)))

(defun bfepm-config-save (_config file)
  "Save CONFIG to FILE (minimal version - create basic template)."
  (bfepm-utils-message "Saving configuration to %s (minimal version)" file)
  (with-temp-buffer
    (insert "# BFEPM Configuration File (minimal template)\n")
    (insert "# For full TOML support, install the toml.el package\n\n")
    (insert "[meta]\n")
    (insert "version = \"1.0.0\"\n\n")
    (insert "[sources]\n")
    (insert "melpa = { url = \"https://melpa.org/packages/\", type = \"elpa\", priority = 10 }\n")
    (insert "gnu = { url = \"https://elpa.gnu.org/packages/\", type = \"elpa\", priority = 5 }\n\n")
    (insert "[packages]\n")
    (insert "# Add your packages here\n")
    (insert "# company = \"latest\"\n")
    (insert "# magit = \"^3.3\"\n")
    (write-file file)))

(provide 'bfepm-config-minimal)

;;; bfepm-config-minimal.el ends here
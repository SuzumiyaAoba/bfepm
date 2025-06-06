;;; epm-config-minimal.el --- EPM Configuration management (minimal version) -*- lexical-binding: t -*-

;;; Commentary:

;; Minimal configuration file handling for EPM without TOML dependency.

;;; Code:

(require 'epm-core)
(require 'epm-utils)

(defvar epm-config--default-sources
  '(("melpa" . ((url . "https://melpa.org/packages/")
                (type . "elpa")
                (priority . 10)))
    ("gnu" . ((url . "https://elpa.gnu.org/packages/")
              (type . "elpa")
              (priority . 5)))
    ("melpa-stable" . ((url . "https://stable.melpa.org/packages/")
                       (type . "elpa")
                       (priority . 7))))
  "Default package sources.")

(defun epm-config-create-default ()
  "Create a default EPM configuration."
  (make-epm-config
   :packages nil
   :sources epm-config--default-sources))

(defun epm-config-validate (config)
  "Validate EPM configuration structure."
  (unless (epm-config-p config)
    (epm-utils-error "Invalid configuration structure"))
  
  ;; Validate required fields
  (unless (epm-config-sources config)
    (epm-utils-error "No package sources defined"))
  
  t)

(defun epm-config-get-package (config package-name)
  "Get package specification for PACKAGE-NAME from CONFIG."
  (cl-find package-name (epm-config-packages config)
           :key #'epm-package-name :test #'string=))

(defun epm-config-get-source (config source-name)
  "Get source specification for SOURCE-NAME from CONFIG."
  (alist-get source-name (epm-config-sources config) nil nil #'string=))

(defun epm-config-load (file)
  "Load EPM configuration from FILE (minimal version - returns default config)."
  (epm-utils-message "Loading configuration from %s (minimal parser)" file)
  ;; Minimal version just returns default config
  ;; A more sophisticated version could implement basic TOML parsing
  (epm-config-create-default))

(defun epm-config-save (config file)
  "Save CONFIG to FILE (minimal version - creates basic template)."
  (epm-utils-message "Saving configuration to %s (minimal version)" file)
  (with-temp-buffer
    (insert "# EPM Configuration File (minimal template)\n")
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

(provide 'epm-config-minimal)

;;; epm-config-minimal.el ends here
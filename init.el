;;; init.el -- pigbug entry point

;;; Commentary:
;; Sets up load pathing and does good work. As all of this,
;; inspired by prelude

;;; Code:

(defvar pigbug-root-dir (file-name-directory load-file-name)
  "Root dir of the pigbug.")
(defvar pigbug-core-dir (expand-file-name "core" pigbug-root-dir)
  "Core elisp for the pigbug.")
(defvar pigbug-modules-dir (expand-file-name "modules" pigbug-root-dir)
  "Assorted elisp for the pigbug.")
(defvar pigbug-save-dir (expand-file-name "saves" pigbug-root-dir)
  "Savefile dir for the pigbug.")
(defvar pigbug-backup-dir (expand-file-name "backups" pigbug-save-dir)
  "Backup dir for the pigbug")
(defvar pigbug-auto-save-dir (expand-file-name "auto" pigbug-save-dir)
  "Autosave dir for the pigbug")

(defun pigbug-ensure-path (path)
  "Ensure that PATH exists."
  (unless (file-exists-p path)
    (make-directory path)))

(pigbug-ensure-path pigbug-save-dir)

(add-to-list 'load-path pigbug-root-dir)
(add-to-list 'load-path pigbug-core-dir)
(add-to-list 'load-path pigbug-modules-dir)

;; Core
(require 'pigbug-packages)
(require 'pigbug-core)
(require 'pigbug-editing)
(require 'pigbug-email)

;; Lang specific
(require 'pigbug-erlang)
(require 'pigbug-python)
(require 'pigbug-etc')
(require 'pigbug-c)
(require 'pigbug-clojure)
(require 'pigbug-html)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("d63e19a84fef5fa0341fa68814200749408ad4a321b6d9f30efc117aeaf68a2e" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

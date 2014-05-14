;;; init.el -- pigbug entry point

;;; Commentary:
;; Sets up load pathing and does good work. As all of this,
;; inspired by prelude

;;; Code:

(setenv "EDITOR" "emacsclient")
(setenv "GIT_EDITOR" "emacsclient")

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
(require 'pigbug-magit)
(require 'pigbug-eshell)

;; Lang specific
(require 'pigbug-erlang)
(require 'pigbug-python)
(require 'pigbug-etc)
(require 'pigbug-c)
(require 'pigbug-clojure)
(require 'pigbug-html)
(require 'pigbug-js)
(require 'pigbug-sass)


;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" default)))
 '(virtualenv-root "~/.virtualenvs"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

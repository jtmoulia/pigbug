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
(require 'pigbug-etc)
(require 'pigbug-c)
(require 'pigbug-clojure)
(require 'pigbug-html)

;;; init.el ends here

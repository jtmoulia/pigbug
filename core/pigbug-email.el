;;; pigbug-core.el --- Core pigbug setup
;; -*- mode: utf-8 -*-

;;; Commentary:

;; Core setup: initialize the appropriate minor modes
;; Prelude, you get more credit

(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-startttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials (expand-file-name "~/.authinfo")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(require 'starttls)
(require 'smtpmail)

(provide 'pigbug-email)
;;; Code:

;;; pigbug-email.el --- Pigbug email setup -- jtmoulai _at_ pocketknife.io
;; -*- mode: utf-8 -*-

;;; Commentary:

;; Email setup: mu4e

;;; Code:

;; mu4e
;; This is a bit of a hack to use mu4e from homebrew
;; TODO: have something outside of emacs symlink mu4e/ into .emacs.d
(add-to-list 'load-path "/usr/local/Cellar/mu/HEAD/share/emacs/site-lisp/mu4e/")
(require 'mu4e)

;; adds unread/total stats to the mu4e main page -- useful, but slow
; (mu4e-maildirs-extension)


;; general settings
(setq
 mu4e-mu/binary         "/usr/local/bin/mu"
 mu4e-maildir           "~/Mail"                ;; top-level Maildir
 mu4e-confirm-quit      nil
 mu4e-get-mail-command  "offlineimap"
 mu4e-headers-skip-duplicates t
 mu4e-update-interval   600
 mu4e-compose-signature "Thomas Moulia\nSkype: jtmoulia\n@jtmoulia"
 mu4e-compose-dont-reply-to-self t
 mu4e-compose-complete-only-personal t
 )

(setq mu4e-user-mail-address-list
      '("jtmoulia@pocketknife.io"
        "jtmoulia@gmail.com"))

;; maildir locations
(setq
 mu4e-sent-folder   "/pocketknife/INBOX.Sent Items"  ;; folder for sent messages
 mu4e-drafts-folder "/pocketknife/INBOX.Drafts"      ;; unfinished messages
 mu4e-trash-folder  "/pocketknife/INBOX.Trash"       ;; trashed messages
 mu4e-refile-folder "/pocketknife/INBOX.Archive"     ;; saved messages
 )

;; setup evil to play nice with mu4e
(progn
  (add-to-list 'evil-emacs-state-modes 'mu4e-headers-mode)
  (add-to-list 'evil-emacs-state-modes 'mu4e-main-mode))

;; Drafts are saved as *message*-___
(add-to-list 'auto-mode-alist '("\\*message\\*-+" . message-mode))

(setq user-full-name "Thomas Moulia"
      user-mail-address "jtmoulia@pocketknife.io"
      message-auto-save-directory "~/Mail/drafts")


;; list of default spam folders
(setq pigbug-spams
      '("maildir:/gmail/[Gmail].spam"))

(defun pigbug-mu4e-join-spam (query &optional spams separator)
  "Modify the mu4e QUERY to include SPAMS folders using SEPARATOR."
  (let ((spams (if spams spams pigbug-spams))
        (separator (if separator separator " AND NOT ")))
    (concat query
            (apply 'concat (mapcar (lambda (spam)
                                     (concat separator spam))
                                   spams)))))

;; mu4e bookmarks -- this is the magic
(setq mu4e-bookmarks
      `((,(pigbug-mu4e-join-spam "flag:unread AND NOT flag:trashed")
         "Unread messages" ?u)
        (,(pigbug-mu4e-join-spam "date:today..now") "Today's messages" ?t)
        (,(pigbug-mu4e-join-spam "date:7d..now") "Last 7 days" ?w)
        (,(pigbug-mu4e-join-spam "mime:image/*") "Messages with images" ?p)
        (,(pigbug-mu4e-join-spam "flag:unread AND NOT flag:trashed"
                                 pigbug-spams
                                 " AND ")
         "Unread spam" ?s)
        ))

;; sending mail
(setq
 send-mail-function 'smtpmail-send-it
 message-send-mail-function 'smtpmail-send-it
 smtpmail-stream-type 'ssl
 smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
 )

(setq pigbug-mu4e-account-alist
      '(("Pocketknife"
         (user-mail-address "jtmoulia@pocketknife.io")
         (mu4e-sent-folder  "/pocketknife/INBOX.Sent Items")
         (mu4e-drafts-folder  "/pocketknife/INBOX.Drafts")
         (smtpmail-default-smtp-server "mail.messagingengine.com")
         (smtpmail-smtp-server "mail.messagingengine.com")
         (smtpmail-smtp-service 465))
        ("Gmail"
         (user-mail-address "jtmoulia@gmail.com")
         (mu4e-sent-folder  "/gmail/[Gmail].Sent Mail")
         (mu4e-drafts-folder  "/gmail/[Gmail].Drafts")
         (smtpmail-default-smtp-server "smtp.gmail.com")
         (smtpmail-smtp-server "smtp.gmail.com")
         (smtpmail-smtp-service 465))))

(defun pigbug-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                pigbug-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) pigbug-mu4e-account-alist)
                             nil t nil nil (caar pigbug-mu4e-account-alist))))
         (account-vars (cdr (assoc account pigbug-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

(add-hook 'mu4e-compose-pre-hook 'pigbug-mu4e-set-account)


(require 'smtpmail)

(provide 'pigbug-email)
;;; pigbug-email.el ends here

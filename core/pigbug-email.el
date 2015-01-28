;;; pigbug-email.el --- Pigbug email setup -- jtmoulia _at_ pocketknife.io
;; -*- mode: utf-8 -*-

;;; Commentary:

;; Email setup: mu4e

;;; Code:

;; mu4e
;; This is a bit of a hack to use mu4e from homebrew
(add-to-list 'load-path "/usr/local/Cellar/mu/HEAD/share/emacs/site-lisp/mu4e/")

(require 'mu4e)
(require 'smtpmail)

;; adds unread/total stats to the mu4e main page -- useful, but slow
; (mu4e-maildirs-extension)


;; general settings
(setq
 mu4e-mu-binary         "/usr/local/bin/mu"
 mu4e-maildir           "~/Mail"                ;; top-level Maildir
 mu4e-confirm-quit      nil
 mu4e-get-mail-command  "offlineimap"
 mu4e-headers-skip-duplicates t
 mu4e-update-interval   600
 mu4e-compose-signature "Thomas Moulia  \njtmoulia.github.io  \nSkype: jtmoulia  \n@jtmoulia  "
 mu4e-compose-dont-reply-to-self t
 mu4e-compose-complete-only-personal t
 mu4e-hide-index-messages t
 mu4e-sent-folder "/pocketknife/INBOX.Sent Items"
 mu4e-html2text-command "html2text --body-width=78")

(setq mu4e-user-mail-address-list
      '("jtmoulia@pocketknife.io"
        "jtmoulia@gmail.com"
	"thomas@spatch.co"))

;; org contacts (wip)
;; TODO - move this over to pigbug-org.el
(require 'org-contacts)
(add-to-list 'org-capture-templates
             '("c" "Contacts" entry (file "~/Dropbox/contacts.org")
               "* %(org-contacts-template-name)
  :PROPERTIES:
  :EMAIL: %(org-contacts-template-email)
  :END:"))

(setq mu4e-org-contacts-file
      (expand-file-name "~/Dropbox/contacts.org"))
(add-to-list 'mu4e-headers-actions
  '("org-contact-add" . mu4e-action-add-org-contact) t)
(add-to-list 'mu4e-view-actions
  '("org-contact-add" . mu4e-action-add-org-contact) t)

;; drafts are saved as *message*-___
(add-to-list 'auto-mode-alist '("\\*message\\*-+" . message-mode))

(setq user-full-name "Thomas Moulia"
      user-mail-address "jtmoulia@pocketknife.io"
      message-auto-save-directory "~/Mail/drafts")


;; sending mail
(setq
 send-mail-function 'smtpmail-send-it
 message-send-mail-function 'smtpmail-send-it
 smtpmail-stream-type 'ssl
 smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg"))


;; multiple accounts
(setq pigbug-mu4e-accounts-config
      '(("pocketknife"
         (user-mail-address   "jtmoulia@pocketknife.io")
         (mu4e-inbox-folder   "/pocketknife/INBOX")
         (mu4e-sent-folder    "/pocketknife/INBOX.Sent Items")
         (mu4e-drafts-folder  "/pocketknife/INBOX.Drafts")
         (mu4e-trash-folder   "/pocketknife/INBOX.Trash")
         (mu4e-refile-folder  "/pocketknife/INBOX.Archive")
         (smtpmail-default-smtp-server "mail.messagingengine.com")
         (smtpmail-smtp-server "mail.messagingengine.com")
         (smtpmail-smtp-service 465))
        ("gmail"
         (user-mail-address  "jtmoulia@gmail.com")
         (mu4e-inbox-folder  "/gmail/INBOX")
         (mu4e-sent-folder   "/gmail/[Gmail].Sent Mail")
         (mu4e-drafts-folder "/gmail/[Gmail].Drafts")
         (mu4e-trash-folder  "/gmail/[Gmail].Trash")
         (mu4e-refile-folder "/gmail/[Gmail].Archive")
         (smtpmail-default-smtp-server "smtp.gmail.com")
         (smtpmail-smtp-server "smtp.gmail.com")
         (smtpmail-smtp-service 465))
        ("spatch"
         (user-mail-address  "thomas@spatch.co")
         (mu4e-inbox-folder  "/spatch/INBOX")
         (mu4e-sent-folder   "/spatch/[Gmail].Sent Mail")
         (mu4e-drafts-folder "/spatch/[Gmail].Drafts")
         (mu4e-trash-folder  "/spatch/[Gmail].Trash")
         (mu4e-refile-folder "/spatch/[Gmail].Archive")
         (smtpmail-default-smtp-server "smtp.gmail.com")
         (smtpmail-smtp-server "smtp.gmail.com")
         (smtpmail-smtp-service 465))))

(defun pigbug-mu4e-msg-root (msg)
  "Return the root directory of the MSG's maildir."
  (let ((maildir (mu4e-message-field msg :maildir)))
    (string-match "/\\(.*?\\)/" maildir)
    (match-string 1 maildir)))

(defun pigbug-mu4e-account-property (account property &optional accounts)
  "For ACCOUNT, return a PROPERTY using ACCOUNTS."
  (let ((accounts (if accounts accounts pigbug-mu4e-accounts-config)))
    (cadr (assoc property (assoc account accounts)))))

(defun pigbug-mu4e-properties (property &optional accounts)
  "Return PROPERTY for ACCOUNTS."
  (let ((accounts (if accounts accounts pigbug-mu4e-accounts-config)))
    (mapcar (lambda (account) (cadr (assoc property (cdr account)))) accounts)))


;; list of default spam folders
(setq pigbug-spams
      '("maildir:/gmail/[Gmail].spam" "maildir:/spatch/[Gmail].spam"))

(defun pigbug-mu4e-join-spam (query &optional spams separator)
  "Modify the mu4e QUERY to include SPAMS folders using SEPARATOR."
  (let ((spams (if spams spams pigbug-spams))
        (separator (if separator separator " AND NOT ")))
    (concat query
            (apply 'concat (mapcar (lambda (spam)
                                     (concat separator spam))
                                   spams)))))

(defun pigbug-interpose-concat (sep list)
  "Interpose SEP into LIST and concatenate."
  (apply 'concat (-interpose sep list)))

(defun pigbug-add-maildir-prefix (maildir)
  "Add maildir: prefix to MAILDIR for mu queries."
  (concat "maildir:" maildir))

;; mu4e bookmarks -- this is the magic
(setq mu4e-bookmarks
      `((,(pigbug-mu4e-join-spam "flag:unread AND NOT flag:trashed")
         "Unread messages" ?u)
        (,(pigbug-mu4e-join-spam "date:today..now")
         "Today's messages" ?t)
        (,(pigbug-mu4e-join-spam "date:7d..now")
         "Last 7 days" ?w)
        (,(pigbug-interpose-concat
           " OR "
           (mapcar 'pigbug-add-maildir-prefix
                   (pigbug-mu4e-properties 'mu4e-inbox-folder)))
         "Messages in inboxes", ?i)
        (,(pigbug-mu4e-join-spam "mime:image/*")
         "Messages with images" ?p)
        (,(pigbug-mu4e-join-spam
           "flag:unread AND NOT flag:trashed"
                                 pigbug-spams
                                 " AND ")
         "Unread spam" ?s)
        ))

(defun pigbug-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if (and (not current-prefix-arg) mu4e-compose-parent-message)
              (pigbug-mu4e-msg-root mu4e-compose-parent-message)
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                pigbug-mu4e-accounts-config"/"))
                             (mapcar #'(lambda (var) (car var))
                                     pigbug-mu4e-accounts-config)
                             nil t nil nil (caar pigbug-mu4e-accounts-config))))
         (account-vars (cdr (assoc account pigbug-mu4e-accounts-config))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

;; select account's folder for refiling emails
(setq mu4e-refile-folder
      (lambda (msg)
        (pigbug-mu4e-account-property (pigbug-mu4e-msg-root msg)
                                      'mu4e-refile-folder)))

;; select account's folder trashing emails
(setq mu4e-trash-folder
      (lambda (msg)
        (pigbug-mu4e-account-property (pigbug-mu4e-msg-root msg)
                                      'mu4e-trash-folder)))

(add-hook 'mu4e-compose-pre-hook 'pigbug-mu4e-set-account)

(provide 'pigbug-email)
;;; pigbug-email.el ends here

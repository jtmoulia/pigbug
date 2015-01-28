;;; pigbug-irc.el --- IRC setup
;; -*- mode: utf-8 -*-

;;; Commentary:

;; Configuration for rcirc.

;;; Code:

(add-to-list 'rcirc-server-alist
             '("spatch.irc.slack.com"
               :port 6697
               :nick "thomas"
               :user-name "thomas"
               :full-name "Thomas Moulia"
               :encryption tls
               :channels ("#general" "#server")))

(add-to-list 'rcirc-authinfo
             '("spatch.irc.slack.com" nickserv "thomas" "spatch.c2MDrG7ODhNtL9EBy27v"))

(provide 'pigbug-irc)
;;; pigbug-irc.el ends here

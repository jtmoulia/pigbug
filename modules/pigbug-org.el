;;; pigbug-org.el --- org-mode config

;;; Commentary:
;; org-mode packages and configuration

;;; Code:


;; Add to the set of babel languages
(org-babel-do-load-languages
 'org-babel-load-languages (quote ((emacs-lisp . t)
                                    (sqlite . t)
                                    (python . t))))

(provide 'pigbug-org)
;;; pigbug-org.el ends here

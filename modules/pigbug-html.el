;;; pigbug-html.el --- HTML support

;;; Commentary:
;; HTML packages and configuration

;;; Code:

(pigbug-require-package 'web-mode)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(provide 'pigbug-html)
;;; pigbug-html.el ends here

;;; pigbug-markdown.el --- Markdown support

;;; Commentary:
;; Markdown configuration

;;; Code:

(add-hook 'markdown-mode 'auto-fill-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(provide 'pigbug-markdown)
;;; pigbug-markdown.el ends here

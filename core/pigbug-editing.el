;;; pigbug-editing.el --- Pigbug: Setup right proper editing

;;; Commentary:
;; Setup the pigbug edting experience

;;; Code:

;; Good parens
;(show-paren-mode +1)

;; Evil
(pigbug-require-package 'evil)
(evil-mode +1)

;; Flycheck
(pigbug-require-package 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; yasnippet
(pigbug-require-package 'yasnippet)
(yas-global-mode +1)

;; smartparens
(pigbug-require-package 'smartparens)
(smartparens-global-mode +1)
(show-smartparens-global-mode +1)
;; No highlighting
(set-variable 'sp-highlight-pair-overlay nil)
(set-variable 'sp-highlight-wrap-overlay nil)
;; bindings
(sp-pair "(" ")" :wrap "M-(")
(sp-pair "\"" "\"" :wrap "M-\"")
(sp-pair "{" "}" :wrap "M-{")

(define-key sp-keymap (kbd "M-DEL") 'sp-unwrap-sexp)
; barfnslurp
(define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "M-f") 'sp-backward-barf-sexp)
(define-key sp-keymap (kbd "M-b") 'sp-backward-slurp-sexp)

;; Backup and save everything to the same dir
(setq backup-directory-alist
      `((".*" . ,pigbug-backup-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,pigbug-auto-save-dir t)))

(provide 'pigbug-editing)
;;; pigbug-editing.el ends here

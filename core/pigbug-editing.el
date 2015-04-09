;;; pigbug-editing.el --- Pigbug: Setup right proper editing

;;; Commentary:
;; Setup the pigbug edting experience

;;; Code:

;; TODO set it as a global variable
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

;; Theme
(setq custom-safe-themes t) ; Dangerous, but zenburn is being a pain
(load-theme 'monokai)

;; Adjust ansi colors to work with a dark background
(setq ansi-color-names-vector
      ["gray40"            ;; black
       "tomato"            ;; red
       "green"             ;; green
       "gold1"             ;; yellow
       "DeepSkyBlue1"      ;; blue
       "MediumOrchid1"     ;; magenta
       "cyan"              ;; cyan
       "gray94"]           ;; white
      ansi-color-map (ansi-color-make-color-map))

;; Flycheck
(pigbug-require-package 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; yasnippet
(pigbug-require-package 'yasnippet)
(yas-global-mode +1)

;; smartparens
;(pigbug-require-package 'smartparens)
;(smartparens-global-mode +1)
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
;; Warning -- not autosaving
(setq auto-save-default nil)
(setq backup-directory-alist
      `((".*" . ,pigbug-backup-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,pigbug-auto-save-dir t)))
(setq auto-save-list-file-prefix
      pigbug-auto-save-dir)

(defun unfill-paragraph ()
  "Unfill paragraph at or after point."
  (interactive "*")
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil (region-active-p))))

(provide 'pigbug-editing)
;;; pigbug-editing.el ends here

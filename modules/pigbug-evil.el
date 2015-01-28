;;; pigbug-evil.el --- Pigbug: Setup evil mode, make it play nice

;;; Commentary:
;; Setup the pigbug evil experience

;;; Code:

;; Evil
;; load evil
(pigbug-require-package 'evil)

; config evil
(setq evil-want-fine-undo t
      evil-default-cursor t
      set-cursor-color "#ffa500")

;; start global evil-mode
(evil-mode +1)

;; mu4e evil config
;; from fromhttps://github.com/antono/emacs.d/blob/master/local/my-evil.el
(eval-after-load 'mu4e
  '(progn
     ;; use the standard bindings as a base
     (evil-make-overriding-map mu4e-view-mode-map 'normal t)
     (evil-make-overriding-map mu4e-main-mode-map 'normal t)
     (evil-make-overriding-map mu4e-headers-mode-map 'normal t)
     (evil-add-hjkl-bindings mu4e-view-mode-map 'normal
       "J" 'mu4e~headers-jump-to-maildir
       "j" 'evil-next-line
       "C" 'mu4e-compose-new
       "o" 'mu4e-view-message
       "Q" 'mu4e-raw-view-quit-buffer)
     ;; (evil-add-hjkl-bindings mu4e-view-raw-mode-map 'normal
     ;; "J" 'mu4e-jump-to-maildir
     ;; "j" 'evil-next-line
     ;; "C" 'mu4e-compose-new
     ;; "q" 'mu4e-raw-view-quit-buffer)
     (evil-add-hjkl-bindings mu4e-headers-mode-map 'normal
       "J" 'mu4e~headers-jump-to-maildir
       "j" 'evil-next-line
       "C" 'mu4e-compose-new
       "o" 'mu4e-view-message)
     (evil-add-hjkl-bindings mu4e-main-mode-map 'normal
       "J" 'mu4e~headers-jump-to-maildir
       "j" 'evil-next-line
       "RET" 'mu4e-view-message)))

(provide 'pigbug-evil)
;;; pigbug-evil.el ends here

;;; pigbug-clojure.el --- Clojure support

;;; Commentary:
;; Clojure packages and configuration

;;; Code:

(pigbug-require-package 'clojure-mode)
(pigbug-require-package 'rainbow-delimiters)

;; CIDER
(pigbug-require-package 'cider)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

(provide 'pigbug-clojure)
;;; pigbug-clojure.el ends here

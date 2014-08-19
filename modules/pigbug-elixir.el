;;; pigbug-elixir.el --- Elixir support

;;; Commentary:
;; Elixir packages and configuration

;;; Code:

(pigbug-require-package 'elixir-mode)

;; Tabs: Just say no
(add-hook 'elixir-mode-hook
	  '(lambda()
	     (set-variable 'indent-tabs-mode nil)
	     (setq tab-width 2)))

(provide 'pigbug-elixir)
;;; pigbug-elixir.el ends here

;;; pigbug-erlang.el --- Erlang support

;;; Commentary:
;; Erlang packages and configuration

;;; Code:

(pigbug-require-package 'erlang)

(add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))

;; Tabs: Just say no
(add-hook 'erlang-mode-hook '(lambda() (setq indent-tabs-mode nil)))

;; distel
(require 'distel)
(distel-setup)

(provide 'pigbug-erlang)
;;; pigbug-erlang.el ends here

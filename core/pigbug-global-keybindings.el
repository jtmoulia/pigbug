;;; pigbug-global-keybindings.el --- Global keybindings for pigbug.
;; -*- mode: utf-8 -*-

;;; Commentary: Global keybindings are set for all buffers.

;; Core setup:

;;; Code:

;; Window switching. (C-x o goes to the next window)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1))) ;; back one

(provide 'pigbug-global-keybindings)
;;; pigbug-global-keybindings.el ends here

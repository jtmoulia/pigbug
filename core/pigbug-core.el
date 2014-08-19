;;; pigbug-core.el --- Core pigbug setup
;; -*- mode: utf-8 -*-

;;; Commentary:

;; Core setup: initialize the appropriate minor modes
;; Prelude, you get more credit

;;; Code:

;; Update the path
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

(set-variable 'inhibit-startup-screen t)
(defalias 'yes-or-no-p 'y-or-n-p)

;; No tool bars or menu bars or scroll bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Automatically revert buffer when file is changed
(global-auto-revert-mode +1)
(set-variable 'auto-revert-verbose nil)

(ido-mode +1)

;; Projectile. Everywhere.
(projectile-mode +1)
(projectile-global-mode)

;; Show trailing whitespace + ☣tabs everywhere
(set-variable 'whitespace-style
      '(face
	tabs
	;spaces
	trailing
	;lines
	space-before-tab
	newline
	indentation
	;empty
	;space-after-tab
	;space-mark
	;tab-mark
	;newline-mark
	))
(global-whitespace-mode +1)

;;; Helpers
(defun pigbug-chomp (str)
      "Chomp leading and tailing whitespace from STR."
      (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                        (: (* (any " \t\n")) eos)))
                                ""
                                str))


(provide 'pigbug-core)
;;; pigbug-core.el ends here

;;; pigbug-core.el --- Core pigbug setup
;; -*- mode: utf-8 -*-

;;; Commentary:

;; Core setup: initialize the appropriate minor modes
;; Prelude, you get more credit

;;; Code:

(set-variable 'inhibit-startup-screen t)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Automatically revert buffer when file is changed
(global-auto-revert-mode +1)

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

(provide 'pigbug-core)
;;; pigbug-core.el ends here

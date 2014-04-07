;;; pigbug-sass.el --- sass support

;;; Commentary:
;; Sass packages for .scss and configuration

;;; Code:

(pigbug-require-package 'scss-mode)
(set-variable 'scss-compile-at-save nil)

(defun pigbug-standard-scss-env ()
  "Set up my standard scss env."
  (setq indent-tabs-mode nil)
  (set-variable 'css-indent-offset 2))

(defun pigbug-tab-scss-env ()
  "Set up a scss env tab based css env."
  (setq indent-tabs-mode t)
  (setq tab-width 2)
  (set-variable 'css-indent-offset 2))

(add-hook 'scss-mode-hook 'pigbug-standard-scss-env)

(provide 'pigbug-sass)
;;; pigbug-sass.el ends here

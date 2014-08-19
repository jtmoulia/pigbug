;;; pigbug-js.el --- js support

;;; Commentary:
;; Javascript env config

;;; Code:

(defun pigbug-standard-js-env ()
  "Set up my standard javascript env."
  (setq indent-tabs-mode nil)
  (setq js-indent-level 4))

(add-hook 'javascript-mode 'pigbug-standard-js-env)

(provide 'pigbug-js)
;;; pigbug-js.el ends here

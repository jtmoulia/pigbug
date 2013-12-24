;;; pigbug-packages.el --- Package management

;;; Commentary:

;; Functions and setup for dealing with packages.
;; Again, inspired by prelude

;;; Code:

(require 'package)
(require 'cl)

(add-to-list 'package-archives
	     ;'("melpa" . "http://melpa.milkbox.net/packages/")
	     '("marmalade" . "http://marmalade-repo.org/packages/")
	     t)

(setq package-user-dir (expand-file-name "elpa"))
(package-initialize)

(defvar pigbug-packages
  '(ack-and-a-half magit projectile flycheck
    solarized-theme)
  "A list of packages to ensure are isntalled at launch")

(defun pigbug-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (package-installed-p package)
    (package-install package)))

(defun pigbug-require-packages (packages)
  "Ensure PACKAGES are installed."
  (mapc #'pigbug-require-package pigbug-packages))

(package-refresh-contents)
(pigbug-require-packages pigbug-packages)

(provide 'pigbug-packages)
;;; prelude-packages.el ends here

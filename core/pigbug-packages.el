;;; pigbug-packages.el --- Package management

;;; Commentary:

;; Functions and setup for dealing with packages.
;; Again, inspired by prelude

;;; Code:

(require 'package)
(require 'cl)

(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/")
	     t)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/")
	     t)

(setq package-user-dir (expand-file-name "elpa" pigbug-root-dir))
(package-initialize)

(defvar pigbug-packages
  '(ack-and-a-half projectile flycheck zenburn-theme
    solarized-theme mu4e-maildirs-extension)
  "A list of packages to ensure are installed at launch")

(defun pigbug-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (package-installed-p package)
    (package-install package)))

(defun pigbug-require-packages (packages)
  "Ensure PACKAGES are installed."
  (mapc #'pigbug-require-package pigbug-packages))

(package-refresh-contents)
(pigbug-require-packages pigbug-packages)

;;; el-get

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(setq el-get-sources
  '(el-get
    distel
    cider))

(el-get 'sync)

(provide 'pigbug-packages)
;;; prelude-packages.el ends here

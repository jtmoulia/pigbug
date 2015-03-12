;;; pigbug-elixir.el --- Elixir support

;;; Commentary:
;; Elixir packages and configuration

;;; Code:

(pigbug-require-package 'elixir-mode)

(defun pigbug-elixir-hook ()
  "Elixir hook."
  (let ((width 2))
    (alchemist-mode +1)
    (set-variable 'indent-tabs-mode nil)
    (electric-indent-mode 0)
    (setq tab-width width)
    (setq evil-shift-width width)))

;; Tabs: Just say no
(add-hook 'elixir-mode-hook 'pigbug-elixir-hook)


(defun pigbug-elixir-open-file (filename)
  "Open FILENAME in other window and goto line."
  (interactive "sfilename: ")
  (let ((filename (match-string 1 filename))
        (line (string-to-number (match-string 2 filename))))
    (find-file-other-window filename)
    (goto-line line)
    (back-to-indentation)))


(defvar pigbug-elixir-open-alist
  '(("^\\(.*\\.exs?\\):\\([0-9]+\\):?" . pigbug-elixir-open-file))
  "The mapping of regexp to arg.")

(defun pigbug-elixir-open ()
  "Attempt to open the current elixir related word.

Example: test/mainframe/conversation/user_data_test.exs:29:"
  (interactive)
  (let ((string (pigbug-WORD-string))
        (data (match-data)))
    (unwind-protect
     (-each
         pigbug-elixir-open-alist
       (lambda (item)
         (let ((regex (car item))
               (func (cdr item)))
           (if (integerp (string-match regex string))
               (funcall func (match-string 0 string))))))
     (set-match-data data))))

(provide 'pigbug-elixir)
;;; pigbug-elixir.el ends here

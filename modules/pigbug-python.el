;;; pigbug-python.el --- Pytnon support

;;; Commentary:
;; Python packages and configuration

;;; Code:

;; Set Python path using homebrew
;; TODO add a homebrew flag
(setenv "PYTHONPATH" (concat
		      (pigbug-chomp (shell-command-to-string "brew --prefix"))
		      "/lib/python2.7/site-packages"))

;; Love ipython
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")


;; Python's Jedi Mind Tricks
(pigbug-require-package 'epc)
(pigbug-require-package 'auto-complete)
(pigbug-require-package 'jedi)
(setq jedi:setup-keys t)
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)


(provide 'pigbug-python)
;;; pigbug-python.el ends here

;;; pigbug-org.el --- org-mode config

;;; Commentary:
;; org-mode packages and configuration

;;; Code:
;(add-to-list 'org-modules 'habits)

;; org contacts (wip)
(require 'org-contacts)

(setq org-contacts-files '("~/Dropbox/org/contacts.org"))

(setq org-plantuml-jar-path (expand-file-name "plantuml.jar"
                                              pigbug-bin-dir))

;; Add to the set of babel languages

(org-babel-do-load-languages
 'org-babel-load-languages (quote ((emacs-lisp . t)
                                   (latex . t)
                                   (sqlite . t)
                                   (python . t)
                                   (plantuml . t)
                                   (mscgen . t)
                                   (ditaa . t)
                                   ; (elixir . t)
                                   )))


;; http://www.newartisans.com/2007/08/using-org-mode-as-a-day-planner/

(define-key mode-specific-map [?a] 'org-agenda)

(eval-after-load "org"
  '(progn
     (define-prefix-command 'org-todo-state-map)

     (define-key org-mode-map "\C-co" 'org-todo-state-map)

     (define-key org-todo-state-map "x"
       #'(lambda nil (interactive) (org-todo "CANCELLED")))
     (define-key org-todo-state-map "d"
       #'(lambda nil (interactive) (org-todo "DONE")))
     (define-key org-todo-state-map "f"
       #'(lambda nil (interactive) (org-todo "DEFERRED")))
     (define-key org-todo-state-map "l"
       #'(lambda nil (interactive) (org-todo "DELEGATED")))
     (define-key org-todo-state-map "s"
       #'(lambda nil (interactive) (org-todo "STARTED")))
     (define-key org-todo-state-map "w"
       #'(lambda nil (interactive) (org-todo "WAITING")))

     (define-key org-agenda-mode-map "\C-n" 'next-line)
     (define-key org-agenda-keymap "\C-n" 'next-line)
     (define-key org-agenda-mode-map "\C-p" 'previous-line)
     (define-key org-agenda-keymap "\C-p" 'previous-line)))

(define-key global-map "\C-cor" 'org-capture)

(defvar pigbug-todo-file "~/Dropbox/org/todo.org"
  "The path to the primary todo file.")

(defvar pigbug-notes-file "~/Dropbox/org/notes.org"
  "The path to the notes file.")

(defvar pigbug-contacts-file "~/Dropbox/contacts.org"
  "The path to the contacts file.")

(custom-set-variables
 '(org-agenda-files (list pigbug-todo-file))
 '(org-default-notes-file pigbug-notes-file)
 '(org-agenda-ndays 7)
 '(org-deadline-warning-days 14)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-reverse-note-order t)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-log-into-drawer t)
 '(org-agenda-custom-commands
   (quote (("d" todo "DELEGATED" nil)
	   ("c" todo "DONE|DEFERRED|CANCELLED" nil)
	   ("w" todo "WAITING" nil)
	   ("W" agenda "" ((org-agenda-ndays 21)))
	   ("A" agenda ""
	    ((org-agenda-skip-function
	      (lambda nil
		(org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
	     (org-agenda-ndays 1)
	     (org-agenda-overriding-header "Today's Priority #A tasks: ")))
	   ("u" alltodo ""
	    ((org-agenda-skip-function
	      (lambda nil
		(org-agenda-skip-entry-if (quote scheduled) (quote deadline)
					  (quote regexp) "\n]+>")))
	     (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-capture-templates
   '(("t" "Todo" entry (file+headline pigbug-todo-file "Tasks")
      "* TODO %?\n  %i\n  %a\n")
     ("n" "Note" entry (file+datetree pigbug-notes-file "Notes")
      "* %?\nEntered on %U\n  %i\n  %a")
     ("c" "Contact" entry (file "~/Dropbox/org/contacts.org")
      "* %(org-contacts-template-name)
  :PROPERTIES:
  :EMAIL: %(org-contacts-template-email)
  :END:"))))


(defun goto-marker (marker)
  "Go to MARKER."
  (switch-to-buffer (marker-buffer marker))
  (goto-char marker))

;; vcard helper

(defun pigbug-parse-vcf ()
  "Parse the VCFs."
  (interactive)
  (let ((start (search-forward "BEGIN:VCARD"))
        (end (search-forward "END:VCARD")))
    (vcard-parse-region)))

(provide 'pigbug-org)
;;; pigbug-org.el ends here

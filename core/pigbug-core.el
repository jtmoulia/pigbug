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

(setq load-prefer-newer t)

;; mode bar

(column-number-mode +1)

;; Automatically revert buffer when file is changed
(global-auto-revert-mode +1)
(set-variable 'auto-revert-verbose nil)

(ido-mode +1)

;; Helm mode
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

;; Projectile. Everywhere.
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)



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

;; Diminish those minor modes

(diminish 'undo-tree-mode)
(diminish 'whitespace-mode)
(diminish 'global-whitespace-mode)
(diminish 'projectile-mode)
; (diminish 'yas/minor-mode "γ")
; (diminish 'flycheck-mode " a")


;;; Helpers

(defun pigbug-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

(defun pigbug-git-root (path)
  "Return the git root relative to PATH."
  (locate-dominating-file path ".git"))

(defun pigbug-fix-timestamp-string (date-string)
  "Returns yyyy-mm-dd format of DATE-STRING

For examples:
 「Nov. 28, 1994」 ⇒ 「1994-11-28」
 「November 28, 1994」 ⇒ 「1994-11-28」
 「11/28/1994」 ⇒ 「1994-11-28」

Any “day of week”, or “time” info, or any other parts of the string, are discarded.

Modified from: `http://xahlee.org/emacs/elisp_parse_time.html'"
  (let ((date-string (replace-regexp-in-string "^ *\\(.+\\) *$" "\\1" date-string))
        (pad (lambda (n x)
               (format (concat "%0" (number-to-string n) "d")
                       (string-to-number x)))))
    (require 'parse-time)
    (cond
     ;; USA convention of [m]m/[d]d/yyyy
     ((string-match
       "^\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\)$"
       date-string)
      (concat
       (match-string 3 date-string) "-"
       (funcall pad 2 (match-string 1 date-string)) "-"
       (funcall pad 2 (match-string 2 date-string))))

     ((string-match
       "^\\([0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\)$"
       date-string)
      (concat
       (match-string 3 date-string) "-"
       (funcall pad 2 (match-string 1 date-string)) "-"
       (funcall pad 2 (match-string 2 date-string))))

     ;; some ISO 8601. yyyy-mm-dd
     ((string-match
       "^\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)$T[0-9][0-9]:[0-9][0-9]"
       date-string)
      (concat
       (match-string 1 date-string) "-"
       (match-string 2 date-string) "-"
       (match-string 3 date-string)))
     ((string-match
       "^\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)$"
       date-string)
      (concat
       (match-string 1 date-string) "-"
       (match-string 2 date-string) "-"
       (match-string 3 date-string)))
     ((string-match "^\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)$" date-string)
      (concat
       (match-string 1 date-string) "-"
       (match-string 2 date-string)))
     ((string-match "^\\([0-9][0-9][0-9][0-9]\\)$" date-string)
      (match-string 1 date-string))

     ;; else
     (t
      (progn
        (setq date-string
              (replace-regexp-in-string "January " "Jan. " date-string))
        (setq date-string
              (replace-regexp-in-string "February " "Feb. " date-string))
        (setq date-string
              (replace-regexp-in-string "March " "Mar. " date-string))
        (setq date-string
              (replace-regexp-in-string "April " "Apr. " date-string))
        (setq date-string
              (replace-regexp-in-string "May " "May. " date-string))
        (setq date-string
              (replace-regexp-in-string "June " "Jun. " date-string))
        (setq date-string
              (replace-regexp-in-string "July " "Jul. " date-string))
        (setq date-string
              (replace-regexp-in-string "August " "Aug. " date-string))
        (setq date-string
              (replace-regexp-in-string "September " "Sep. " date-string))
        (setq date-string
              (replace-regexp-in-string "October " "Oct. " date-string))
        (setq date-string
              (replace-regexp-in-string "November " "Nov. " date-string))
        (setq date-string
              (replace-regexp-in-string "December " "Dec. " date-string))

        (setq date-string (replace-regexp-in-string " 1st," " 1" date-string))
        (setq date-string (replace-regexp-in-string " 2nd," " 2" date-string))
        (setq date-string (replace-regexp-in-string " 3rd," " 3" date-string))
        (setq date-string (replace-regexp-in-string "\\([0-9]\\)th," "\\1" date-string))

        (setq date-string (replace-regexp-in-string " 1st " " 1 " date-string))
        (setq date-string (replace-regexp-in-string " 2nd " " 2 " date-string))
        (setq date-string (replace-regexp-in-string " 3rd " " 3 " date-string))
        (setq date-string (replace-regexp-in-string "\\([0-9]\\)th " "\\1 " date-string))

        (let* ((dateList (parse-time-string date-string))
               (year (nth 5 dateList))
               (month (nth 4 dateList))
               (day (nth 3 dateList))
               (yyyy (number-to-string year))
               (mm (if month (format "%02d" month) "" ))
               (dd (if day (format "%02d" day) "" )))
          (concat yyyy "-" mm "-" dd)))))))


(defun pigbug-parse-date (date-string)
  "Returns the DATE-STRING parsed into the date format (`decode-time')

Works for American formatted date strings, e.g. MM/DD/YYYY"
  (parse-time-string (pigbug-fix-timestamp-string date-string)))

(defun pigbug-zero-date (date)
  "Zero the DATE by filling nils with `0'."
  (let* ((dropped (-drop-while 'null date)))
    (append (-repeat (- (length date) (length dropped)) 0) dropped)))

(defun pigbug-date-to-time (date)
  "Encode the DATE as a time."
  (apply 'encode-time (pigbug-zero-date date)))

(defun pigbug-date-less-p (date1 date2)
  "Returns true if DATE1 is less than DATE2"
  (time-less-p (pigbug-date-to-time date1)
               (pigbug-date-to-time date2)))

(provide 'pigbug-core)
;;; pigbug-core.el ends here

;;; maxima.el --- Major mode for Maxima             -*- lexical-binding: t; -*-

;; Copyright (C) 1998,1999 William F. Schelter
;; Copyright (C) 2001-2007 Jay Belanger
;; Copyright (C) 2020 Fermin Munoz

;; Author: William F. Schelter
;;         Jay Belanger
;;         Fermin Munoz
;; Maintainer: Fermin Munoz <fmfs@posteo.net>
;; Created: 30 April 2020
;; Version: 0.7.6
;; Keywords: maxima,tools,math
;; URL: https://gitlab.com/sasanidas/maxima
;; Package-Requires: ((emacs "25.1")(s "1.11.0")(test-simple "1.3.0"))
;; License: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Quick intro
;;
;; To install, put this file (as well as maxima-font-lock.el) somewhere in your
;; Emacs load path. To make sure that `maxima.el' is loaded when necessary,
;; whether to edit a file in maxima mode or interact with Maxima in an Emacs
;; buffer, put the lines (or for use-package users see below)
;;  (autoload 'maxima-mode "maxima" "Maxima mode" t)
;;  (autoload 'maxima "maxima" "Maxima interaction" t)
;; in your `.emacs' file.  If you want any file ending in `.mac' to begin
;; in `maxima-mode', for example, put the line
;;  (setq auto-mode-alist (cons '("\\.mac" . maxima-mode) auto-mode-alist))
;; to your `.emacs' file.

;; for users of use-package, the maxima package can be loaded with the form

;; (use-package maxima
;;   :init
;;   (add-hook 'maxima-mode-hook #'maxima-hook-function)
;;   (add-hook 'maxima-inferior-mode-hook #'maxima-hook-function)
;;   (setq
;; 	 org-format-latex-options (plist-put org-format-latex-options :scale 2.0)
;; 	 maxima-display-maxima-buffer nil)
;;   :mode ("\\.mac\\'" . maxima-mode)
;;   :interpreter ("maxima" . maxima-mode))

;;; Code:

;;;; The requires

(require 'comint)
(require 'easymenu)
(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'info)
(require 's)

(require 'maxima-font-lock)


;;;; The variables that the user may wish to change

(defgroup maxima nil
  "Maxima mode group."
  :prefix "maxima-"
  :tag    "Maxima"
  :group 'maxima-mode
  :link '(url-link :tag "Repository" "https://gitlab.com/sasanidas/maxima"))

(defcustom maxima-inchar "\\(C\\|%i\\)"
  "*The character used for an input prompt."
  :group 'maxima
  :type 'string)

(defcustom maxima-outchar "\\(D\\|%o\\)"
  "*The character used for an output prompt."
  :group 'maxima
  :type 'string)

(defcustom maxima-linechar "\\(E\\|%t\\)"
  "*The character used for an intermediate output prompt."
  :group 'maxima
  :type 'string)

(defcustom maxima-indent-amount 2
  "*The amount of each indentation level in `maxima-mode'.
This is used after `then', etc."
  :group 'maxima
  :type '(integer))

(defcustom maxima-paren-indent-amount 1
  "*The amount of indentation after a parenthesis."
  :group 'maxima
  :type '(integer))

(defcustom maxima-function-indent-amount 2
  "*The amount of extra indentation to give within functions."
  :group 'maxima
  :type 'integer)

(defvar maxima-blockparen-indent-amount nil)
(if maxima-blockparen-indent-amount
    (setq maxima-function-indent-amount
          maxima-blockparen-indent-amount))

(defcustom maxima-continuation-indent-amount 2
  "*The amount of extra indentation given when a line is continued."
  :group 'maxima
  :type '(integer))

(defcustom maxima-multiline-comment-indent-amount 2
  "*The amount of extra indentation inside a multiline comment."
  :group 'maxima
  :type '(integer))

(defcustom maxima-dont-reindent-some-comments t
  "*If non-nil, TAB will not change indentation of some comments.
Namely those with nothing on the starting line past the `/*'."
  :group 'maxima
  :type 'boolean)

(defcustom maxima-if-extra-indent-amount 0
  "*The amount of extra indentation to give a \"then\" following an \"if\"."
  :group 'maxima
  :type 'integer)

(defcustom maxima-display-maxima-buffer t
  "*If non-nil, display maxima buffer.
It shows the buffer every time `maxima-send-buffer' is called."
  :group 'maxima
  :type 'boolean)

(defcustom maxima-indent-style 'standard
  "*Determines how `maxima-mode' will handle tabs.
Choices are 'standard, 'perhaps-smart"
  :group 'maxima
  :type '(choice :menu-tag "Indent style"
                 :tag      "Indent style"
                 (const standard)
                 (const perhaps-smart)))

(defcustom maxima-return-style 'newline-and-indent
  "*Determines how `maxima-mode' will handle RET.
Choices are 'newline, 'newline-and-indent, and 'reindent-then-newline-and-indent"
  :group 'maxima
  :type '(choice :menu-tag "Return style"
                 :tag      "Return style"
                 (const newline)
                 (const newline-and-indent)
                 (const reindent-then-newline-and-indent)))



(defvar maxima-newline-style nil
  "For compatibility.")

(defcustom maxima-command "maxima"
  "*The command used to start Maxima."
  :group 'maxima
  :type 'string)

(defcustom maxima-args nil
  "*Extra arguments to pass to the `maxima-command'."
  :group 'maxima
  :type 'string)

(defcustom maxima-use-tabs nil
  "*If non-nil, indentation will use tabs."
  :group 'maxima
  :type 'boolean)

(defcustom maxima-minibuffer-2d nil
  "*If non-nil, use 2D output for `maxima-minibuffer'."
  :group 'maxima
  :type 'boolean)

(defcustom maxima-use-full-color-in-process-buffer nil
  "*If non-nil, font-lock the maxima process buffer."
  :group 'maxima
  :type 'boolean)

(defcustom maxima-save-input-history nil
  "*If non-nil, save the input history in a file."
  :group 'maxima
  :type 'boolean)

(defcustom maxima-input-history-file "~/.maxima_history"
  "*A file to save the input history in."
  :group 'maxima
  :type 'file)

(defcustom maxima-input-history-length 50
  "*How many lines of history to save."
  :group 'maxima
  :type 'integer)

(defcustom maxima-minor-prefix "^[ \t]*$"
  "*Regexp that indicate the beginning of a region to send to Maxima.
Available with function `maxima-minor-mode'."
  :group 'maxima
  :type 'string)

(defcustom maxima-minor-postfix "^[ \t]*$"
  "*Regexp to indicate the end of a region to send to Maxima.
Available with function `maxima-minor-mode'."
  :group 'maxima
  :type 'string)

(defcustom maxima-minor-output "==>"
  "*A string to insert in the buffer right before the output."
  :group 'maxima
  :type 'string)

(defcustom maxima-minor-output-end " //"
  "*A string to insert in the buffer right after the output."
  :group 'maxima
  :type 'string)

(defcustom maxima-mode-minor-output "/*==>"
  "*A string to insert in the buffer right before the output."
  :group 'maxima
  :type 'string)

(defcustom maxima-mode-minor-output-end " <==*/"
  "*A string to insert in the buffer right after the output."
  :group 'maxima
  :type 'string)

(defcustom maxima-minor-mode-check-input t
  "*Non-nil means check the input in Maxima minor mode before sending it."
  :group 'maxima
  :type 'boolean)

(defcustom maxima-comint-output-functions
  '(maxima-inferior-output-filter
    maxima-inferior-replace-tabs-by-spaces
    maxima-inferior-remove-double-input-prompt)
  "List of functions to use when `maxima-make-inferior' is called."
  :group 'maxima
  :type 'list)

(defun maxima-minor-output-mark ()
  "Internal function, check if point is in a comment.
And call the correct function."
  (if (and
       (eq major-mode 'maxima-mode)
       (not (maxima-in-comment-p)))
      maxima-mode-minor-output
    maxima-minor-output))

(defun maxima-minor-output-mark-end ()
  "Internal function, check if point is in a comment.
And insert the correct end output."
  (if (and
       (eq major-mode 'maxima-mode)
       (not (maxima-in-comment-p)))
      maxima-mode-minor-output-end
    maxima-minor-output-end))

(defun maxima-get-version (command)
  "Internal function, get the maxima version, with COMMAND."
  (let* ((output (shell-command-to-string (format "%s --version" (eval command) ))))
    (car (s-match "[[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+" output))))

;;;; The other variables

;; This variable seems to be necessary

(defvar maxima-version (maxima-get-version 'maxima-command)
  "Maxima version, it uses `maxima-command'.")

(defcustom maxima-libraries-directory
  (format "%s/%s/share/" "/usr/share/maxima" maxima-version)
  "Maxima libraries directory."
  :group 'maxima
  :type 'string)

(defvar maxima-inferior-after-output-wait 100)

(defvar maxima-temp-suffix 0
  "Temporary filename suffix.  Incremented by 1 for each filename.")

(defvar maxima-special-symbol-letters "!:='")

(defvar maxima-minibuffer-history nil)

(defvar maxima-block "")

(defvar maxima-block-wait "")

(defvar maxima-inferior-process nil
  "The Maxima process.")

(defvar maxima-auxiliary-inferior-process nil
  "The Maxima auxiliary process.
This process is used for autocompletion and documentation.")

(defvar maxima-inferior-input-end 0
  "The end of the latest input that was sent to Maxima.")

(defvar maxima-inferior-output-end 0)

(defvar maxima-inferior-waiting-for-output nil)

(defvar maxima-inferior-exit-hook nil)

(defvar maxima-inferior-prompt
  (concat "\\(^(" maxima-inchar
          "[0-9]*) \\)\\|\\(^MAXIMA> \\)\\|\\(^(dbm:[0-9]*) \\)")
					; \\(^[^#%)>]*[#%)>]+ *\\)"
  "*Regexp to recognize prompts from the inferior Maxima.") ; or lisp")


(defvar maxima-mode-highlight nil)

(defvar maxima-mode-region-begin nil)

(defvar maxima-mode-region-end nil)

(defvar maxima-minor-mode-region-begin nil)

(defvar maxima-minor-mode-region-end nil)

(defvar maxima-minor-mode-highlight nil)

(defvar maxima-minor-mode-bad-delimiter-regexp "\\([ \t\n]+\\|[0-9]+\\)")

;;;; Utility functions

(defun maxima-string-equal (str1 str2)
  "Check if STR1 and STR2 are equal,ignoring font case."
  (string= (downcase str1) (downcase str2)))

;; This was taken from `replace-regexp-in-string' from subr.el in GNU emacs.
(defun maxima-replace-in-string (regexp rep string)
  "Replace all ocurrences for REGEXP with REP in STRING."
  (let ((l (length string))
	(start 0)
	matches str mb me)
    (save-match-data
      (while (and (< start l) (string-match regexp string start))
	(setq mb (match-beginning 0)
	      me (match-end 0))
	(when (= me mb) (setq me (min l (1+ mb))))
	(string-match regexp (setq str (substring string mb me)))
	(setq matches
	      (cons (replace-match (if (stringp rep)
				       rep
				     (funcall rep (match-string 0 str)))
				   nil nil str)
		    (cons (substring string start mb)       ; unmatched prefix
			  matches)))
	(setq start me))
      ;; Reconstruct a string from the pieces.
      (setq matches (cons (substring string start l) matches)) ; leftover
      (apply #'concat (nreverse matches)))))

(defun maxima-remove-kill-buffer-hooks ()
  "Remove the kill-buffer-hooks locally."
  (let ((hooks kill-buffer-hook))
    (while hooks
      (remove-hook 'kill-buffer-hook (car hooks) t)
      (setq hooks (cdr hooks)))))

(defun maxima-strip-string-beginning (string)
  "Return STRING with whitespace and comments removed from the beginning."
  (with-temp-buffer
    (modify-syntax-entry ?/ ". 14")
    (modify-syntax-entry ?* ". 23")
    (insert string)
    (goto-char (point-min))
    (maxima-forward-over-comment-whitespace)
    (buffer-substring-no-properties (point) (point-max))))
  

(defun maxima-strip-string-end (string)
  "Return STRING with whitespace and comments removed from the end."
  (with-temp-buffer
    (modify-syntax-entry ?/ ". 14")
    (modify-syntax-entry ?* ". 23")
    (insert string)
    (goto-char (point-max))
    (maxima-back-over-comment-whitespace)
    (buffer-substring-no-properties (point-min) (point))))

(defun maxima-strip-string (string)
  "Return STRING with whitespace and comments removed from the ends."
  (maxima-strip-string-beginning (maxima-strip-string-end string)))

(defun maxima-strip-string-add-semicolon (string)
  "Return STRING with whitespace and comments removed from the ends."
  (setq string
        (maxima-strip-string-beginning (maxima-strip-string-end string)))
  (unless (or
           (string= string "")
           (and (>= (length string) 5)
                (string= (substring string 0 5) ":lisp"))
           (string= (substring string -1) ";")
           (string= (substring string -1) "$"))
    (setq string (concat string ";")))
  string)

;;;; Functions that query position
(defun maxima-in-comment-p ()
  "Non-nil means that the point is in a comment."
  (let ((pt (point)))
    (save-excursion
      (and
       (search-backward "/*" nil t)
       (not (search-forward "*/" pt t))))))
;; FIXME add test
(defun maxima-in-output-p ()
  "Non-nil means that the point is in minibuffer output."
  (let ((pt (point)))
    (save-excursion
      (and
       (search-backward (maxima-minor-output-mark) nil t)
       (not (search-forward (maxima-minor-output-mark-end) pt t))))))

;;; Functions that search

;; Some additions to help with different types of searches
(defvar maxima-mode-type 'maxima-mode)
(make-variable-buffer-local 'maxima-mode-type)



(defun maxima-re-search-forward (regexp &optional pmax)
  "Search forward for REGEXP, bounded by PMAX.
Ignore matches found in comments and strings."
  (let ((keep-looking t)
        (didnt-find nil)
        (match nil)
        (ppe)
        (pt)
        (origpt (point)))
    (setq pt origpt)
    (while (and keep-looking
                (not didnt-find)
                (re-search-forward regexp pmax t))
      (setq match (match-string 0))
      (setq ppe (parse-partial-sexp pt (point)))
      (cond
       ((nth 3 ppe)  ;; In a string
        (if (maxima-goto-end-of-string)
            (setq pt (point))
          (setq didnt-find t)))
       ((nth 4 ppe)  ;; In a comment
        (if (maxima-goto-end-of-comment)
            (setq pt (point))
          (setq didnt-find t)))
       (t            ;; not in a comment or string
        (setq keep-looking nil))))
    (if (or didnt-find keep-looking)
        (progn
          (goto-char origpt)
          nil)
      match)))

(defun maxima-re-search-forward-skip-blocks (regexp &optional pmax)
  "Search forward for REGEXP, bounded by PMAX.
Ignore matches found in comments and strings, and skip over
parenthesized or bracketed blocks."
  (let ((keep-looking t)
        (didnt-find nil)
        (match nil)
        (pt (point)))
    (while (and keep-looking
                (setq match (maxima-re-search-forward
                             (concat regexp "\\|[[(]") pmax)))
      (cond
       ((or
         (string= match "[")
         (string= match "("))
        (unless
            (maxima-goto-end-of-list)
          (setq didnt-find t)
          (setq keep-looking nil)))
       (t
        (setq keep-looking nil))))
    (if (or keep-looking didnt-find)
        (progn
          (goto-char pt)
          nil)
      match)))

(defun maxima-re-search-backward (regexp &optional pmin)
  "Search backward for REGEXP, bounded by PMIN.
Ignore matches found in comments and strings."
  (let ((keep-looking t)
        (match nil)
        (ppe)
        (origpt (point)))
    (unless pmin
      (setq pmin (point-min)))
    (while (and keep-looking
                (re-search-backward regexp pmin t))
      (setq match (match-string 0))
      (setq ppe (parse-partial-sexp pmin (point)))
      (cond
       ((nth 8 ppe)  ;; In a string or comment
        (goto-char (nth 8 ppe)))
       (t            ;; not in a comment or string
        (setq keep-looking nil))))
    (if keep-looking
        (progn
          (goto-char origpt)
          nil)
      match)))

(defun maxima-re-search-backward-skip-blocks (regexp &optional pmin)
  "Search forward for REGEXP, bounded by PMIN.
Ignore matches found in comments and strings, and skip over
parenthesized and bracketed blocks."
  (let ((keep-looking t)
        (didnt-find nil)
        (match nil)
        (pt (point)))
    (while (and keep-looking
                (setq match (maxima-re-search-backward
                             (concat regexp "\\|[])]") pmin)))
      (cond
       ((or
         (string= match ")")
         (string= match "]"))
        (unless
            (maxima-goto-beginning-of-list)
          (setq didnt-find t)
          (setq keep-looking nil)))
       (t
        (setq keep-looking nil))))
    (if (or keep-looking didnt-find)
        (progn
          (goto-char pt)
          nil)
      match)))

(defun maxima-escaped-char-p ()
  "Return non-nil if the character after point is escaped."
  (let ((pm (point-min))
        (esc-chars 0))
    (when (> (point) pm)
      (save-excursion
        (forward-char -1)
        (while (and
                (looking-at "\\\\")
                (setq esc-chars (1+ esc-chars))
                (> (point) pm))
          (forward-char -1))))
    (if (= (% esc-chars 2) 0)
        nil
      t)))

(defun maxima-goto-end-of-string ()
  "Go to the end of the string that the point is in.
Assumes that point is in a string."
  (interactive)
  (let ((keep-looking t))
    (while (and keep-looking (search-forward "\"" nil t))
      (forward-char -2)
      (unless (maxima-escaped-char-p)
        (setq keep-looking nil))
      (forward-char 2))
    (if keep-looking
        nil
      t)))

(defun maxima-goto-beginning-of-string ()
  "Go to the beginning of the string that the point is in.
Assumes that point is in a string."
  (interactive)
  (let ((keep-looking t))
    (while (and keep-looking (search-backward "\"" nil t))
      (forward-char -1)
      (unless (maxima-escaped-char-p)
        (setq keep-looking nil))
      (forward-char 1))))

(defun maxima-goto-end-of-comment ()
  "Go to the end of the comment that the point is in.
Assumes that point is in a comment."
  (interactive)
  (search-forward "*/" nil t))

(defun maxima-goto-beginning-of-comment ()
  "Go to the beginning of the comment that the point is in.
Assumes that point is in a comment."
  (interactive)
  (search-backward "/*"))

(defun maxima-find-next-nonnested-close-char ()
  "Search forward for next , ; $ or closing ), skipping over balanced parens.
If character is in a string or a list, ignore it."
  (interactive)
  (maxima-re-search-forward-skip-blocks "[,;$)]"))


;;; Functions for dealing with words

(defun maxima-next-char-word-part-p ()
  "Non-nil if next char is a a word part."
  (or
   (looking-at "\\w")
   (looking-at "\\\\")
   (save-excursion
     (forward-char -1)
     (looking-at "\\\\"))))

(defun maxima-previous-char-word-part-p ()
  "Non-nil if previous character is a word part."
  (save-excursion
    (forward-char -1)
    (maxima-next-char-word-part-p)))

(defun maxima-forward-word ()
  "Go to the end of the current word."
  (let ((keep-going t))
    (while keep-going
      (cond
       ((looking-at "\\w")
        (forward-word 1))
       ((looking-at "\\\\")
        (forward-char 2))
       (t
        (setq keep-going nil))))))

(defun maxima-backward-word ()
  "Go to the beginning of the current word."
  (let ((keep-going t))
    (while keep-going
      (cond
       ((and
         (not (bobp))
         (save-excursion
           (forward-char -1)
           (looking-at "\\w")))
        (backward-word 1))
       ((and
         (> (point) (1+ (point-min)))
         (save-excursion
           (forward-char -2)
           (looking-at "\\\\")))
        (forward-char -2))
       (t
        (setq keep-going nil))))))

;;;; Functions that return special positions

(defun maxima-name-beginning ()
  "Return the name of the position."
  (save-excursion
    (maxima-backward-word)
    (point)))

(defun maxima-special-symbol-beginning ()
  "Find the beginning of a special symbol."
  (save-excursion
    (skip-chars-backward maxima-special-symbol-letters)
    (point)))

(defun maxima-special-symbol-end ()
  "Find the end of a special symbol."
  (save-excursion
    (skip-chars-forward maxima-special-symbol-letters)
    (point)))

(defun maxima-form-beginning-position ()
  "Find the beginning of a form."
  (save-excursion
    (maxima-goto-beginning-of-form)
    (point)))

(defun maxima-form-end-position ()
  "Find the end of a form."
  (with-current-buffer (current-buffer)
    (if (maxima-goto-end-of-form)
	(point)
      nil)))

(defun maxima-form-end-position-or-point-max ()
  "Find the end of a form or the end of the buffer."
  (let ((mfep (maxima-form-end-position)))
    (if mfep
        mfep
      (point-max))))

(defun maxima-expression-end-position ()
  "Return the point where the current expression ends, or nil."
  (save-excursion
    (if (maxima-goto-end-of-expression)
        (point)
      nil)))

(defun maxima-begin-if-position (pmin)
  "Find the point PMIN of the opening \"if\" for the current point."
  (let ((nest 0)
        (match)
        (keep-looking t)
        (pt (point)))
    (save-excursion
      (while (and keep-looking
                  (setq match
                        (maxima-re-search-backward-skip-blocks
                         "\\<if\\>\\|\\<then\\>" pmin)))
        (setq match (downcase match))
        (cond ((maxima-string-equal match "if") (setq nest (1+ nest)))
              (t (setq nest (1- nest))))
        (when (= nest 1)
          (setq pt (point))
          (setq keep-looking nil))))
    (if keep-looking
        nil
      pt)))

(defun maxima-begin-then-position (pmin)
  "Find the point PMIN of the opening \"then\" for the current \"else\"."
  (let ((keep-going t)
        (pt (point))
        (begin-then nil))
    (save-excursion
      (while (and keep-going
                  (maxima-re-search-backward-skip-blocks "\\<then\\>" pmin))
	;; A potential "then".  Let's see if it is.
	(let ((meep (maxima-expression-end-position)))
          (when (or (not meep) (<= pt meep))
            ;; This "then" is looking pretty good.
            ;; Now we need to make sure that there aren't any "else"s
            ;; in the way.
            (let ((level 0)
                  (match))
              (save-excursion
		(while (setq match (maxima-re-search-forward-skip-blocks
                                    "\\<then\\>\\|\\<else\\>" pt))
                  (cond ((maxima-string-equal match "then")
			 (setq level (1+ level)))
			((maxima-string-equal match "else")
			 (setq level (1- level))))))
              (when (= level 1)
		(setq begin-then (point))
		(setq keep-going nil))))))
      begin-then)))

;;;; Functions that move the position
(defun maxima-forward-over-comment-whitespace ()
  "Move forward over comments and whitespace."
  (forward-comment (buffer-size))
  (let ((mmo (string-trim-left (maxima-minor-output-mark))))
    (when (and (> (- (point-max) (point)) (length mmo))
               (string=
                (buffer-substring-no-properties
                 (point) (+ (point) (length mmo)))
                mmo))
      (search-forward (maxima-minor-output-mark-end))
      (forward-comment (buffer-size)))))

(defun maxima-back-over-comment-whitespace ()
  "Move backward over comments and whitespace."
  (forward-comment (- (buffer-size)))
  (let ((mme (string-trim-right (maxima-minor-output-mark-end))))
    (when (and (> (- (point) (point-min)) (length mme))
               (string=
                (buffer-substring-no-properties
                 (- (point) (length mme)) (point))
                mme))
      (search-backward (maxima-minor-output-mark))
      (forward-comment (- (buffer-size))))))

(defun maxima-goto-beginning-of-form ()
  "Move to the beginning of the form."
  (interactive)
  (let ((pt (point))
        (keep-looking t))
    (while (and keep-looking
                (maxima-re-search-backward "[;$]" nil))
      (forward-char -1)
      (unless (looking-at "\\\\\\$")
        (forward-char 2)
        (setq keep-looking nil)))
    (if keep-looking
        (goto-char (point-min)))
    (maxima-forward-over-comment-whitespace)
    (if (< pt (point))
        (goto-char pt))
    (point)))

(defun maxima-goto-end-of-form ()
  "Move to the end of the form."
  (interactive)
  (let ((keep-looking t)
        (pt (point)))
    (while (and keep-looking
                (maxima-re-search-forward "[;$]" nil))
      (forward-char -2)
      (unless (looking-at "\\\\\\$")
        (setq keep-looking nil))
      (forward-char 2))
    (if (not keep-looking)
        (point)
      (goto-char pt)
      nil)))

(defun maxima-goto-end-of-expression ()
  "Find the point that ends the expression that begins at point.
The expression is assumed to begin with \"if\", \"then\", \"do\"
\"else\" or \"(\".  Return nil if the end is not found."
					;  (interactive)
  ;; To find the end of the expression:
  ;;  if looking at (, look for )
  ;;  otherwise look for a , ; or $ at the same nesting level of
  ;;  parentheses or a closing ).
  (cond ((or (looking-at "(")
             (looking-at "\\["))
         (maxima-forward-list))
        (t
         (maxima-find-next-nonnested-close-char))))

(defun maxima-goto-beginning-of-construct (pmin)
  "Go to the point PMIN,the begins of the current construct."
  (let ((keep-looking t)
        (pt (point)))
    (while (and keep-looking
                (maxima-re-search-backward-skip-blocks
                 "\\<if\\>\\|\\<then\\>\\|\\<do\\>\\|\\<else\\>\\|(\\|\\[" pmin))
      (save-excursion
        (when (or (not (maxima-goto-end-of-expression)) (<= pt (point)))
          (setq keep-looking nil))))
    (if keep-looking
        (goto-char pmin))
    (point)))

;; FIXME this can be optimize, maybe without so much workaround
(defun maxima-goto-end-of-list ()
  "Go up a list.
Return t if possible, nil otherwise."
  (interactive)
  (if
      (condition-case nil
          (up-list 1)
        (error t))
      nil
    t))

(defun maxima-goto-beginning-of-list ()
  "Go up a list backwards.
Return t if possible, nil otherwise."
  (interactive)
  (if
      (condition-case nil
          (up-list -1)
        (error t))
      nil
    t))

(defun maxima-forward-list ()
  "Go forward a list.
Return t if possible, nil otherwise."
  (if
      (condition-case nil
          (forward-list 1)
        (error nil))
      t
    nil))

(defun maxima-backward-list ()
  "Go backward a list.
Return t if possible, nil otherwise."
  (if
      (condition-case nil
          (backward-list 1)
        (error nil))
      t
    nil))

;; TEST
;;; Newlines and indents
(defun maxima-indent-form ()
  "Indent the entire form."
  (interactive)
  (indent-region
   (maxima-form-beginning-position)
   (maxima-form-end-position-or-point-max)
   nil))

;;; 'standard
(defun maxima-standard-indent ()
  "Indent the line based on the previous line.
If the previous line opened a parenthesis, `maxima-indent-amount' is
added to the indentation, if the previous line closed a parenthesis,
`maxima-indent-amount' is subtracted, otherwise the indentation
is the same as the previous line."
  (interactive)
  (let ((indent 0)
        (match)
        (pt))
    (save-excursion
      (when (= (forward-line -1) 0)
        (progn
          (setq indent (current-indentation))
          (setq pt (line-end-position))
          (while (setq match (maxima-re-search-forward "[()]" pt))
            (cond ((string= match ")")
                   (setq indent (- indent maxima-indent-amount)))
                  ((string= match "(")
                   (setq indent (+ indent maxima-indent-amount))))))))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-line-to (max indent 0)))
    (skip-chars-forward " \t")))

(defun maxima-untab ()
  "Delete a level of indentation."
  (interactive)
  ;; Check to see if the previous maxima-indent-amount spaces are
  ;; on the same line and blank
  (let ((i maxima-indent-amount)
	(ok t)
	(pt (point)))
    (save-excursion
      (while (and (> i 0) ok)
	(setq i (- i 1))
	(forward-char -1)
	(if (not (looking-at " "))
	    (setq ok nil))
	(if (looking-at "\n")
	    (setq ok nil))))
    (if ok
	(delete-region pt (- pt maxima-indent-amount)))))


;;; Find the beginning of a function

(defun maxima-open-paren-is-function ()
  "Check to see if the point is before an opening paren.
Also if the previous character is a close paren or word part."
  (and
   (looking-at "[ \t]*(")
   (save-excursion
     (maxima-back-over-comment-whitespace)
     (forward-char -1)
     (looking-at ")\\|\\w"))))

(defun maxima-close-paren-before-open-paren ()
  "Return point of close paren before the current open paren.
Otherwise nil if there is none."
  (save-excursion
    (maxima-back-over-comment-whitespace)
    (forward-char -1)
    (if (looking-at ")")
        (1+ (point))
      nil)))

(defun maxima-back-over-paren-groups ()
  "Go over any paren groups.
Assume point is at \"(\", as long as preceding character is
\")\", go to open parentheses."
  (let ((cpbop (maxima-close-paren-before-open-paren)))
    (while cpbop
      (goto-char cpbop)
      (maxima-backward-list)
      (setq cpbop
            (maxima-close-paren-before-open-paren)))))

(defun maxima-word-on-previous-line ()
  "Go to the previous word part."
  (let ((pt (point)))
    (save-excursion
      (skip-chars-backward " \t\n")
      ;; Same line?
      (and
       (maxima-previous-char-word-part-p)
       (string-match "\n"
                     (buffer-substring-no-properties (point) pt))))))

(defun maxima-string-on-previous-line ()
  "Go to the previous string part."
  (let ((pt (point)))
    (save-excursion
      (skip-chars-backward " \t\n")
      ;; Same line?
      (forward-char -1)
      (and
       (looking-at "\"")
       (string-match "\n"
                     (buffer-substring-no-properties (point) pt))))))

(defun maxima-back-over-function-name ()
  "Go back over the `foo' in 'foo(x,y)', return the length of `foo'.
Return nil if open paren is not part of function.
Assumes the point is right before the open parenthesis."
  (let ((pt (point))
        (endpt))
    (maxima-back-over-paren-groups)
    (setq endpt (point))
    ;; Let's see what's before this
    (cond
     ;; There is a word right before this
     ((maxima-previous-char-word-part-p)
      (maxima-backward-word)
      (- endpt (point)))
     ;; There is a string before this
     ((save-excursion
        (forward-char -1)
        (looking-at "\""))
      (maxima-goto-beginning-of-string)
      (- endpt (point)))
     ;; There is a word before this on the previous line
     ((maxima-word-on-previous-line)
      (skip-chars-backward " \t\n")
      (setq endpt (point))
      (maxima-backward-word)
      (- endpt (point)))
     ;; There is a string before this on the previous line
     ((maxima-string-on-previous-line)
      (skip-chars-backward " \t\n")
      (setq endpt (point))
      (maxima-goto-beginning-of-string)
      (- endpt (point)))
     ;; This point is the original point
     ((= endpt pt)
      nil)
     ;; Finally, the last parenthesized expression is the function
     (t
      (save-excursion
        (maxima-forward-list)
        (setq endpt (point)))
      (- endpt (point))))))

;;; 'perhaps-smart

(defun maxima-after-lisp-expression-p ()
  "Return non-nil if the point is right after a Lisp expression."
  (let ((pm (point-min))
        (pt))
    (save-excursion
      (maxima-back-over-comment-whitespace)
      (setq pt (point))
      (condition-case nil
          (backward-sexp)
        (error t))
      (when (< (point) pt)
        (maxima-back-over-comment-whitespace)
        (if (< (point) (+ 5 pm))
            nil
          (forward-char -5)
          (if (looking-at ":lisp")
              (current-column)
            nil))))))

(defun maxima-perhaps-smart-calculate-indent ()
  "Return appropriate indentation for current line as Maxima code.
Returns an integer: the column to indent to."
  (let ((indent 0)
        (pmin)
        (tmpchar)
        (pt)
        (le)
        (comma-line)
        (len)
        (pps))
    (save-excursion
      (beginning-of-line)
      (setq pt (point))
      (setq pmin (maxima-form-beginning-position))
      (setq pps (parse-partial-sexp pmin (point)))
      (setq le (maxima-after-lisp-expression-p))
      (when (nth 1 pps)
        (setq pmin (nth 1 pps))
        (unless (looking-at "^[ \t]*,")
          (save-excursion
            (when (maxima-re-search-backward-skip-blocks "," pmin)
              (save-excursion
                (let ((lep (line-end-position)))
                  (forward-char 1)
                  (maxima-forward-over-comment-whitespace)
                  (unless (>= (point) lep)
                    (setq pmin (point)))))))))
      (cond
       ;; First, take care of the cases where the indentation is clear
       ;; No indentation at the beginning of the buffer
       ((= pt (point-min))
        (setq indent 0))
       ;; Don't change the indentation if in a string
       ((nth 3 pps)
        (setq indent -1))
       ;; If the line begins a comment and comments aren't reindented,
       ;; don't reindent it
       ((and
         (looking-at "[ \t]*/\\*[ \t]*$")
         maxima-dont-reindent-some-comments)
        (setq indent -1))
       ;; Deal with comments separately
       ((maxima-perhaps-smart-in-comment-p (nth 4 pps) pmin pt)
        (setq indent (maxima-perhaps-smart-comment-calculate-indent)))
       ;; If the current point is the beginning of the form, the level is 0
       ((= (point) pmin)
        (setq indent 0))
       ;; If the current point is in maxima minor output, don't reindent it
       ((maxima-in-output-p)
        (setq indent -1))
       ;; A line beginning "then" is indented like the opening "if"
       ((and
         (looking-at "[ \t]*\\<then\\>")
         (setq tmpchar (maxima-begin-if-position pmin)))
        (goto-char tmpchar)
        (setq indent (+ maxima-if-extra-indent-amount (current-column))))
       ;; A line beginning "else" is indented like the corresponding "then"
       ((and
         (looking-at "[ \t]*\\<else\\>")
         (setq tmpchar (maxima-begin-then-position pmin)))
        (goto-char tmpchar)
        (setq indent (current-column)))
       ;; A line beginning with an open paren that is the
       ;; beginning of a function argument is indented according to
       ;; the function
       ((and
         (looking-at "[ \t]*(")
         (setq len (maxima-back-over-function-name)))
        (setq indent (+ (current-column)
                        (min len maxima-function-indent-amount))))
       ;; A line beginning with a closing paren is indented like the open paren
       ((looking-at "[ \t]*)")
        ;; Here, pmin should be the opening paren position
        (goto-char pmin)
        (if (looking-at "( *$")
            (progn
              (setq len (maxima-back-over-function-name))
              (setq indent (+
                            (if len
                                (min len maxima-function-indent-amount)
                              0)
                            (current-column))))
          (setq indent (current-column))))
       ;; A line beginning with a closing bracket is indented like the open bracket
       ((looking-at "[ \t]*\\]")
        ;; Here, pmin should be the opening bracket position
        (goto-char pmin)
        (setq indent (current-column)))
					;       ;; A line beginning with a comma is indented like the opening paren
					;       ((looking-at "[ \t]*,")
					;        (goto-char pmin)
					;        (setq indent (current-column)))
       ;; The point is at the end of a lisp expression
       (le
        (setq indent le))
       ;; Otherwise, the correct indentation needs to be computed.
       (t
        ;; Find the indentation of beginning of the current construct
        ;; If begin-construct is nil, indent according to the opening paren
        (setq comma-line (looking-at "[ \t]*,"))
        (save-excursion
          (maxima-goto-beginning-of-construct pmin)
          (cond
           ;; The construct begins with a bracket
           ((looking-at "\\[")
					;            (if comma-line
					;                (setq indent (current-column))
            (setq indent (+ maxima-paren-indent-amount (current-column)))
            (forward-char 1)
            (skip-chars-forward " \t")
            (unless (looking-at "\n")
              (setq indent (current-column))));)
           ;; The construct begins with a paren
           ((looking-at "(")
            (cond
					;             (comma-line
					;              (setq indent (current-column)))
             ((save-excursion
                (let ((lep (line-end-position)))
                  (forward-char 1)
                  (maxima-forward-over-comment-whitespace)
                  (>= (point) lep)))
					;(looking-at "( *$")
              ;; Check to see if there is anything before the (
              (if (save-excursion
                    (re-search-backward "\\(?:^[ \t]*\\)\\=" nil t))
                  (setq tmpchar maxima-paren-indent-amount)
                (setq tmpchar 0))
              ;; If there is nothing after the (, there are two
              ;; cases
              ;; First, there is a function before it
              (if (setq len (maxima-back-over-function-name))
                  (setq indent (+ (min len maxima-function-indent-amount)
                                  tmpchar
                                  (current-column)))
                ;; Otherwise,
                (setq indent (+ maxima-paren-indent-amount (current-column)))))
             ;; If there is something after the (, indent according to that
             (t
              (forward-char 1)
              (skip-chars-forward " \t")
              (setq indent (current-column)))))
           ;; The construct does not begin with a paren
           (t
            (setq indent (current-column)))))
        ;; Now, we need to possibly do some adjustments.
        ;; If the previous column doesn't end with close-char or a
        ;; parenthesis, assume that the current line in a continuation
        ;; of that line, and add to the indentation, unless the
        ;; previous line was the beginning of the construct containing
        ;; the point or only an open parenthesis.
        (if comma-line
            (let (comma-pt
                  diff)
              (skip-chars-forward " \t")
              (setq comma-pt (point))
              (forward-char 1)
              (skip-chars-forward " \t")
              (setq diff (- (point) comma-pt))
              (if (> diff indent)
                  (let ((lineno
                         (save-excursion
                           (forward-line 0)
                           (1+ (count-lines (point-min) (point))))))
                    (message
                     "Leading comma prevents proper indentation on line %d" lineno)
                    (setq indent 0))
                (setq indent (- indent diff))))
          (maxima-back-over-comment-whitespace)
          (unless (looking-at "^")
            (forward-char -1)
            (if (not (or le (looking-at "[,;$]")))
		(setq indent (+ maxima-continuation-indent-amount indent))))))))
    indent))

(defun maxima-perhaps-smart-in-comment-p (incomment pmin pt)
  "Determine if the point is in a comment or not.
It requires INCOMMENT PMIN and PT as an arguments."
  (cond
   ;; If we're told it's in a comment, then it is.
   (incomment
    t)
   ;; Otherwise, if pmin is less than point, we're not in a comment
   ((> pt pmin)
    nil)
   ;; Otherwise, we have to check it out
   (t
    (maxima-in-comment-p))))

(defun maxima-perhaps-smart-comment-calculate-indent ()
  "Calculate the indentation of the current line.
Which it is in a comment which begins on a previous line."
  (let ((indent 0)
        (blankline nil)
        (endcomment nil))
    (cond
     ((looking-at "^[ \t]*$")
      (setq blankline t))
     ((looking-at "^[ \t]*\\*/")
      (setq endcomment t)))
    ;; First of all, if the comment begins with `/*' and nothing
    ;; else on the line, don't indent it.
    (save-excursion
      (maxima-goto-beginning-of-comment)
      (cond
       ;; Take care of the case when comments won't be indented
       ((and
         (looking-at "/\\*[ \t]*$")
         maxima-dont-reindent-some-comments)
        (if blankline
            (setq indent (+ (current-column)
                            maxima-multiline-comment-indent-amount))
          (setq indent -1)))
       ;; Now, the other cases
       ;; If the current line ends a column, indent it like the opening line
       (endcomment
        (setq indent (current-column)))
       ;; If the opening line has a blank after the `/*'
       ((looking-at "/\\*[ \t\n]")
        (forward-char 2)
        (skip-chars-forward " \t")
        (if (not (looking-at "\n"))
            (setq indent (current-column))
          (search-backward "/*")
          (setq indent (+ (current-column)
                          maxima-multiline-comment-indent-amount))))
       (t
        (setq indent (+ (current-column)
                        maxima-multiline-comment-indent-amount)))))
    indent))

(defun maxima-perhaps-smart-indent-line ()
  "Reindent the current line."
  (interactive)
  (let ((indent (maxima-perhaps-smart-calculate-indent)))
    (unless (= indent -1)
      (save-excursion
        (beginning-of-line)
        (delete-horizontal-space)
        (indent-to indent)))
    (skip-chars-forward " \t")))

;;;; Indentation according to style

;; FIXME Unify identation, so there is no need for those cond
(defun maxima-indent-line ()
  "Indent the current line base on the indent style."
  (interactive)
  (cond
   ((eq maxima-newline-style 'basic)
    (maxima-standard-indent))
   ((eq maxima-indent-style 'standard)
    (maxima-standard-indent))
   ((eq maxima-indent-style 'perhaps-smart)
    (maxima-perhaps-smart-indent-line))))

(defun maxima-change-indent-style (new-style)
  "Change the newline style, it requires NEW-STYLE."
  (interactive "sNewline style (insert \"b\" for basic, \"s\" for standard, or \"p\" for perhaps-smart): ")
  (cond
   ((string= new-style "b")
    (setq maxima-indent-style 'basic))
   ((string= new-style "s")
    (setq maxima-indent-style 'standard))
   ((string= new-style "p")
    (setq maxima-indent-style 'perhaps-smart))))

(defun maxima-return ()
  "Check return function call,dictated by `maxima-return-style'."
  (interactive)
  (cond
   ((eq maxima-return-style 'newline)
    (newline))
   ((eq maxima-return-style 'newline-and-indent)
    (newline-and-indent))
   ((eq maxima-return-style 'reindent-then-newline-and-indent)
    (reindent-then-newline-and-indent))))

;;;; Commenting

(defun maxima-insert-short-comment ()
  "Prompt for a comment."
  (interactive)
  (let ((comment (read-string "Comment: ")))
    (insert "/* " comment " */")
    (newline-and-indent)))

(defun maxima-insert-long-comment ()
  "Insert a comment environment."
  (interactive)
  (indent-for-tab-command)
  (insert "/*")
  (newline)
  (newline)
  (insert "*/")
  (indent-for-tab-command)
  (forward-line -1)
  (indent-for-tab-command))

(defun maxima-uncomment-region (beg end)
  "`uncomment-region' to use with the menu, it requires BEG and END."
  (interactive "r")
  (uncomment-region beg end (universal-argument)))

;;;; Help functions
;; RX
;; (rx  (literal current-symbol)space
;;      (syntax open-parenthesis)
;;      (+ (any  "<" lower-case digit ">" "," "_" "]" "(" ")" "[" space "."))
;;      (syntax close-parenthesis))

;; FIXME add these functions to README.org
(defun maxima-document-get (symbol inferior-process)
  "Get the documentation with describe(SYMBOL).
Process the output of INFERIOR-PROCESS and get a list of the
function arguments documentation."
  (let* ((current-symbol symbol))
    (maxima-send-block (format "describe(\"%s\");" current-symbol) inferior-process)
    (seq-map 'car (with-temp-buffer
		    (insert (maxima-last-output-noprompt inferior-process))
		    (seq-uniq (s-match-strings-all
			       (format "%s[[:space:]]\\s([](),.<>[_[:lower:][:digit:][:space:]]+\\s)" current-symbol)
			       (buffer-substring-no-properties (point-min) (point-max))))))))

(defun maxima-symbol-doc ()
  "Pretty print documentation function."
  (interactive)
  (let* ((symbol (thing-at-point 'symbol t)))
    (message "%s" (mapconcat (lambda (element)
			       (format "%s" element))
			     (maxima-document-get symbol maxima-auxiliary-inferior-process) " || "))))

(defun maxima-get-info-on-subject (inferior-process  subject &optional test)
  "Get info of the Maxima SUBJECT in the INFERIOR-PROCESS.
The TEST variable is for test purpose."
  (let* ((command-output nil)
	 (help-buffer-displayed (get-buffer-window "*maxima-help*"))
	 (help-buffer (get-buffer-create "*maxima-help*")))
    (maxima-send-block (format "describe(\"%s\");" subject) inferior-process)
    (setq command-output (maxima-last-output-noprompt inferior-process))
    (with-current-buffer help-buffer
      (erase-buffer)
      (insert
       command-output)
      (goto-char (point-min)))
    (unless (or help-buffer-displayed test)
      (display-buffer-in-side-window help-buffer
				     `((side . right) (slot . 0)
				       (window-width . fit-window-to-buffer)
				       (preserve-size . (t . nil)))))))

(defun maxima-get-help (inferior-process)
  "Get help on a given subject.
Within the context of INFERIOR-PROCESS"
  (let ((pt)
	(name))
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward "* ")
      (setq pt (point))
      (search-forward ":")
      (skip-chars-backward ": ")
      (setq name (buffer-substring-no-properties pt (point))))
    (maxima-get-info-on-subject inferior-process name)))

(defun maxima-help (&optional arg)
  "Get help of the desired symbol.
If ARG is t it get the symbol at point."
  (interactive "P")
  (let* ((cw (current-word))
	 (completion-list (maxima-get-completions cw maxima-auxiliary-inferior-process t))
	 (subj nil))
    (if (not(seq-empty-p completion-list))
	(progn
	  (setq subj
		(if arg
                    cw
		  (completing-read (format "Maxima help(%s): " cw)
				   completion-list)))
	  (maxima-get-info-on-subject maxima-auxiliary-inferior-process subj))
      (message "No help for \" %s \"" cw))))

(defun maxima-help-at-point ()
  "Get help at point, calling `maxima-help' with t argument."
  (interactive)
  (maxima-help t))

(defun maxima-apropos (&optional arg)
  "Get help on a certain subject.
To call it with ARG use `maxima-apropos-at-point'"
  (interactive "P")
  (let* ((cw (current-word))
         (expr (if arg
                   cw
                 (read-string (concat "Maxima help (" cw "): ")
                              nil nil cw)))
         (maxima-help-buffer
	  (get-buffer-create (concat "*Maxima Help*")))
	 (have-info nil)
	 expr-line)
    (set-buffer maxima-help-buffer)
    (maxima-remove-kill-buffer-hooks)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert "Maxima help for " expr "\n\n")
    (insert "[RET] will get help on the subject on the given line\n")
    (insert "q in the *info* buffer will return you here.\n")
    (insert "q in this buffer will exit Maxima help\n\n")
    ;;FIXME We can use `maxima-get-info-on-subject' for this task.
    (with-temp-buffer
      (require 'info)
      (Info-mode)
      (Info-goto-node "(Maxima)Function and Variable Index")
      (goto-char (point-min))
      (search-forward "Menu:")
      (forward-line 1)
      (beginning-of-line)
      (while (re-search-forward (concat "\\*.*" expr ".*:") nil t)
        (setq have-info t)
        (setq expr-line  (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position)))
        (with-current-buffer maxima-help-buffer
          (insert expr-line "\n"))))
    (if have-info
	(progn
	  (set-buffer maxima-help-buffer)
	  (defun maxima-help-subject ()
	    (interactive)
	    (maxima-get-help maxima-auxiliary-inferior-process))
	  (defun maxima-kill-help ()
	    (interactive)
	    (let ((buf (current-buffer)))
	      (delete-window)
              (maxima-remove-kill-buffer-hooks)
	      (kill-buffer buf)))
          (defun maxima-next-subject ()
            (interactive)
            (forward-char 1)
            (if (re-search-forward "^\\*" nil t)
                ()
              (goto-char (point-min))
              (re-search-forward "^\\*" nil t))
            (forward-char -1))
	  (use-local-map (make-sparse-keymap))
	  (define-key (current-local-map) "\C-m" 'maxima-help-subject)
	  (define-key (current-local-map) "q" 'maxima-kill-help)
          (define-key (current-local-map) "\t" 'maxima-next-subject)
	  (goto-char (point-min))
          (re-search-forward "^\\*")
          (forward-char -1)
	  (pop-to-buffer maxima-help-buffer)
          (setq buffer-read-only t))
      (kill-buffer maxima-help-buffer)
      (message (concat "No help for \"" expr "\"")))))

(defun maxima-apropos-at-point ()
  "Call `maxima-apropos' with a true argument."
  (interactive)
  (maxima-apropos t))

(defun maxima-apropos-help ()
  "Call `maxima-help-dispatcher' with both arguments nil."
  (interactive)
  (maxima-help-dispatcher nil nil))

(defun maxima-completion-help ()
  "Call `maxima-help-dispatcher' with nil and t."
  (interactive)
  (maxima-help-dispatcher nil t))

(defun maxima-help-dispatcher (&optional arg1 arg2)
  "Show symbol help depending on ARG1 and ARG2.
If ARG1 and ARG2 are nil, call `maxima-apropos-help'.  If ARG1 is
nil and ARG2 non-nil call `maxima-completion-help'."
  (interactive)
  (cond
   ((or (looking-at "[a-zA-Z_]")
	(looking-at "\\?[a-zA-Z]")
	(looking-at "%[a-zA-Z]"))
    (if arg2
	(maxima-context-help)
      (maxima-help (current-word))))
   ((looking-at "\\?")
    (maxima-get-info-on-subject maxima-auxiliary-inferior-process "\"\\?\""))
   ((looking-at "#")
    (maxima-get-info-on-subject maxima-auxiliary-inferior-process "\"#\""))
   ((looking-at "\\.")
    (maxima-get-info-on-subject maxima-auxiliary-inferior-process "\"\\.\""))
   ((looking-at "[:=!%']")
    (let ((expr (buffer-substring-no-properties
		 (maxima-special-symbol-beginning) (maxima-special-symbol-end))))
      (cond
       ((or (string= expr "%") (string= expr "%%"))
	(maxima-get-info-on-subject maxima-auxiliary-inferior-process expr)) ; % and %% are without double quotes
       ((string= expr "''")
	(maxima-get-info-on-subject maxima-auxiliary-inferior-process "\"")) ; "''" is called """ in the manual
       ((or (string= expr ":") (string= expr "::")
            (string= expr ":=") (string= expr "::=")
            (string= expr "=") (string= expr "!") (string= expr "!!"))
	(maxima-get-info-on-subject maxima-auxiliary-inferior-process (concat "\"" expr "\"")))
       (t (error "No help for %s" expr)))))
   (arg1
    (error "No help for %s" (char-to-string (char-after (point)))))
   (t					; point is behind a name? idk
    (save-excursion
      (progn
	(backward-char 1)
	(maxima-help-dispatcher t arg2))))))

(defun maxima-context-help ()
  "Provide completion help base on the context."
  (interactive)
  (let* ((stub  (current-word))
	 (completions  (maxima-get-completions (downcase stub) maxima-auxiliary-inferior-process  t)))
    (setq completions
	  (mapc
	   (function upcase) completions))
    (if (member (upcase stub) completions)
	(setq completions (list (upcase stub))))
    (cond ((null completions)
	   (message "No help for %s" stub))
	  ((= 1 (length completions))
	   (maxima-get-info-on-subject maxima-auxiliary-inferior-process (car completions)))
	  (t				; There's no unique completion.
	   (maxima-help-variation completions)))))

;; FIXME Code duplication with `maxima-apropos'
(defun maxima-help-variation (completions)
  "Get help on certain subjects, with COMPLETIONS as a arg."
  (let* ((maxima-help-buffer
	  (get-buffer-create (concat "*Maxima Help*")))
	 expr-line)
    (set-buffer maxima-help-buffer)
    (erase-buffer)
    (insert "Maxima help\n")
    (insert "[RET] will get help on the subject on the given line\n")
    (insert "q in the *info* buffer will return you here.\n")
    (insert "q in this buffer will exit Maxima help\n\n")
    (with-temp-buffer
      (require 'info nil t)
      (Info-mode)
      (Info-goto-node "(Maxima)Function and Variable Index")
      (goto-char (point-min))
      (search-forward "Menu:")
      (forward-line 1)
      (beginning-of-line)
      (mapc (lambda (expr)
	      (re-search-forward (concat "\\* " expr ":") nil t)
	      (setq expr-line  (buffer-substring-no-properties
				(line-beginning-position)
				(line-end-position)))
	      (with-current-buffer maxima-help-buffer
		(insert expr-line "\n")))
	    completions))
    (goto-char (point-min))
    (defun maxima-help-subject ()
      (interactive)
      (maxima-get-help maxima-auxiliary-inferior-process))
    (defun maxima-kill-help ()
      (interactive)
      (let ((buf (current-buffer)))
	(delete-window)
        (maxima-remove-kill-buffer-hooks)
	(kill-buffer buf)))
    (use-local-map (append (make-sparse-keymap) (current-local-map)))
    (define-key (current-local-map) "\C-m" 'maxima-help-subject)
    (define-key (current-local-map) "q" 'maxima-kill-help)
    (goto-char 1)
    (pop-to-buffer maxima-help-buffer)))

(defun maxima-info ()
  "Read the info file for Maxima."
  (interactive)
  (info-other-window "Maxima"))

;;; The help map

(defvar maxima-help-map nil)
(if maxima-help-map
    nil
  (let ((map (make-sparse-keymap)))
    (define-key map "h" 'maxima-help)
    (define-key map "s" 'maxima-symbol-doc)
    (define-key map "d" 'maxima-completion-help)
    (define-key map "\C-d" 'maxima-completion-help)
    (define-key map "i" 'maxima-info)
    (define-key map "\C-i" 'maxima-info)
    (define-key map "m" 'maxima-info)
    (define-key map "\C-m" 'maxima-info)
    (define-key map "a" 'maxima-apropos)
    (define-key map "\C-a" 'maxima-apropos)
    (define-key map "p"  'maxima-apropos-help)
    (define-key map "\C-p"  'maxima-apropos-help)
    (setq maxima-help-map map)))

;;;; Completion

(defun maxima-get-completions (prefix inferior-process &optional fuzzy)
  "Return a list of completions with PREFIX from INFERIOR-PROCESS.
If FUZZY is non-nil, it return all the apropos without the prefix
filter."
  (unless inferior-process
    (maxima-init-inferiors))
  (let* ((command-output nil)
	 (command-list-raw nil))
    (if (>= (length prefix) 1)
	(remove "" (progn
		     (maxima-send-block (concat "apropos(\""prefix"\");") inferior-process)
		     (setq command-output (maxima-last-output-noprompt inferior-process))
		     (setq command-list-raw (seq-map (lambda (string)
						       (string-remove-suffix "]"
									     (string-remove-prefix "[" (string-trim string))))
						     (split-string command-output ",")))
		     (if fuzzy
			 command-list-raw
		       (seq-filter (lambda (str) (string-prefix-p prefix str))
				   command-list-raw))))
      '())))

;;RX (rx (literal ".") (or (literal "lisp") (literal "mac")))

(defun maxima-get-libraries (prefix &optional fuzzy)
  "Return a list of libraries in `maxima-libraries-directory' with PREFIX.
It also have an option for FUZZY search."
  (let* ((libraries-list (seq-map (lambda (file) (file-name-base file))
				  (directory-files-recursively
				   maxima-libraries-directory
				   "\\.\\(?:lisp\\|mac\\)"))))
    (seq-filter (lambda (file-name)
		  (if fuzzy
		      (string-match-p prefix file-name)
		    (string-prefix-p prefix file-name)))
		libraries-list)))

(defun maxima-complete-symbol ()
  "Complete word from list of candidates.
A completions listing will be shown in a help buffer
if completion is ambiguous."
  (let* ((stub  (buffer-substring-no-properties
                 (maxima-name-beginning) (point)))
	 (completions (maxima-get-completions stub maxima-auxiliary-inferior-process t)))
    (completion-in-region (maxima-name-beginning) (point) completions)))

(defun maxima-complete-filename ()
  "Complete the filename."
  (comint-dynamic-complete-filename))

(defun maxima-complete ()
  "Complete the current object, depending on context."
  (interactive)
  (let* ((pmin (maxima-form-beginning-position))
         (pps (parse-partial-sexp pmin (point))))
    (cond
     ;; complete filename if the point is in a string
     ((nth 3 pps)
      (maxima-complete-filename))
     ;; Otherwise, complete the symbol
     (t
      (maxima-complete-symbol)))))

;;;; Miscellaneous

;; FIXME This doesn't seem to work
(defun maxima-mark-form ()
  "Make the current form as the region."
  (interactive)
  (maxima-goto-beginning-of-form)
  (set-mark (maxima-form-end-position-or-point-max)))

(defun maxima-check-commas (beg end)
  "Check for a stray comma at the beginning or end.
It uses BEG and END as a parameters."
  (let ((commapt nil))
    (save-excursion
      (goto-char beg)
      (maxima-forward-over-comment-whitespace)
      (if (looking-at ",")
          (setq commapt (point))
        (goto-char end)
        (maxima-back-over-comment-whitespace)
        (when (save-excursion
                (forward-char -1)
                (looking-at "[;$]"))
          (forward-char -1)
          (maxima-back-over-comment-whitespace))
        (forward-char -1)
        (if (looking-at ",")
            (setq commapt (point)))))
    (if commapt
        (progn
          (message "Misplaced comma")
          (goto-char commapt)
          nil)
      t)))

(defun maxima-check-parens-region (beg end)
  "Make sure that the parentheses are balanced in the region.
It uses BEG and END as a parameters."
  (interactive "r")
  (let* ((string (buffer-substring-no-properties beg end))
         (match)
         (pt)
         (errmessage nil)
         (parenstack nil))
    (with-temp-buffer
      (maxima-mode)
      (modify-syntax-entry ?/ ". 14")
      (modify-syntax-entry ?* ". 23")
      (insert string)
      (goto-char (point-min))
      (while (and (not errmessage)
                  (setq match (maxima-re-search-forward "[][()]" end)))
	(unless (save-excursion
                  (forward-char -1)
                  (maxima-escaped-char-p))
          (cond
           ((string= match "(")
            (setq parenstack (cons (cons 1 (1- (point))) parenstack)))
           ((string= match "[")
            (setq parenstack (cons (cons 2 (1- (point))) parenstack)))
           ((string= match ")")
            (cond
             ((not parenstack)
              (setq errmessage "Unmatched close parenthesis")
              (setq pt (1- (point))))
             ((= (caar parenstack) 1)
              (setq parenstack (cdr parenstack)))
             ((= (caar parenstack) 2)
              (setq errmessage "Open bracket closed by parenthesis")
              (setq pt (1- (point))))))
           ((string= match "]")
            (cond
             ((not parenstack)
              (setq errmessage "Unmatched close bracket")
              (setq pt (1- (point))))
             ((= (caar parenstack) 2)
              (setq parenstack (cdr parenstack)))
             ((= (caar parenstack) 1)
              (setq errmessage "Open parenthesis closed by bracket")
              (setq pt (1- (point))))))))))
    (cond
     ((not (or parenstack errmessage))
      t)
     (errmessage
      (message errmessage)
      (goto-char (1- (+ beg pt)))
      nil)
     (t
      (cond
       ((= (caar parenstack) 1)
        (message "Unmatched open parenthesis")
        (goto-char (1- (+ beg (cdar parenstack))))
        nil)
       (t
        (message "Unmatched open bracket")
        (goto-char (+ beg (cdar parenstack)))
        nil))))))


(defun maxima-check-form-parens ()
  "Check to see if the parentheses in the current form are balanced."
  (interactive)
  (maxima-check-parens-region (maxima-form-beginning-position)
			      (maxima-form-end-position-or-point-max)))

(defun maxima-load-file (file)
  "Prompt for a Maxima FILE to load."
  (interactive "fMaxima file: ")
  (maxima-string (concat "load(\"" (expand-file-name file) "\");")))

(defun maxima-load-current-file ()
  "Load the current file into Maxima."
  (interactive)
  (maxima-string (concat "load(\"" buffer-file-name "\");")))

;;; For highlighting the region being sent

;; FIXME Check this functionality
(defun maxima-mode-add-highlight ()
  "Add highlight to a `maxima-mode' buffer."
  (maxima-mode-remove-highlight)
  (if (and maxima-mode-region-begin maxima-mode-region-end)
      (setq maxima-mode-highlight
            (make-overlay
             maxima-mode-region-begin
             maxima-mode-region-end))
    (overlay-put maxima-mode-highlight 'face 'highlight))
  (setq maxima-mode-region-begin nil)
  (setq maxima-mode-region-end nil))

(defun maxima-mode-remove-highlight ()
  "Remove highlight to a `maxima-mode' buffer."
  (when maxima-mode-highlight
    (delete-overlay maxima-mode-highlight)
    (setq maxima-mode-highlight nil)))

(defun maxima-mode-add-remove-highlight ()
  "Check whether or not add or remove highlight."
  (if (or
       (eq this-command 'maxima-send-region)
       (eq this-command 'maxima-send-buffer)
       (eq this-command 'maxima-send-line)
       (eq this-command 'maxima-send-form)
       (eq this-command 'maxima-send-previous-form)
       (eq this-command 'maxima-send-previous-form-and-goto-end-of-form)
       (eq this-command 'maxima-send-full-line)
       (eq this-command 'maxima-send-full-line-and-goto-next-form)
       (eq this-command 'maxima-send-completed-region)
       (eq this-command 'maxima-send-completed-region-and-goto-next-form)
       (eq this-command 'maxima-minibuffer-on-region)
       (eq this-command 'maxima-minibuffer-on-form)
       (eq this-command 'maxima-minibuffer-on-line))
      (maxima-mode-add-highlight)
    (maxima-mode-remove-highlight)))

;;;; Syntax table

;; FIXME check this functionality
(defvar maxima-mode-syntax-table nil
  "Maxima-mode syntax table.")

(if (not maxima-mode-syntax-table)
    (setq maxima-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?_ "w" maxima-mode-syntax-table)
  (modify-syntax-entry ?% "w" maxima-mode-syntax-table)
  (modify-syntax-entry ?? "w" maxima-mode-syntax-table)
  (modify-syntax-entry ?  "    " maxima-mode-syntax-table)
  (modify-syntax-entry ?\t "   " maxima-mode-syntax-table)
  (modify-syntax-entry ?` "'   " maxima-mode-syntax-table)
  (modify-syntax-entry ?' "'   " maxima-mode-syntax-table)
  (modify-syntax-entry ?, "'   " maxima-mode-syntax-table)
  (modify-syntax-entry ?. "w" maxima-mode-syntax-table)
  (modify-syntax-entry ?# "'   " maxima-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\" maxima-mode-syntax-table)
  (modify-syntax-entry ?/ ". 14" maxima-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" maxima-mode-syntax-table)
  (modify-syntax-entry ?+ "." maxima-mode-syntax-table)
  (modify-syntax-entry ?- "." maxima-mode-syntax-table)
  (modify-syntax-entry ?= "." maxima-mode-syntax-table)
  (modify-syntax-entry ?< "." maxima-mode-syntax-table)
  (modify-syntax-entry ?> "." maxima-mode-syntax-table)
  (modify-syntax-entry ?& "." maxima-mode-syntax-table)
  (modify-syntax-entry ?| "." maxima-mode-syntax-table)
  (modify-syntax-entry ?\" "\"    " maxima-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\   " maxima-mode-syntax-table)
  (modify-syntax-entry ?\( "()  " maxima-mode-syntax-table)
  (modify-syntax-entry ?\) ")(  " maxima-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]  " maxima-mode-syntax-table)
  (modify-syntax-entry ?\] ")[  " maxima-mode-syntax-table))


;;;; Keymap

;; FIXME Add the new functions to the keymap
(defvar maxima-mode-map nil
  "The keymap for `maxima-mode'.")

;; FIXME Use kbd for clearance
(if maxima-mode-map
    nil
  (let ((map (make-sparse-keymap)))
    ;; Motion
    (define-key map "\M-\C-a" 'maxima-goto-beginning-of-form)
    (define-key map "\M-\C-e" 'maxima-goto-end-of-form)
    (define-key map "\M-\C-b" 'maxima-goto-beginning-of-list)
    (define-key map "\M-\C-f" 'maxima-goto-end-of-list)
    ;; Process
    (define-key map "\C-c\C-p" 'maxima-display-buffer)
    (define-key map "\C-c\C-r" 'maxima-send-region)
    (define-key map "\C-c\C-b" 'maxima-send-buffer)
    (define-key map "\C-c\C-c" 'maxima-send-line)
    (define-key map "\C-c\C-e" 'maxima-send-previous-form)
    (define-key map "\C-c\C-s" 'maxima-send-previous-form-and-goto-end-of-form)
    (define-key map [(control return)]
      'maxima-send-full-line-and-goto-next-form)
    (define-key map [(meta return)]
      'maxima-send-completed-region-and-goto-next-form)
    (define-key map [(control meta return)] 'maxima-send-buffer)
    (define-key map "\C-c\C-k" 'maxima-stop)
    (define-key map "\C-c\C-q" 'maxima-clear-queue)
    (define-key map "\C-c\C-l" 'maxima-load-file)
    (define-key map "\C-c\C-f" 'maxima-load-current-file)
    ;; Completion
					;(if maxima-use-dynamic-complete
					;    (define-key map (kbd "M-TAB") 'maxima-dynamic-complete)
    (define-key map (kbd "M-TAB") 'maxima-complete)
    ;; Commenting
    (define-key map "\C-c;" 'comment-region)
    (define-key map "\C-c:" 'maxima-uncomment-region)
    (define-key map "\M-;" 'maxima-insert-short-comment)
    (define-key map "\C-c*" 'maxima-insert-long-comment)
    ;; Indentation
					;    (define-key map "\t" 'maxima-reindent-line)
    (define-key map "\C-m" 'maxima-return)
    (define-key map "\M-\C-q" 'maxima-indent-form)
					;    (define-key map [(control tab)] 'maxima-untab)
    ;; Help
    (define-key map "\C-c\C-d" maxima-help-map)
    (define-key map [(f12)] 'maxima-help)
    (define-key map [(meta f12)] 'maxima-apropos)
    ;; Minibuffer
    (define-key map "\C-c\C-nr" 'maxima-minibuffer-on-region)
    (define-key map "\C-c\C-nl" 'maxima-minibuffer-on-line)
    (define-key map "\C-c\C-nf" 'maxima-minibuffer-on-form)
    ;; Misc
    (define-key map "\M-h" 'maxima-mark-form)
    (define-key map (kbd "C-c (") 'maxima-check-parens-region)
    (define-key map [(control c) (control \))] 'maxima-check-form-parens)
					;    (define-key map "\C-cC-\)" 'maxima-check-form-parens)
    (define-key map "\177" 'backward-delete-char-untabify)
    (setq maxima-mode-map map)))

;;;; Menu

;; FIXME Add the new functions to the menu
(easy-menu-define maxima-mode-menu maxima-mode-map "Maxima mode menu"
  '("Maxima"
    ("Motion"
     ["Beginning of form" maxima-goto-beginning-of-form t]
     ["End of form" maxima-goto-end-of-form t]
     ["Beginning of sexp" maxima-goto-beginning-of-list t]
     ["End of sexp" maxima-goto-end-of-list t])
    ("Process"
     ["Start process" maxima-init-inferiors t]
     ["Send region" maxima-send-region t]
     ["Send buffer" maxima-send-buffer t]
     ["Send line" maxima-send-line t]
     ["Send form" maxima-send-form t]
     ["Load file" maxima-load-file t]
     "----"
     ["Display buffer" maxima-display-buffer t]
     "----"
     ["Kill main processes" maxima-stop t])
    ("Indentation"
     ["Change to basic" (maxima-change-indent-style "b")
      (not (eq maxima-indent-style 'basic))]
     ["Change to standard" (maxima-change-indent-style "s")
      (not (eq maxima-indent-style 'standard))]
     ["Change to smart" (maxima-change-indent-style "p")
      (not (eq maxima-indent-style 'perhaps-smart))])
    ("Misc"
     ["Mark form" maxima-mark-form t]
     ["Check parens in region" maxima-check-parens-region t]
     ["Check parens in form" maxima-check-form-parens t]
     ["Comment region" comment-region t]
     ["Uncomment region" maxima-uncomment-region t])
    ("Help"
     ["Maxima info" maxima-info t]
     ["Maxima apropos" maxima-apropos t]
     ["Maxima eldoc" maxima-symbol-doc t]
     ["Help" maxima-help t])))


;;;; Variable setup
;;;; (These are used in both maxima-mode and maxima-inferior-mode).

(defvar maxima-mode-abbrev-table nil
  "Abbreviation table for `maxima-mode'.")

(defun maxima-mode-variables ()
  "Set all the necessary variables for `maxima-mode'."
  (set-syntax-table maxima-mode-syntax-table)
  (setq-local local-abbrev-table maxima-mode-abbrev-table)
  (setq-local paragraph-start (concat "^$\\|" page-delimiter))
	      (setq-local	      paragraph-separate paragraph-start)
	      (setq-local     indent-line-function #'maxima-indent-line)
	      (setq-local	      case-fold-search t)
	      (setq-local     comment-start "/*")
	      (setq-local     comment-end "*/")
	      (setq-local     comment-start-skip "/\\*+ *")
	      (setq-local     comment-column 40)
	      (setq-local     comment-indent-function #'comment-indent-default)
	      (unless maxima-use-tabs
		(setq-local indent-tabs-mode nil))
	      (setq imenu-generic-expression
		    (list '(nil "^ *\\([a-zA-Z0-9_]*\\) *(.*) *:=" 1))))


;;;; Maxima mode

;; FIXME improve comment, add the "new" functions
;;;###autoload
(define-derived-mode maxima-mode prog-mode "Maxima" ()
  "Major mode for editing Maxima code.

Maxima mode provides the following motion commands:
\\[maxima-goto-beginning-of-form]: Move to the beginning of the form.
\\[maxima-goto-end-of-form]: Move to the end of the form.
\\[maxima-goto-beginning-of-list]: Move to the beginning of the sexp.
\\[maxima-goto-end-of-list]: Move to the end of the sexp.

and the following miscellaneous commands.
\\[maxima-mark-form]: Mark the current form
\\[maxima-check-parens-region]: Check the current region for balanced parentheses.
\\[maxima-check-form-parens]: Check the current form for balanced parentheses.

Maxima mode has the following completions commands:
M-TAB: Complete the Maxima symbol as much as possible, providing
     a completion buffer if there is more than one possible completion.

Portions of the buffer can be sent to a Maxima process.  (If a process is
not running, one will be started.)
\\[maxima-send-region]: Send the region to Maxima.
\\[maxima-send-buffer]: Send the buffer to Maxima.
\\[maxima-send-line]: Send the line to Maxima.
\\[maxima-send-form]: Send the form to Maxima.
\\[maxima-send-full-line-and-goto-next-form]: Send the smallest set of lines which contains
   the cursor and contains no incomplete forms, and go to the next form.
\\[maxima-send-completed-region-and-goto-next-form]:  As above, but with
   the region instead of the current line.
\\[maxima-load-file] will prompt for a filename and load it into Maxima
When something is sent to Maxima, a buffer running an inferior Maxima
process will appear.  It can also be made to appear by using the command
\\[maxima-display-buffer].
If an argument is given to a command to send information to Maxima,
the region (buffer, line, form) will first be checked to make sure
the parentheses are balanced.
The Maxima process can be killed, after asking for confirmation
with \\[maxima-stop].  To kill without confirmation, give \\[maxima-stop]
an argument.

By default, indentation will be to the same level as the
previous line, with an additional space added for open parentheses.
The behaviour of indent can be changed by the command
\\[maxima-change-indent-style].  The possibilities are:
Standard:      Standard indentation.
Perhaps smart: Tries to guess an appropriate indentation, based on
               open parentheses, \"do\" loops, etc.
The default can be set by setting the value of the variable
\"maxima-indent-style\" to either 'standard or 'perhaps-smart.
In both cases, \\[maxima-untab] will remove a level of indentation.

To get help on a Maxima topic, use:
\\[maxima-help].
To read the Maxima info manual, use:
\\[maxima-info].
To get help with the symbol under point, use:
\\[maxima-completion-help].
To get apropos with the symbol under point, use:
\\[maxima-apropos-help].

\\{maxima-mode-map}"
  (maxima-mode-variables)
  (cond
   ((eq maxima-newline-style 'basic)
    (setq maxima-indent-style 'standard))
   ((eq maxima-newline-style 'standard)
    (setq maxima-indent-style 'standard))
   ((eq maxima-newline-style 'perhaps-smart)
    (setq maxima-indent-style 'perhaps-smart)))
  (easy-menu-add maxima-mode-menu maxima-mode-map)
  (add-hook 'post-command-hook
            'maxima-mode-add-remove-highlight nil t))

;;;; Interacting with the Maxima process

;;; Checking on the process
(defun maxima-inferior-running (inferior-process)
  "Check if INFERIOR-PROCESS is running."
  (and (processp inferior-process)
       (eq (process-status inferior-process) 'run)))

;;; Sending the information
(defun maxima-inferior-get-old-input ()
  "Get last output from a `maxima-inferior-mode' buffer."
  (let (pt pt1)
    (save-excursion
      (if (re-search-forward
           (concat "\\(^(\\(" maxima-outchar "\\|" maxima-linechar "\\)[0-9]*) \\)")
           nil 1)
          (goto-char (match-beginning 0)))
      (skip-chars-backward " \t\n")
      (setq pt (point)))
    (save-excursion
      (re-search-backward maxima-inferior-prompt)
      (setq pt1 (match-end 0)))
    (buffer-substring-no-properties pt1 pt)))

(defun maxima-inferior-comint-send-input (&optional query)
  "Take note of position, then send the input.
If QUERY is not nil, it takes the input in point."
  (unless query
    (setq maxima-inferior-input-end (point)))
  (setq maxima-inferior-waiting-for-output t)
  (comint-send-input))

(defun maxima-inferior-remove-double-input-prompt (&optional _string)
  "Fix the double prompt that occasionally appears in Emacs."
  (let* ((buffer (current-buffer))
	 (inferior-process (get-buffer-process buffer)))
    (when (processp inferior-process)
      (with-current-buffer buffer
	(goto-char maxima-inferior-input-end)
	(forward-line 1)
	(if (looking-at (concat "(" maxima-inchar "[0-9]+)"))
	    (kill-line 1))
	(if (looking-at "
")
	    (delete-char 1))))))

(defun maxima-inferior-replace-tabs-by-spaces (&optional _string)
  "Replace tabs in the Maxima output by spaces."
  (let* ((beg)
	 (buffer (current-buffer))
	 (inferior-process (get-buffer-process buffer)))
    (when (processp inferior-process)
      (with-current-buffer buffer
	(if (marker-position comint-last-output-start)
	    (setq beg comint-last-output-start)
	  (setq beg (point-min)))
	(untabify beg
		  (process-mark inferior-process))))))

(defun maxima-inferior-wait-for-output (inferior-process)
  "Wait for output from INFERIOR-PROCESS."
  ;; FIXME This is a hack, the output must be check
  ;; correctly.
  (sleep-for 0.05)
  (when
      (and
       maxima-inferior-waiting-for-output
       (maxima-inferior-running inferior-process))
    (accept-process-output inferior-process)))

(defun maxima-inferior-output-filter (str)
  "Look for a new input prompt.
It requires STR."
  (let* ((buffer (current-buffer))
	 (inferior-process (get-buffer-process buffer)))
    (cond ((and
            (string-match "Still waiting:" str)
            (not (string-match (concat "(" maxima-outchar "[0-9]+)") str)))
	   ;; FIXME This is a workaround, it may be some cases
	   ;; that "all" doesn't work.
	   (if (equal maxima-auxiliary-inferior-process inferior-process)
	       (maxima-send-block "all" inferior-process)
	     (maxima-ask-question str inferior-process)))
	  ((string-match maxima-inferior-prompt str)
	   (if (and inferior-process (not (string= maxima-block "")))
	       (maxima-single-string (maxima-get-command) inferior-process)
	     (if (not inferior-process)
		 (maxima-clear-queue))
	     (setq maxima-inferior-waiting-for-output nil))))))

(defun maxima-inferior-sentinel (_proc state)
  "Write the input history when the process ends.
It requires PROC and STATE."
  (unless (string-match "^run" state)
    (comint-write-input-ring)))

;; RX
;; (rx (or
;;      ;;block,load,loadfile function match
;;      (group
;;       (or (literal "block")
;; 	  (literal "load")
;; 	  (literal "loadfile")
;; 	  (literal "kill")) (* space) (literal "(") (* anything) (literal ")")(or (literal ";") (literal "$")))
;;      ;;:lisp construct match
;;      (group
;;       (literal ":lisp") (* anything) eol)
;;      ;; Variable definition match
;;      (group bol (* alnum) (literal ":") (* anything) (or (literal ";") (literal "$")))
;;      ;; Function definition match
;;      (group (+ alnum)(syntax open-parenthesis)
;; 	    (+ alnum (? (literal ",") ) (? (literal "[")) (? (literal "]")))
;; 	    (literal ")") (* space) (literal ":=") (* anything)
;; 	    (or (literal ";") (literal "$")))))

(defun maxima-inferior-auxiliar-filter (user-string)
  "Check if USER-STRING is allow in the auxiliar-process.
This prevents gnuplot or similar functions to show duplicates
graphics and allow only some commands.  This only affects user
sending commands throw `maxima-string'"
  (let* ((auxiliar-regex
	  "\\(\\(?:block\\|load\\|loadfile\\|kill\\)[[:space:]]*([^z-a]*)\\(?:;\\|\\$\\)\\)\\|\\(:lisp[^z-a]*$\\)\\|\\(^[[:alnum:]]*:[^z-a]*\\(?:;\\|\\$\\)\\)\\|\\([[:alnum:]]+\\s(\\(?:[[:alnum:]],?\\[?]?\\)+)[[:space:]]*:=[^z-a]*\\(?:;\\|\\$\\)\\)"))
    (string-match-p auxiliar-regex user-string)))

(defun maxima-make-inferior (name &optional test)
  "Create an `maxima-inferior-mode' process and return it.
Creates the process with the command defined in `maxima-command'
with the args defined in `maxima-args' with the name in NAME.
Is possible to attach functions to `comint-output-filter-functions'
to pass a list of functions in COMINT-FILTER-FUNCTIONS.
The TEST variable is for test purpose."
  (let* ((proc-buf)
	 (cmd)
	 (proc))
    (setq maxima-inferior-input-end 0)
    (setq maxima-inferior-waiting-for-output t)
    (if maxima-args
	(setq cmd
	      (append (list 'make-comint name maxima-command
			    nil)
		      (split-string maxima-args)))
      (setq cmd (list 'make-comint name maxima-command)))
    (setq proc-buf (eval cmd))
    (with-current-buffer proc-buf
      (unless (seq-empty-p maxima-comint-output-functions)
	(seq-map (lambda (filter-function)
		   (add-hook 'comint-output-filter-functions
			     filter-function nil t))
		 maxima-comint-output-functions))
      (maxima-inferior-mode))
    (setq proc (get-buffer-process proc-buf))
    (unless test
      (maxima-inferior-wait-for-output proc))
    proc))

;;;###autoload
(defmacro maxima-remove-inferior (inferior-process)
  "Remove the INFERIOR-PROCESS and the process buffer."
  `(let* ((inferior-buffer (get-buffer (process-buffer ,inferior-process))))
     (delete-process ,inferior-process)
     (kill-buffer inferior-buffer)))

;;;###autoload
(defmacro maxima-start (inferior-symbol name)
  "Start a maxima process and save the process in INFERIOR-SYMBOL.
The process name is passed in NAME."
  `(setq ,inferior-symbol (maxima-make-inferior ,name)))

(defun maxima-init-inferiors ()
  "Check if the main inferior process exists.
The inferior processes are defined inside
the variables `maxima-inferior-process' and `maxima-auxiliary-inferior-process'.
This functions assigns process to those variables."
  (interactive)
  (unless (and (get-process "maxima")
	       maxima-inferior-process)
    (maxima-start maxima-inferior-process "maxima"))

  (unless (and (get-process "aux-maxima")
	       maxima-auxiliary-inferior-process)
    (maxima-start maxima-auxiliary-inferior-process "aux-maxima")))


(defun maxima-stop (&optional arg)
  "Kill the main inferior.
The \"main inferiors\" are defined in the variables
`maxima-inferior-process' and
`maxima-auxiliary-inferior-process'.
If ARG is t, the confirmation is omitted."
  (interactive)
  (if (processp maxima-inferior-process)
      (if arg
	  (progn
	    (maxima-remove-inferior maxima-inferior-process)
	    (setq maxima-inferior-process nil)
	    (maxima-remove-inferior maxima-auxiliary-inferior-process )
	    (setq maxima-auxiliary-inferior-process nil))
	(when (y-or-n-p "Really quit Maxima? ")
	  (maxima-remove-inferior maxima-inferior-process)
	  (setq maxima-inferior-process nil)
	  (maxima-remove-inferior maxima-auxiliary-inferior-process)
	  (setq maxima-auxiliary-inferior-process nil)))))

;;; Sending information to the process

(defun maxima-single-string (string inferior-process)
  "Send STRING to INFERIOR-PROCESS."
  (let* ((strip-string (maxima-strip-string-add-semicolon string)))
    (with-current-buffer (process-buffer inferior-process)
      (goto-char (point-max))
      (let ((start (point)))
	(insert strip-string)
	(untabify start (point)))
      (goto-char (point-max))
      (maxima-inferior-comint-send-input)
      (goto-char (point-max)))))

(defun maxima-ask-question (string inferior-process)
  "Ask the STRING question maxima wants answered.
Send the answer to INFERIOR-PROCESS."
  (let ((ans))
    (setq ans (read-from-minibuffer
	       string nil nil nil nil nil t))
    (unless (string-match "[;$]" ans)
      (setq ans (concat ans ";")))
    (setq ans (maxima-strip-string ans))
    (save-current-buffer
      (set-buffer (process-buffer inferior-process))
      (goto-char (point-max))
      (insert ans)
      (maxima-inferior-comint-send-input t)
      (goto-char (point-max)))))

(defun maxima-get-command (&optional arg)
  "Return the maxima command that's at the front of `maxima-block'.
Remove it from the front of `maxima-block'.
With ARG, use `maxima-block-wait' instead of `maxima-block'."
  (let* ((pt)
         (command))
    (with-temp-buffer
      (if arg
          (insert maxima-block-wait)
	(insert maxima-block))
      (goto-char (point-min))
      (maxima-forward-over-comment-whitespace)
      (setq pt (point))
      (if (string-match "[$;]\\|:lisp"
			(buffer-substring-no-properties (point) (point-max)))
          (progn
            (if (looking-at ":lisp")
		(progn
                  (search-forward ":lisp")
                  (forward-sexp)
                  (setq command (buffer-substring-no-properties pt (point))))
              (maxima-goto-end-of-form)
              (setq command (buffer-substring-no-properties pt (point))))
            (maxima-forward-over-comment-whitespace)
            (if arg
		(setq maxima-block-wait
                      (maxima-strip-string-add-semicolon
                       (buffer-substring-no-properties (point) (point-max))))
              (setq maxima-block
                    (maxima-strip-string-add-semicolon
                     (buffer-substring-no-properties (point) (point-max)))))
            (setq command (buffer-substring-no-properties pt (point))))
	(if arg
            (setq maxima-block-wait "")
          (setq maxima-block "")))
      (if arg
          (if (string= maxima-block-wait ";") (setq maxima-block-wait ""))
	(if (string= maxima-block ";") (setq maxima-block ""))))
    command))

(defun maxima-send-block (stuff inferior-process)
  "Send a STUFF block of code to INFERIOR-PROCESS."
  (let* ((strip-stuff (maxima-strip-string-add-semicolon stuff)))
    (if (string= maxima-block "")
	(progn
          (setq maxima-block strip-stuff)
	  (maxima-single-string (maxima-get-command) inferior-process))
      (setq maxima-block (concat maxima-block strip-stuff)))))

;; FIXME Adapt this to the process architecture.
(defun maxima-send-block-wait (stuff)
  "Send a STUFF block of code to Maxima; wait for it to finish.
Return the last string sent."
  (if (not (string= maxima-block ""))
      (message "Maxima process currently busy.")
    (setq maxima-block-wait (maxima-strip-string-add-semicolon stuff))
    (while (not (string= maxima-block-wait ""))
      (maxima-single-string-wait (maxima-get-command t)))))

(defun maxima-clear-queue ()
  "Clear out the queue of commands to send to the maxima process."
  (interactive)
  (setq maxima-block "")
  (setq maxima-block-wait ""))

;;; Getting information back from Maxima.

(defun maxima-last-output (inferior-process)
  "Get the most recent output from a Maxima INFERIOR-PROCESS."
  (interactive)
  (maxima-inferior-wait-for-output inferior-process)
  (let* ((proc-buffer (process-buffer inferior-process)))
    (with-current-buffer proc-buffer
      (let* ((pt (point))
             (pmark (progn (goto-char (process-mark inferior-process))
                           (forward-line 0)
                           (point-marker)))
             (beg (progn
                    (goto-char maxima-inferior-input-end)
                    (forward-line 1)
                    (point)))
             (output (buffer-substring-no-properties beg pmark)))
	(goto-char pt)
	output))))

  (defun maxima-last-output-noprompt (inferior-process)
    "Return the last Maxima output from INFERIOR-PROCESS.
Without the prompts."
    (interactive)
    (if (not (maxima-inferior-running inferior-process))
	(maxima-last-output inferior-process)
      (let* ((output (maxima-last-output inferior-process))
             (newstring)
             (i 0)
             (beg)
             (end)
             (k))
	;; Replace the output prompt with spaces
	(setq beg (string-match
                   (concat "\\(^(" maxima-outchar "[0-9]*) \\)") output))
	(if (not beg)
            output
          (setq end (1+ (string-match ")" output beg)))
          (setq newstring (substring output 0 beg))
          (setq k (- end beg))
          (while (< i k)
            (setq newstring (concat newstring " "))
            (setq i (1+ i)))
          (concat newstring
                  (substring output
                             end))))))

(defun maxima-last-output-tex-noprompt (inferior-process)
  "Return the last INFERIOR-PROCESS output, between the dollar signs."
  (interactive)
  (let* ((output (maxima-last-output inferior-process))
         (begtex (string-match "\\$\\$" output))
         (endtex (string-match "\\$\\$" output (1+ begtex))))
    (concat
     (substring output begtex (+ endtex 2))
     "\n")))


;;; Sending information to the process should be done through these
;; next five commands

;; FIXME This command...
(defun maxima-single-string-wait (string)
  "Send a single STRING to the maxima process.
Waiting for output after."
  (maxima-inferior-wait-for-output maxima-inferior-process)
  (maxima-single-string string maxima-inferior-process)
  (maxima-inferior-wait-for-output maxima-inferior-process))

(defun maxima-string (string)
  "Send a STRING to `maxima-inferior-process'.
It also checks with `maxima-inferior-auxiliar-filter' if the
string is going to be sent to
`maxima-auxiliary-inferior-process'."
  (unless maxima-inferior-process
    (maxima-init-inferiors))
  (maxima-send-block string maxima-inferior-process)
  (when (maxima-inferior-auxiliar-filter string)
    (maxima-send-block string maxima-auxiliary-inferior-process)))

(defun maxima-region (beg end)
  "Send the region between BEG and END to the Maxima process."
  (setq maxima-mode-region-begin beg)
  (setq maxima-mode-region-end end)
  (maxima-string
   (buffer-substring-no-properties beg end)))

;;; Some functions to send commands to the process.

(defun maxima-send-region (beg end &optional arg)
  "Send the current region between BEG and END to the Maxima process.
With ARG , don't check the parentheses first."
  (interactive "r\nP")
  (if arg
      (maxima-region beg end)
    (if (maxima-check-parens-region beg end)
        (maxima-region beg end)))
  (when maxima-display-maxima-buffer
    (maxima-display-buffer)))

(defun maxima-send-buffer (&optional arg)
  "Send the buffer to the Maxima process, after checking the parentheses.
With ARG, don't check the parentheses."
  (interactive "P")
  (maxima-send-region (point-min) (point-max) arg))

(defun maxima-send-line (&optional arg)
  "Send the current line to the Maxima process, after checking parentheses.
With ARG, don't check parentheses."
  (interactive "P")
  (let ((b (line-beginning-position))
	(e (line-end-position)))
    (maxima-send-region b e arg)))

(defun maxima-send-form (&optional arg)
  "Send the current form to the Maxima process, checking parentheses.
With ARG, don't check parentheses."
  (interactive "P")
  (maxima-send-region (maxima-form-beginning-position)
                (maxima-form-end-position-or-point-max) arg))

(defun maxima-send-previous-form (&optional arg)
  "Send the previous form to the Maxima process, checking parentheses.
With ARG, don't check parentheses."
  (interactive "P")
  (save-excursion
    (if (maxima-re-search-backward "[;$]")
        (maxima-send-region (maxima-form-beginning-position)
                      (maxima-form-end-position-or-point-max) arg)
      (message "No previous form."))))

(defun maxima-send-previous-form-and-goto-end-of-form (&optional arg)
  "Send the previous ARG form to the Maxima process.
Then go to the end of form."
  (interactive "P")
  (maxima-send-previous-form arg)
  (maxima-goto-end-of-form))

(defun maxima-send-full-line ()
  "Send the minimum number of lines such that the current is one of them.
Also that no line contains an incomplete form."
  (interactive)
  (let ((beg (point)) (end (point)))
    (save-excursion
      (goto-char beg)
      (beginning-of-line)
      (setq beg (point))
      (maxima-goto-beginning-of-form)
      (while (< (point) beg)
	(progn
	  (beginning-of-line)
	  (setq beg (point))
	  (maxima-goto-beginning-of-form)))
      (goto-char end)
      (end-of-line)
      (setq end (point))
      (while (and (< (maxima-form-beginning-position) end) (< end (point-max)))
	(progn
	  (forward-line 1)
	  (end-of-line)
	  (setq end (point))))
      (skip-chars-backward " \t;$")
      (if (re-search-forward "[;$]" end t)
	  (maxima-send-region beg (point))
	(error "No ; or $ at end"))
      end)))

(defun maxima-send-full-line-and-goto-next-form ()
  "Do a `maxima-send-full-line' and go to the beginning of the next form."
  (interactive)
  (goto-char (maxima-send-full-line))
  (maxima-goto-beginning-of-form))

(defun maxima-send-completed-region (beg end)
  "Send the marked region between BEG and END.
Also complete possibly non-complete forms at the bounderies."
  (interactive "r\nP")
  (let ((beg1)
        (end1))
    (save-excursion
      (goto-char beg)
      (setq beg1 (maxima-form-beginning-position))
      (goto-char end)
      (setq end1 (maxima-form-end-position-or-point-max))
      (maxima-send-region beg1 end1)
      end1)))

(defun maxima-send-completed-region-and-goto-next-form (beg end)
  "Do a `maxima-send-completed-region' between BEG and END.
Then go to the beginning of the next form."
  (interactive "r\nP")
  (goto-char (maxima-send-completed-region beg end))
  (maxima-goto-beginning-of-form))

(defun maxima-display-buffer ()
  "Display the `maxima-inferior-process' buffer so the recent output is visible."
  (interactive)
  (let ((origbuffer (current-buffer)))
    (if (not (processp maxima-inferior-process))
	(maxima-init-inferiors))
    (pop-to-buffer (process-buffer maxima-inferior-process))
    (goto-char (point-max))
    (pop-to-buffer origbuffer)))


;;;; The inferior Maxima process

;;; Completions from previous input

;; First, a function to take the comint-input-ring and return a
;; list of previous inputs

(defun maxima-inferior-previous-inputs ()
  "Return a list of previous inputs."
  (interactive)
  (let* ((comint-inputs
	  (seq-remove 'null (cddr comint-input-ring))))
    (seq-uniq comint-inputs)))

;; FIXME this doesn't work at all.
(defun maxima-inferior-input-complete ()
  "Complete line from list of previous input."
  (interactive)
  (let* ((stub  (buffer-substring-no-properties
                 (maxima-inferior-bol-position) (point)))
	 (completions (all-completions (downcase stub)
                                       (maxima-inferior-previous-inputs))))
    (cond ((null completions)
	   (message "No completions of %s" stub))
	  ((= 1 (length completions))	; Gotcha!
	   (let ((completion (car completions)))
	     (if (string-equal completion stub)
		 (message "Sole completion")
	       (insert (substring completion (length stub)))
	       (message "Completed"))))
	  (t				; There's no unique completion.
           (comint-dynamic-list-completions completions)))))

(defun maxima-inferior-complete ()
  "Complete the current object, depending on context."
  (interactive)
  (let* ((pmin (save-excursion
                 (re-search-backward maxima-inferior-prompt)
                 (point)))
         (pps (parse-partial-sexp pmin (point))))
    (cond
     ;; complete filename if the point is in a string
     ((nth 3 pps)
      (maxima-complete-filename))
     ;; Otherwise, complete the symbol
     (t
      (maxima-complete-symbol)))))

;;; Sending a line to the process while in the process buffer

(defun maxima-inferior-check-and-send-line ()
  "Check the lines for mis-matched parentheses, then send the line."
  (interactive)
  (let ((ok nil)
	(pt (point))
	pt1)
    (save-excursion
      (end-of-line)
      (skip-chars-backward " \t")
      (forward-char -1)
      (when (looking-at "[$;]")
        (setq pt (point))
        (setq ok t)))
    (if ok
	(progn
	  (save-excursion
	    (re-search-backward maxima-inferior-prompt)
	    (setq pt1 (match-end 0)))
	  (if (maxima-check-parens-region pt1 pt)
              (maxima-inferior-comint-send-input)))
      (maxima-inferior-comint-send-input))))

(defun maxima-inferior-send-line ()
  "Send the line to the Maxima process."
  (interactive)
  (maxima-inferior-comint-send-input))

(defun maxima-inferior-bol ()
  "Go to the beginning of the line, but past the prompt."
  (interactive)
  (let ((eol (save-excursion (end-of-line) (point))))
    (forward-line 0)
    (if (and (looking-at maxima-inferior-prompt)
	     (<= (match-end 0) eol))
	(goto-char (match-end 0)))))

(defun maxima-inferior-bol-position ()
  "Internal function, go to the beginning and save the point."
  (save-excursion
    (maxima-inferior-bol)
    (point)))


;;;; Inferior Maxima mode


;;;; Keymap

(defvar maxima-inferior-mode-map nil
  "The keymap for function `maxima-inferior-mode'.")

(unless maxima-inferior-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map  (kbd "C-a") 'maxima-inferior-bol)
    (define-key map  (kbd "C-m") 'maxima-inferior-check-and-send-line)
    (define-key map  (kbd "<C-return>") 'maxima-inferior-send-line)
    (define-key map  (kbd "<C-M-tab>") 'maxima-inferior-input-complete)
    (define-key map  (kbd "C-c C-s") 'maxima-inferior-complete)
    (define-key map  (kbd "\d") 'backward-delete-char-untabify)
    (define-key map  (kbd "C-c C-k") 'maxima-stop)
    (define-key map  (kbd "C-c C-d") maxima-help-map)
    (setq maxima-inferior-mode-map map)))

;;;; Menu

;; FIXME add more options to the inferior menu, like autocomplete
(easy-menu-define maxima-inferior-mode-menu maxima-inferior-mode-map
  "Maxima mode menu"
  '("Maxima"
    ("Help"
     ["Maxima info" maxima-info t]
     ["Help" maxima-help t])
    ("Quit"
     ["Kill process" maxima-stop t])))


(define-derived-mode maxima-inferior-mode
  comint-mode
  "Inferior Maxima"
  "Major mode for interacting with an inferior Maxima process.

Return will check the line for balanced parentheses, and send line as input.
Control return will send the line as input without checking for balanced
parentheses.

M-TAB will complete the Maxima symbol as much as possible, providing
     a completion buffer if there is more than one possible completion.

\\[maxima-smart-complete] will complete the input line, based on previous input lines.
\\[maxima-help] will get help on a Maxima topic.
\\[maxima-info] will bring up the Maxima info manual.
\\[maxima-stop] will kill the process and the buffer, after asking for
  confirmation.  To kill without confirmation, give \\[maxima-stop] an
  argument.

To scroll through previous commands,
\\[comint-previous-input] will bring the previous input to the current prompt,
\\[comint-next-input] will bring the next input to the prompt.
\\[comint-previous-matching-input] will bring the previous input matching
  a regular expression to the prompt,
\\[comint-next-matching-input] will bring the next input matching
  a regular expression to the prompt.
"
  (setq comint-prompt-regexp maxima-inferior-prompt)
  (setq comint-get-old-input (function maxima-inferior-get-old-input))
  (setq mode-line-process '(": %s"))
  (easy-menu-add maxima-inferior-mode-menu maxima-inferior-mode-map)
  (maxima-mode-variables)
  (setq tab-width 8)

  ;; (add-hook 'kill-buffer-hook
  ;;           (lambda ()
  ;;             (maxima-clear-queue)
  ;;             (if (processp maxima-inferior-process)
  ;;                 (delete-process maxima-inferior-process))
  ;;             (setq maxima-inferior-process nil)
  ;;             (run-hooks 'maxima-inferior-exit-hook))
  ;; 	    t t)

  (setq comint-input-ring-size maxima-input-history-length)
  (if maxima-save-input-history
      (progn
        (setq comint-input-ring-file-name maxima-input-history-file)
        (comint-read-input-ring t)
        (set-process-sentinel maxima-inferior-process
                              'maxima-inferior-sentinel)))
  (set (make-local-variable 'comint-prompt-read-only) t))

(defun maxima-hook-function ()
  (when (require 'company-maxima nil t)
    (add-to-list 'company-backends '(company-maxima-symbols company-maxima-libraries))
    (company-mode-on)))

;;;; Running Maxima

;;;###autoload
(defun maxima ()
  "Run Maxima interactively inside a buffer."
  (interactive)
  (maxima-init-inferiors)
  (switch-to-buffer (process-buffer maxima-inferior-process))
  (goto-char (point-max)))


;;; Interacting with Maxima outside of a maxima buffer

(defun maxima-minibuffer ()
  "Communicate with Maxima through the minibuffer."
  (interactive)
  (maxima-init-inferiors)
  (let ((input (read-string "Maxima: " nil maxima-minibuffer-history))
        (output nil)
        (twod maxima-minibuffer-2d))
    (setq input (maxima-strip-string-add-semicolon input))
    (if twod
        (maxima-single-string
         "block(emacsdisplay:display2d,display2d:true,linenum:linenum-1,%);" maxima-inferior-process)
      (maxima-single-string
       "block(emacsdisplay:display2d,display2d:false,linenum:linenum-1,%);" maxima-inferior-process))
    (maxima-single-string-wait input)
    (setq output (maxima-last-output-noprompt maxima-inferior-process))
    (maxima-single-string-wait "block(display2d:emacsdisplay,linenum:linenum-1,%);")
    (if (not twod)
        (setq output (string-trim-right output))
      ;; Strip the beginning and trailing newline
      (while (string-match "\\` *\n" output)
        (setq output (substring output (match-end 0))))
      (while (string-match "\n *\\'" output)
        (setq output (substring output 0 (match-beginning 0)))))
    (setq output (maxima-replace-in-string "%" "%%" output))
    (message output)))

(defun maxima-minibuffer-delete-output (beg end)
  "Delete the minibuffer output from BEG to END."
  (let ((mmom (maxima-minor-output-mark))
        (mmoe (maxima-minor-output-mark-end)))
    (if (or
         (and (string-match maxima-minor-mode-bad-delimiter-regexp mmom)
              (string= (match-string 0 mmom) mmom))
         (and (string-match maxima-minor-mode-bad-delimiter-regexp mmoe)
              (string= (match-string 0 mmoe) mmoe)))
        (message "Old output not deleted (improper delimiter).")
      (let (pt)
        (save-excursion
          (goto-char beg)
          (if (search-forward mmom end t)
              (progn
                (setq pt (match-beginning 0))
                (search-forward mmoe)
                (kill-region pt (point)))
            (goto-char end)
            (if (looking-at (concat "[ \n]*" (regexp-quote mmom)))
                (progn
                  (search-forward mmoe)
                  (kill-region end (point)))))
          (point))))))

;; FIXME this probably need a re-write
(defun maxima-minibuffer-on-region (beg end &optional arg)
  "Send the current region between BEG and END to Maxima.
Display last output in minibuffer.  With ARG, insert \" ==> \"
into the current buffer, followed by the output, followed by
\"\\\".  In this case, any previous output will be deleted."
  (interactive "r\nP")
  (let ((output nil)
        (minibufferoutput)
        (input)
        (realend nil)
        (realbeg)
        (delreg)
        (delregbeg)
        (delregend)
        (twod maxima-minibuffer-2d ))
    (save-excursion
      (goto-char beg)
      (maxima-forward-over-comment-whitespace)
      (setq realbeg (point))
      (if (re-search-forward (maxima-minor-output-mark) end t)
          (setq realend
                (if (eq major-mode 'maxima-mode)
                    (- (point) (length maxima-mode-minor-output))
                  (- (point) (length maxima-minor-output))))
        (goto-char end)
        (maxima-back-over-comment-whitespace)
        (setq realend (point))))
    (setq input (maxima-strip-string-add-semicolon
                 (buffer-substring-no-properties realbeg realend)))
    (if arg
        (maxima-minibuffer-delete-output beg end))
    (setq maxima-minor-mode-region-begin realbeg)
    (setq maxima-minor-mode-region-end realend)
    (when (or (not maxima-minor-mode-check-input)
              (and
               (maxima-check-parens-region realbeg realend)
               (maxima-check-commas realbeg realend)))
      (maxima-init-inferiors)
      (if twod
          (maxima-single-string-wait
           "block(emacsdisplay:display2d,display2d:true,linenum:linenum-1,%);")
        (maxima-single-string-wait
         "block(emacsdisplay:display2d,display2d:false,linenum:linenum-1,%);"))
      (maxima-send-block-wait input)
      (setq output (maxima-last-output-noprompt maxima-inferior-process))
      (maxima-single-string-wait "block(display2d:emacsdisplay,linenum:linenum-1,%);")
      (if (not twod)
          (setq output (string-trim-right output))
        ;; Strip the beginning and trailing newline
        (while (string-match "\\` *\n" output)
          (setq output (substring output (match-end 0))))
        (while (string-match "\n *\\'" output)
          (setq output (substring output 0 (match-beginning 0)))))
      (unless arg
        (setq minibufferoutput (maxima-replace-in-string "%" "%%" output))
        (message minibufferoutput))
      (if (and arg
               (not twod))
          (save-excursion
            (goto-char realend)
            (if (looking-at "^")
                (setq realend (1- realend)))
            (goto-char realend)
            (skip-chars-backward " \t\n")
            (unless (= (point) realend)
              (setq delreg (buffer-substring-no-properties (point) realend))
              (kill-region (point) realend)
              (cond
               ((< (length delreg) 15)
		(setq delreg (maxima-replace-in-string "\n" " " delreg))
		(message (concat "\"" delreg "\" killed")))
               (t
		(setq delregbeg
                      (maxima-replace-in-string "\n" " "(substring delreg 0 5)))
		(setq delregend
                      (maxima-replace-in-string "\n" " "(substring delreg -5)))
		(message (concat "\"" delregbeg " ... " delregend "\"  killed")))))
            (let ((ind (save-excursion
			 (goto-char realbeg)
			 (current-column)))
		  (here (point))
		  (there (make-marker)))
              (if (or
		   (string-match "\n" output)
		   (> (+ (current-column) (length output)) fill-column))
		  (progn
                    (insert "\n")
                    (setq here (point)))
		(insert " "))
              (insert (maxima-minor-output-mark) " " output
                      (maxima-minor-output-mark-end))
              (set-marker there (point))
              (goto-char here)
              (goto-char (line-end-position))
              (if (string-match
		   "\n"
		   (buffer-substring-no-properties here (point)))
		  (forward-line -1)
		(forward-line 1))
              (indent-region (point) there ind)))
	(if (and arg twod)
            (let ((ind (save-excursion
			 (goto-char realbeg)
			 (current-column)))
                  (here))
              (save-excursion
		(goto-char realend)
		(insert (maxima-minor-output-mark) "\n")
		(setq here (point))
		(insert output (maxima-minor-output-mark-end))
		(indent-region here (point) ind))))))))

(defun maxima-minibuffer-on-line (&optional arg)
  "Send the current line to Maxima; display last output in minibuffer.
With ARG, insert \" ==> \" into the current buffer,
followed by the output.  In this case, anything in the line
after any occurrence of \" ==> \" will be deleted."
  (interactive "P")
  (maxima-minibuffer-on-region
   (line-beginning-position)
   (line-end-position)
   arg))

(defun maxima-minibuffer-on-form (&optional arg)
  "Send the current form to Maxima; display last output in minibuffer.
With ARG, insert \" ==> \" into the current buffer,
followed by the output."
  (interactive "P")
  (let ((beg (maxima-form-beginning-position))
        (end (maxima-form-end-position)))
    (save-excursion
      (when (re-search-backward "^[ \t]*$" beg t)
        (maxima-forward-over-comment-whitespace)
        (setq beg (point))))
    (maxima-minibuffer-on-region beg end arg)))

(defun maxima-minibuffer-on-determined-region (&optional arg)
  "Send a determined region to Maxima; display the output in the minibuffer.
The region is the region between `maxima-minor-prefix' and
`maxima-minor-postfix' With ARG, insert \" ==> \" into
the current buffer, followed by the output.  In this case,
anything in the determined region after any occurrence of \" ==>
\" will be deleted."
  (interactive "P")
  (let ((beg)
        (end)
        (pt (point)))
    (save-excursion
      (if (re-search-backward maxima-minor-prefix nil t)
          (setq beg (match-end 0))
        (error "No beginning to determined region"))
      (goto-char pt)
      (if (re-search-forward maxima-minor-prefix nil t)
          (setq end (match-beginning 0))))
    (maxima-minibuffer-on-region beg end arg)))

(defun maxima-insert-last-output ()
  "Insert last output to inferior-maxima to the current buffer."
  (interactive)
  (maxima-single-string-wait
   "block(emacsdisplay:display2d,display2d:false,linenum:linenum-1,%);")
  (let ((output (maxima-last-output-noprompt maxima-inferior-process)))
    (maxima-single-string "block(display2d:emacsdisplay,linenum:linenum-1,%);" maxima-inferior-process)
    (insert (string-trim-right output))))

(defun maxima-insert-last-output-tex ()
  "Insert the last output in tex format."
  (interactive)
  (maxima-single-string-wait "tex(%);")
  (let ((output (substring (maxima-last-output-tex-noprompt maxima-inferior-process) 2 -3)))
    (maxima-single-string "block(linenum:linenum-2,%th(2));" maxima-inferior-process)
    (insert output)))

;;; Latex and org-mode interaction

;; FIXME generalise this function for use in minor-mode
(if (fboundp 'org-latex-preview)
    (defun maxima-latex-insert-form ()
      "Send and insert the current form in text formated output."
      (interactive)
      (let* ((keep-looking t)
	     (beg-point nil)
             (end-point nil)
	     (form-text nil)
	     (command-output nil)
	     (current-pos (point)))
	(maxima-goto-beginning-of-form)
	(setq beg-point (point))
	(while (and keep-looking
                    (maxima-re-search-forward "[;$]" nil))
	  (forward-char -2)
	  (unless (looking-at "\\\\\\$")
            (setq keep-looking nil))
	  (forward-char 2))
	(if (not keep-looking)
	    (setq end-point (point))
	  (error "End of the form not found"))
	(setq form-text (string-remove-suffix ";" (buffer-substring beg-point end-point)))
	(maxima-send-block (format "%s;" form-text) maxima-auxiliary-inferior-process)
	(maxima-send-block "tex(%,false);" maxima-auxiliary-inferior-process)
	(setq command-output (maxima-last-output-tex-noprompt maxima-auxiliary-inferior-process))
	(backward-char 3)
	(end-of-line)
	(insert
	 "\n"
	 command-output)
	(org-latex-preview)
	(goto-char current-pos))))


;;; The Maxima minor mode

(defvar maxima-minor-mode-map nil
  "The keymap for function `maxima-minor-mode'.")

;; FIXME Use kbd for clearance
(if maxima-minor-mode-map
    nil
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c=m" 'maxima-minibuffer)
    (define-key map "\C-c=e" 'maxima-minibuffer-on-determined-region)
    (define-key map "\C-c=l" 'maxima-minibuffer-on-line)
    (define-key map "\C-c=r" 'maxima-minibuffer-on-region)
    (define-key map "\C-c=f" 'maxima-minibuffer-on-form)
    (define-key map "\C-c=o" 'maxima-insert-last-output)
    (define-key map "\C-c=t" 'maxima-insert-last-output-tex)
    (define-key map "\C-c=d" maxima-help-map)
    (setq maxima-minor-mode-map map)))

(define-minor-mode maxima-minor-mode
  "Toggle Maxima minor mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When Maxima minor mode is enabled, the following keystrokes
are in effect:
\\[maxima-minibuffer-on-determined-region]
   Send the region between the customizable regexps
  `maxima-minor-prefix' and  `maxima-minor-postfix' to Maxima
  and display the result in the minibuffer.
\\[maxima-minibuffer-on-line]
  Send the current line to Maxima and display the result in the minibuffer.
\\[maxima-minibuffer-on-region]
  Send the current region to Maxima and display the result in the minibuffer.
\\[maxima-minibuffer-on-form]
  Send the current form to Maxima and display the result in the minibuffer.
  (The form is the region between the preceding ; or $ and the subsequent
  ; or $)
With a prefix, the above commands will insert the output in the current
buffer, preceded by \" ==> \" (customizable with `maxima-minor-output').
\\[maxima-minibuffer]
  Prompt for an expression in the minibuffer, return result in minibuffer.
\\[maxima-insert-last-output]
  Insert the last Maxima result into the current buffer.
\\[maxima-insert-last-output-tex]
  Insert the last Maxima result in TeX form into the current buffer."
  ;; The initial value.;  :initial-value
  nil
  ;; The indicator for the mode line.;  :lighter
  " maxima"
  nil)

;;;###autoload
(define-globalized-minor-mode global-maxima-minor-mode maxima-minor-mode maxima-minor-mode)

;;; For highlighting the region being sent

(defun maxima-minor-mode-add-highlight ()
  "Add highlight to function `maxima-minor-mode'."
  (maxima-minor-mode-remove-highlight)
  (when (and maxima-minor-mode-region-begin
             maxima-minor-mode-region-end)
    (setq maxima-minor-mode-highlight
          (make-overlay
           maxima-minor-mode-region-begin
           maxima-minor-mode-region-end))
    (overlay-put maxima-minor-mode-highlight 'face 'highlight)
    (setq maxima-minor-mode-region-begin nil)
    (setq maxima-minor-mode-region-end nil)))

(defun maxima-minor-mode-remove-highlight ()
  "Remove highlight to function `maxima-minor-mode'."
  (when maxima-minor-mode-highlight
    (delete-overlay maxima-minor-mode-highlight)
    (setq maxima-minor-mode-highlight nil)))

(defun maxima-minor-mode-add-remove-highlight ()
  "Check to add or remove highlight to function `maxima-minor-mode'."
  (if (or
       (eq this-command 'maxima-minibuffer-on-region)
       (eq this-command 'maxima-minibuffer-on-determined-region)
       (eq this-command 'maxima-minibuffer-on-form)
       (eq this-command 'maxima-minibuffer-on-line))
      (maxima-minor-mode-add-highlight)
    (maxima-minor-mode-remove-highlight)))

(defun maxima-minor-mode-highlighting ()
  "Add a function to `post-command-hook'."
  (add-hook 'post-command-hook
            'maxima-minor-mode-add-remove-highlight nil t))

(add-hook 'maxima-minor-mode-hook
          'maxima-minor-mode-highlighting)

(provide 'maxima)
;;; maxima.el ends here

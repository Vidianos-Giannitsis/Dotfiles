;;; tex-fold.el --- Fold TeX macros.  -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2024  Free Software Foundation, Inc.

;; Author: Ralf Angeli <angeli@caeruleus.net>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2004-07-04
;; Keywords: tex, text

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.

;; AUCTeX is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides support for hiding and unhiding TeX, LaTeX,
;; ConTeXt, Texinfo and similar macros and environments inside of
;; AUCTeX.
;;
;; Caveats:
;;
;; The display string of content which should display part of itself
;; is made by copying the text from the buffer together with its text
;; properties.  If fontification has not happened when this is done
;; (e.g. because of lazy or just-in-time font locking) the intended
;; fontification will not show up.  Maybe this could be improved by
;; using some sort of "lazy folding" or refreshing the window upon
;; scrolling.  As a workaround fontification of the whole buffer
;; currently is forced before folding it.

;;; Code:

(require 'tex)
(autoload 'LaTeX-forward-paragraph "latex")
(autoload 'LaTeX-backward-paragraph "latex")
(autoload 'LaTeX-find-matching-begin "latex")
(autoload 'LaTeX-find-matching-end "latex")
(autoload 'LaTeX-mark-section "latex")
(autoload 'ConTeXt-find-matching-start "context")
(autoload 'ConTeXt-find-matching-stop "context")
(autoload 'Texinfo-find-env-start "tex-info")
(autoload 'Texinfo-find-env-end "tex-info")

;; Silence the compiler
(declare-function LaTeX-verbatim-macro-boundaries "latex")
(declare-function LaTeX-verbatim-macros-with-braces "latex")
(declare-function LaTeX-verbatim-macros-with-delims "latex")
(declare-function bibtex-parse-entry "bibtex")
(declare-function bibtex-text-in-field "bibtex")

(defgroup TeX-fold nil
  "Fold TeX macros."
  :group 'AUCTeX)

(defcustom TeX-fold-type-list '(env macro math)
  "List of item types to consider when folding.
Valid items are the symbols `env' for environments, `macro' for
macros, `math' for math macros and `comment' for comments."
  :type '(set (const :tag "Environments" env)
              (const :tag "Macros" macro)
              (const :tag "Math Macros" math)
              (const :tag "Comments" comment)))

(defcustom TeX-fold-macro-spec-list
  '(("[f]" ("footnote" "marginpar"))
    (TeX-fold-cite-display ("cite"))
    ("[l]" ("label"))
    ("[r]" ("ref" "pageref" "eqref" "footref"))
    ("[i]" ("index" "glossary"))
    ("[1]:||*" ("item"))
    ("..." ("dots"))
    ("(C)" ("copyright"))
    ("(R)" ("textregistered"))
    ("TM"  ("texttrademark"))
    (TeX-fold-alert-display ("alert"))
    (TeX-fold-textcolor-display ("textcolor"))
    (TeX-fold-begin-display ("begin"))
    (TeX-fold-end-display ("end"))
    (1 ("part" "chapter" "section" "subsection" "subsubsection"
        "paragraph" "subparagraph"
        "part*" "chapter*" "section*" "subsection*" "subsubsection*"
        "paragraph*" "subparagraph*"
        "emph" "textit" "textsl" "textmd" "textrm" "textsf" "texttt"
        "textbf" "textsc" "textup")))
  "List of replacement specifiers and macros to fold.

The first element of each item can be a string, an integer or a
function symbol.  The second element is a list of macros to fold
without the leading backslash.

If the first element is a string, it will be used as a display
replacement for the whole macro.  Numbers in braces, brackets,
parens or angle brackets will be replaced by the respective macro
argument.  For example \"{1}\" will be replaced by the first
mandatory argument of the macro.  One can also define
alternatives within the specifier which are used if an argument
is not found.  Alternatives are separated by \"||\".  They are
most useful with optional arguments.  As an example, the default
specifier for \\item is \"[1]:||*\" which means that if there is
an optional argument, its value is shown followed by a colon.  If
there is no optional argument, only an asterisk is used as the
display string.

If the first element is an integer, the macro will be replaced by
the respective macro argument.

If the first element is a function symbol, the function will be
called with all mandatory arguments of the macro and the result
of the function call will be used as a replacement for the macro.
Such functions typically return a string, but may also return the
symbol `abort' to indicate that the macro should not be folded.

Setting this variable does not take effect immediately.  Use
Customize or reset the mode."
  :type '(repeat (group (choice (string :tag "Display String")
                                (integer :tag "Number of argument" :value 1)
                                (function :tag "Function to execute"))
                        (repeat :tag "Macros" (string))))
  :package-version '(auctex . "14.0.8"))

(defvar-local TeX-fold-macro-spec-list-internal nil
  "Internal list of display strings and macros to fold.
Is updated when the TeX Fold mode is being activated and then
contains all constructs to fold for the given buffer or mode
respectively, that is, contents of both `TeX-fold-macro-spec-list'
and <mode-prefix>-fold-macro-spec-list.")

(defcustom TeX-fold-env-spec-list
  '(("[comment]" ("comment")))
  "List of display strings and environments to fold."
  :type '(repeat (group (choice (string :tag "Display String")
                                (integer :tag "Number of argument" :value 1)
                                (function :tag "Function to execute"))
                        (repeat :tag "Environments" (string)))))

(defvar-local TeX-fold-env-spec-list-internal nil
  "Internal list of display strings and environments to fold.
Is updated when the TeX Fold mode is being activated and then
contains all constructs to fold for the given buffer or mode
respectively, that is, contents of both `TeX-fold-env-spec-list'
and <mode-prefix>-fold-env-spec-list.")

(defcustom TeX-fold-math-spec-list nil
  "List of display strings and math macros to fold."
  :type '(repeat (group (choice (string :tag "Display String")
                                (integer :tag "Number of argument" :value 1)
                                (function :tag "Function to execute"))
                        (repeat :tag "Math Macros" (string)))))

(defvar-local TeX-fold-math-spec-list-internal nil
  "Internal list of display strings and math macros to fold.
Is updated when the TeX Fold mode is being activated and then
contains all constructs to fold for the given buffer or mode
respectively, that is, contents of both `TeX-fold-math-spec-list'
and <mode-prefix>-fold-math-spec-list.")

(defcustom TeX-fold-unspec-macro-display-string "[m]"
  "Display string for unspecified macros.
This string will be displayed if a single macro is being hidden
which is not specified in `TeX-fold-macro-spec-list'."
  :type '(string))

(defcustom TeX-fold-unspec-env-display-string "[env]"
  "Display string for unspecified environments.
This string will be displayed if a single environment is being
hidden which is not specified in `TeX-fold-env-spec-list'."
  :type '(string))

(defcustom TeX-fold-unspec-use-name t
  "If non-nil use the name of an unspecified item as display string.
Set it to nil if you want to use the values of the variables
`TeX-fold-unspec-macro-display-string' or
`TeX-fold-unspec-env-display-string' respectively as a display
string for any unspecified macro or environment."
  :type 'boolean)

(defcustom TeX-fold-preserve-comments nil
  "If non-nil do not fold in comments."
  :type 'boolean)

(defcustom TeX-fold-unfold-around-mark t
  "Unfold text around the mark, if active."
  :type 'boolean)

(defcustom TeX-fold-help-echo-max-length 70
  "Maximum length of help echo message for folded overlays.
Set it to zero in order to disable help echos."
  :type 'integer)

(defcustom TeX-fold-force-fontify t
  "Force the buffer to be fully fontified by folding it."
  :type 'boolean)

(defcustom TeX-fold-auto nil
  "If non-nil, fold macros automatically after `TeX-insert-macro'."
  :type 'boolean)

(defface TeX-fold-folded-face
  '((((class color) (background light))
     (:foreground "SlateBlue"))
    (((class color) (background dark))
     (:foreground "SlateBlue1"))
    (((class grayscale) (background light))
     (:foreground "DimGray"))
    (((class grayscale) (background dark))
     (:foreground "LightGray"))
    (t (:slant italic)))
  "Face for the display string of folded content.")

(defvar TeX-fold-folded-face 'TeX-fold-folded-face
  "Face for the display string of folded content.")

(defface TeX-fold-unfolded-face
  '((((class color) (background light))
     (:background "#f2f0fd"))
    (((class color) (background dark))
     (:background "#38405d"))
    (((class grayscale) (background light))
     (:background "LightGray"))
    (((class grayscale) (background dark))
     (:background "DimGray"))
    (t (:inverse-video t)))
  "Face for folded content when it is temporarily opened.")

(defvar TeX-fold-unfolded-face 'TeX-fold-unfolded-face
  "Face for folded content when it is temporarily opened.")

(defvar TeX-fold-ellipsis "..."
  "String used as display string for overlays instead of a zero-length string.")

(defvar-local TeX-fold-open-spots nil)

(defcustom TeX-fold-command-prefix "\C-c\C-o"
  "Prefix key to use for commands in TeX Fold mode.
The value of this variable is checked as part of loading TeX Fold mode.
After that, changing the prefix key requires manipulating keymaps."
  :type 'string)

(defvar TeX-fold-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-o" #'TeX-fold-dwim)
    (define-key map "\C-b" #'TeX-fold-buffer)
    (define-key map "\C-r" #'TeX-fold-region)
    (define-key map "\C-p" #'TeX-fold-paragraph)
    (define-key map "\C-s" #'TeX-fold-section)
    (define-key map "\C-m" #'TeX-fold-macro)
    (define-key map "\C-e" #'TeX-fold-env)
    (define-key map "\C-c" #'TeX-fold-comment)
    (define-key map "b"    #'TeX-fold-clearout-buffer)
    (define-key map "r"    #'TeX-fold-clearout-region)
    (define-key map "p"    #'TeX-fold-clearout-paragraph)
    (define-key map "s"    #'TeX-fold-clearout-section)
    (define-key map "i"    #'TeX-fold-clearout-item)
    map))

(defcustom TeX-fold-auto-reveal-commands
  '((key-binding [left])
    (key-binding [right])
    backward-char
    forward-char
    mouse-set-point
    pop-to-mark-command
    undo)
  "List of commands that may cause a fold to be revealed.
This list is consulted by the default value of `TeX-fold-auto-reveal'."
  :type '(repeat (choice (function :tag "Function")
                         (sexp :tag "Key binding"))))

(defcustom TeX-fold-auto-reveal
  '(eval . ((apply #'TeX-fold-arrived-via
                   (mapcar (lambda (cmd)
                             (if (and (listp cmd) (eq (car cmd) 'key-binding))
                                 (eval cmd t)
                               cmd))
                           TeX-fold-auto-reveal-commands))
            t))
  "Predicate to open a fold when entered.
Possibilities are:
t autoopens,
nil doesn't,
a symbol will have its value consulted if it exists,
defaulting to nil if it doesn't.
A CONS-cell means to call a function for determining the value.
The CAR of the cell is the function to call which receives
the CDR of the CONS-cell in the rest of the arguments, while
point and current buffer point to the position in question.
All of the options show reasonable defaults."
  :group 'TeX-fold
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)
                 (symbol :tag "Indirect variable" :value reveal-mode)
                 (cons :tag "Function call"
                       :value (eval (TeX-fold-arrived-via
                                     (key-binding [left])
                                     (key-binding [right])
                                     #'backward-char #'forward-char
                                     #'mouse-set-point))
                       function (list :tag "Argument list"
                                      (repeat :inline t sexp)))))


;;; Folding

(defun TeX-fold-dwim ()
  "Hide or show items according to the current context.
If there is folded content, unfold it.  If there is a marked
region, fold all configured content in this region.  If there is
no folded content but a macro or environment, fold it."
  (interactive)
  (cond ((TeX-fold-clearout-item))
        ((TeX-active-mark) (TeX-fold-region (mark) (point)))
        ((TeX-fold-item 'macro))
        ((TeX-fold-item 'math))
        ((TeX-fold-item 'env))
        ((TeX-fold-comment))))

(defun TeX-fold-buffer ()
  "Hide all configured macros and environments in the current buffer.
The relevant macros are specified in the variable `TeX-fold-macro-spec-list'
and `TeX-fold-math-spec-list', and environments in `TeX-fold-env-spec-list'."
  (interactive)
  (TeX-fold-clearout-region (point-min) (point-max))
  (when (and TeX-fold-force-fontify
             (boundp 'jit-lock-mode)
             jit-lock-mode
             (fboundp 'jit-lock-fontify-now))
    ;; We force fontification here only because it should rarely be
    ;; needed for the other folding commands.
    (jit-lock-fontify-now))
  (TeX-fold-region (point-min) (point-max)))

(defun TeX-fold-paragraph ()
  "Hide all configured macros and environments in the current paragraph.
The relevant macros are specified in the variable `TeX-fold-macro-spec-list'
and `TeX-fold-math-spec-list', and environments in `TeX-fold-env-spec-list'."
  (interactive)
  (save-excursion
    (let ((end (progn (LaTeX-forward-paragraph) (point)))
          (start (progn (LaTeX-backward-paragraph) (point))))
      (TeX-fold-clearout-region start end)
      (TeX-fold-region start end))))

(defun TeX-fold-section ()
  "Hide all configured macros and environments in the current section.
The relevant macros are specified in the variable `TeX-fold-macro-spec-list'
and `TeX-fold-math-spec-list', and environments in `TeX-fold-env-spec-list'."
  (interactive)
  (save-mark-and-excursion
    (LaTeX-mark-section)
    (let ((start (point))
          (end (mark)))
      (TeX-fold-clearout-region start end)
      (TeX-fold-region start end))))

(defcustom TeX-fold-region-functions '(TeX-fold-verbs TeX-fold-quotes)
  "List of additional functions to call when folding a region.
Each function is called with two arguments, the start and end positions
of the region to fold."
  :type '(repeat function)
  :package-version '(auctex . "14.0.8"))

(defun TeX-fold-region (start end)
  "Fold all items in region from START to END."
  (interactive "r")
  (when (and (memq 'env TeX-fold-type-list)
             (not (eq major-mode 'plain-TeX-mode)))
    (TeX-fold-region-macro-or-env start end 'env))
  (when (memq 'macro TeX-fold-type-list)
    (TeX-fold-region-macro-or-env start end 'macro))
  (when (memq 'math TeX-fold-type-list)
    (TeX-fold-region-macro-or-env start end 'math))
  (when (memq 'comment TeX-fold-type-list)
    (TeX-fold-region-comment start end))
  (run-hook-with-args 'TeX-fold-region-functions start end))

(defun TeX-fold-region-macro-or-env (start end type)
  "Fold all items of type TYPE in region from START to END.
TYPE can be one of the symbols `env' for environments, `macro'
for macros and `math' for math macros."
  (save-excursion
    (let (fold-list item-list regexp)
      (dolist (item (cond ((eq type 'env) TeX-fold-env-spec-list-internal)
                          ((eq type 'math) TeX-fold-math-spec-list-internal)
                          (t TeX-fold-macro-spec-list-internal)))
        (dolist (i (cadr item))
          (cl-pushnew (list i (car item)) fold-list :test #'equal)
          (cl-pushnew i item-list :test #'equal)))
      (when item-list
        (setq regexp (cond ((and (eq type 'env)
                                 (eq major-mode 'ConTeXt-mode))
                            (concat (regexp-quote TeX-esc)
                                    "start" (regexp-opt item-list t)))
                           ((and (eq type 'env)
                                 (eq major-mode 'Texinfo-mode))
                            (concat (regexp-quote TeX-esc)
                                    (regexp-opt item-list t)))
                           ((eq type 'env)
                            (concat (regexp-quote TeX-esc)
                                    "begin[ \t]*{"
                                    (regexp-opt item-list t) "}"))
                           (t
                            (concat (regexp-quote TeX-esc)
                                    (regexp-opt item-list t)))))
        (save-restriction
          (narrow-to-region start end)
          ;; Start from the bottom so that it is easier to prioritize
          ;; nested macros.
          (goto-char (point-max))
          (let ((case-fold-search nil)
                item-name)
            (while (re-search-backward regexp nil t)
              (setq item-name (match-string 1))
              (unless (or (and TeX-fold-preserve-comments
                               (TeX-in-commented-line))
                          ;; Make sure no partially matched macros are
                          ;; folded.  For macros consisting of letters
                          ;; this means there should be none of the
                          ;; characters [A-Za-z@*] after the matched
                          ;; string.  Single-char non-letter macros like
                          ;; \, don't have this requirement.
                          (and (memq type '(macro math))
                               (save-match-data
                                 (string-match "[A-Za-z]" item-name))
                               (save-match-data
                                 (string-match "[A-Za-z@*]"
                                               (string (char-after
                                                        (match-end 0)))))))
                (let* ((item-start (match-beginning 0))
                       (display-string-spec (cadr (assoc item-name
                                                         fold-list)))
                       (item-end (TeX-fold-item-end item-start type))
                       (ov (TeX-fold-make-overlay item-start item-end type
                                                  display-string-spec)))
                  (TeX-fold-hide-item ov))))))))))

(defun TeX-fold-region-comment (start end)
  "Fold all comments in region from START to END."
  (save-excursion
    (goto-char start)
    (let (beg)
      (while (setq beg (TeX-search-forward-comment-start end))
        (goto-char beg)
        ;; Determine the start of the region to be folded just behind
        ;; the comment starter.
        (looking-at TeX-comment-start-regexp)
        (setq beg (match-end 0))
        ;; Search for the end of the comment.
        (while (TeX-comment-forward))
        (end-of-line 0)
        ;; Hide the whole region.
        (TeX-fold-hide-item (TeX-fold-make-overlay beg (point) 'comment
                                                   TeX-fold-ellipsis))))))

(defun TeX-fold-macro ()
  "Hide the macro on which point currently is located."
  (interactive)
  (unless (TeX-fold-item 'macro)
    (message "No macro found")))

(defun TeX-fold-math ()
  "Hide the math macro on which point currently is located."
  (interactive)
  (unless (TeX-fold-item 'math)
    (message "No macro found")))

(defun TeX-fold-env ()
  "Hide the environment on which point currently is located."
  (interactive)
  (unless (TeX-fold-item 'env)
    (message "No environment found")))

(defun TeX-fold-comment ()
  "Hide the comment on which point currently is located."
  (interactive)
  (unless (TeX-fold-comment-do)
    (message "No comment found")))

(defun TeX-fold-item (type)
  "Hide the item on which point currently is located.
TYPE specifies the type of item and can be one of the symbols
`env' for environments, `macro' for macros or `math' for math
macros.
Return non-nil if an item was found and folded, nil otherwise."
  (if (and (eq type 'env)
           (eq major-mode 'plain-TeX-mode))
      (message
       "Folding of environments is not supported in current mode")
    (let ((item-start (cond ((and (eq type 'env)
                                  (eq major-mode 'ConTeXt-mode))
                             (save-excursion
                               (ConTeXt-find-matching-start) (point)))
                            ((and (eq type 'env)
                                  (eq major-mode 'Texinfo-mode))
                             (save-excursion
                               (Texinfo-find-env-start) (point)))
                            ((eq type 'env)
                             (condition-case nil
                                 (save-excursion
                                   (LaTeX-find-matching-begin) (point))
                               (error nil)))
                            (t
                             (TeX-find-macro-start)))))
      (when item-start
        (let* ((item-name (save-excursion
                            (goto-char item-start)
                            (looking-at
                             (cond ((and (eq type 'env)
                                         (eq major-mode 'ConTeXt-mode))
                                    (concat (regexp-quote TeX-esc)
                                            "start\\([A-Za-z]+\\)"))
                                   ((and (eq type 'env)
                                         (eq major-mode 'Texinfo-mode))
                                    (concat (regexp-quote TeX-esc)
                                            "\\([A-Za-z]+\\)"))
                                   ((eq type 'env)
                                    (concat (regexp-quote TeX-esc)
                                            "begin[ \t]*{"
                                            "\\([A-Za-z*]+\\)}"))
                                   (t
                                    (concat (regexp-quote TeX-esc)
                                            "\\([A-Za-z@*]+\\)"))))
                            (match-string-no-properties 1)))
               (fold-list (cond ((eq type 'env) TeX-fold-env-spec-list-internal)
                                ((eq type 'math)
                                 TeX-fold-math-spec-list-internal)
                                (t TeX-fold-macro-spec-list-internal)))
               fold-item
               (display-string-spec
                (or (catch 'found
                      (while fold-list
                        (setq fold-item (car fold-list))
                        (setq fold-list (cdr fold-list))
                        (when (member item-name (cadr fold-item))
                          (throw 'found (car fold-item)))))
                    ;; Item is not specified.
                    (if TeX-fold-unspec-use-name
                        (concat "[" item-name "]")
                      (if (eq type 'env)
                          TeX-fold-unspec-env-display-string
                        TeX-fold-unspec-macro-display-string))))
               (item-end (TeX-fold-item-end item-start type))
               (ov (TeX-fold-make-overlay item-start item-end type
                                          display-string-spec)))
          (TeX-fold-hide-item ov))))))

(defun TeX-fold-comment-do ()
  "Hide the comment on which point currently is located.
This is the function doing the work for `TeX-fold-comment'.  It
is an internal function communicating with return values rather
than with messages for the user.
Return non-nil if a comment was found and folded, nil otherwise."
  (if (and (not (TeX-in-comment)) (not (TeX-in-line-comment)))
      nil
    (let (beg)
      (save-excursion
        (while (progn
                 (beginning-of-line 0)
                 (and (TeX-in-line-comment)
                      (not (bobp)))))
        (goto-char (TeX-search-forward-comment-start (line-end-position 2)))
        (looking-at TeX-comment-start-regexp)
        (setq beg (match-end 0))
        (while (TeX-comment-forward))
        (end-of-line 0)
        (when (> (point) beg)
          (TeX-fold-hide-item (TeX-fold-make-overlay beg (point) 'comment
                                                     TeX-fold-ellipsis)))))))

;;; Display functions

;; This section provides functions for use in `TeX-fold-macro-spec-list'.

;;;; textcolor

(defun TeX-fold-textcolor-display (color text &rest _args)
  "Fold display for a \\textcolor{COLOR}{TEXT} macro."
  (with-temp-buffer
    (insert text)
    (put-text-property (point-min) (point-max)
                       'face `(:foreground ,color)
                       (current-buffer))
    (buffer-string)))

;;;; alert

(defcustom TeX-fold-alert-color "red"
  "Color for alert text."
  :type 'color
  :package-version '(auctex . "14.0.8"))

(defun TeX-fold-alert-display (text &rest _args)
  "Fold display for a \\alert{TEXT} macro."
  (with-temp-buffer
    (insert text)
    (put-text-property (point-min) (point-max)
                       'face `(:foreground ,TeX-fold-alert-color)
                       (current-buffer))
    (buffer-string)))

;;;; begin/end

(defcustom TeX-fold-begin-end-spec-list
  '((("↴" . "↲")
     ("itemize" "enumerate" "description" "frame"))
    ((TeX-fold-format-titled-block . "◼")
     ("block"))
    ((TeX-fold-format-titled-alertblock . "◼")
     ("alertblock"))
    ((TeX-fold-format-theorem-environment . "□")
     ("proof"))
    ((TeX-fold-format-theorem-environment . "◼")
     ("abstract"
      "acknowledgment"
      "algorithm"
      "assumptions"
      "claim"
      "commentary"
      "fact"
      "note"
      "questions"
      ("answer" "ans")
      ("conclusion" "conc")
      ("condition" "cond")
      ("conjecture" "conj")
      ("corollary" "cor")
      ("criterion" "crit")
      ("definition" "def" "defn")
      ("example" "ex")
      ("exercise" "exer")
      ("lemma" "lem")
      ("notation" "not")
      ("problem" "prob")
      ("proposition" "prop")
      ("question" "ques")
      ("remark" "rem" "rmk")
      ("summary" "sum")
      ("terminology" "term")
      ("theorem" "thm"))))
  "Replacement specifier list for \\begin{env} and \\end{env} macros.

This option is relevant only if the replacement specifiers in
`TeX-fold-macro-spec-list' for \"begin\" and \"end\" macros are the
defaults, namely `TeX-fold-begin-display' and `TeX-fold-end-display'.

Each item is a list consisting of two elements:

The first element is a cons cell, with car and cdr the display
specifications for \\begin{...} and \\end{...}  macros, respectively.
Each specification is either

  - a string, used as the fold display string, or

  - a function, called with the (unabbreviated) environment name and a
    list consisting of the remaining mandatory macro arguments, that
    returns a string.

The second element is a list of environment types, which are either

- the environment name, e.g., \"remark\", or

- a list with first element an environment name and remaining elements
  any abbreviated environment names, e.g., (\"remark\" \"rem\" \"rmk\")."
  :type '(repeat
          (group
           (cons (choice (string :tag "Display String for \\begin{...}")
                         (function :tag "Function to execute for \\begin{...}"))
                 (choice (string :tag "Display String for \\end{...}")
                         (function :tag "Function to execute for \\end{...}")))
           (repeat :tag "Environment Types"
                   (choice (string :tag "Environment")
                           (cons :tag "Environment and Abbreviations"
                                 (string :tag "Environment")
                                 (repeat :tag "Abbreviations"
                                         (string :tag "Abbreviation")))))))
  :package-version '(auctex . "14.0.8"))


(defun TeX-fold-begin-display (env &rest args)
  "Fold display for a \\begin{ENV}.
Intended for use in `TeX-fold-begin-end-spec-list'.  ARGS is a list
consisting of the remaining mandatory macro arguments."
  (TeX-fold--helper-display env args #'car))

(defun TeX-fold-end-display (env &rest args)
  "Fold display for a \\end{ENV} macro.
Intended for use in `TeX-fold-begin-end-spec-list'.  ARGS is a list
consisting of the remaining mandatory macro arguments."
  (TeX-fold--helper-display env args #'cdr))

(defun TeX-fold--helper-display (env args spec-retriever)
  "Generate fold display string for \\begin{ENV} or \\end{ENV} macro.
ARGS are the remaining mandatory macro arguments.  Returns the string or
function determined by `TeX-fold-begin-end-spec-list' if ENV is found
there, otherwise `abort'.  SPEC-RETRIEVER, which should be either `car'
or `cdr', retrieves the appropriate part of the display specification."
  (catch 'result
    (dolist (item TeX-fold-begin-end-spec-list)
      (let* ((spec (funcall spec-retriever (car item)))
             (types (cadr item)))
        (dolist (type types)
          (when-let* ((name (cond ((stringp type)
                                   (when (string= env type)
                                     env))
                                  ((consp type)
                                   (when (member env type)
                                     (car type))))))
            (throw 'result
                   (if (functionp spec)
                       (funcall spec name args)
                     spec))))))
    'abort))

;;;;; block environments

(defun TeX-fold-format-titled-block (_env args)
  "Format fold display for beamer block environments.
Intended for use in `TeX-fold-begin-end-spec-list'.  ENV is ignored.
ARGS is a list whose car will be the block title.

Example: \"\\begin{block}{Theorem 1}\" folds to \"Theorem 1\"."
  (car args))

(defun TeX-fold-format-titled-alertblock (_env args)
  "Format fold display for beamer alertblock environments.
Intended for use in `TeX-fold-begin-end-spec-list'.  The arguments
ENV/ARGS and the behavior are as in `TeX-fold-format-titled-block', but
the folded text is colored using `TeX-fold-alert-color'."
  (let ((caption (car args)))
    (add-face-text-property 0 (length caption)
                            `(:foreground ,TeX-fold-alert-color) nil caption)
    (format "%s" caption)))

;;;;; theorem-like environments

(defun TeX-fold-format-theorem-environment (env _args)
  "Format fold display for theorem-like LaTeX environments.
Intended for use in `TeX-fold-begin-end-spec-list'.  ENV is the
environment name, ARGS are ignored.  Returns a string of the form
\"Environment \" or \"Environment (Description) \""
  (let* ((env (with-temp-buffer
                (insert env)
                (goto-char (point-min))
                (capitalize-word 1)
                (buffer-string)))
         (description
          (car (TeX-fold-macro-nth-arg 1 (point)
                                       (TeX-fold-item-end (point) 'macro)
                                       '(?\[ . ?\])))))
    (concat
     (format "%s " env)
     (when description
       (format "(%s) " description)))))

;;;; citations

(defun TeX-fold--last-name (name)
  "Return string consisting of last name of NAME.
NAME should be of the form \"Last, First\" or \"First Last\", possibly
with some additional non-alphabetical characters such as braces."
  (if-let* ((comma (string-match "," name)))
      (setq name (substring name 0 comma))
    (when-let* ((space (string-match " " name)))
      (setq name (substring name space))))
  (when-let* ((index (string-match "[[:alpha:]]" name)))
    (setq name (substring name index)))
  (when-let* ((index (string-match "[^[:alpha:]]" name)))
    (setq name (substring name 0 index)))
  name)

(defun TeX-fold--bib-abbrev-entry-at-point ()
  "Abbreviate the BibTeX entry at point.
Return string of the form \"XYZ99\", formed using authors' last names and
publication year, or nil if author/year not found."
  (require 'bibtex)
  (when-let* ((case-fold-search t)
              (entry (bibtex-parse-entry))
              (author (bibtex-text-in-field "author" entry))
              (year (bibtex-text-in-field "year" entry))
              (last-names
               (mapcar #'TeX-fold--last-name (string-split author " and ")))
              (last-names (seq-filter (lambda (name) (> (length name) 0))
                                      last-names))
              (initials
               (if (and (eq (length last-names) 1)
                        (> (length (car last-names)) 1))
                   (substring (car last-names) 0 2)
                 (mapconcat (lambda (name)
                              (substring name 0 1))
                            last-names)))
              (year-XX (when year (substring year -2))))
    (concat initials year-XX)))

(defun TeX-fold--bib-entry (key files)
  "Retrieve BibTeX entry for KEY from FILES.
Return first BibTeX entry found as a string, or nil if none found."
  (when (fboundp 'reftex-pop-to-bibtex-entry)
    (condition-case nil
        (reftex-pop-to-bibtex-entry key files nil nil nil t)
      (error nil))))

(defcustom TeX-fold-bib-files nil
  "List of BibTeX files from which to extract citation keys.
This is used as a fallback option for citation folding when RefTeX can't
find the citation keys in the provided bib files, and may be useful when
using \\thebibliography or when working in non-file buffers."
  :type '(repeat file)
  :package-version '(auctex . "14.0.8"))

(defun TeX-fold--bib-abbrev (key)
  "Get abbreviation for BibTeX entry associated with KEY.
Search using RefTeX (if available) and `TeX-fold-bib-file'.  Return
string of the form \"XYZ99\" or nil if the key is not found or does not
contain the required information."
  (when-let* ((entry (or (and (bound-and-true-p reftex-mode)
                              (fboundp 'reftex-get-bibfile-list)
                              (when-let* ((files
                                           (condition-case nil
                                               (reftex-get-bibfile-list)
                                             (error nil))))
                                (TeX-fold--bib-entry key files)))
                         (TeX-fold--bib-entry
                          key TeX-fold-bib-files))))
    (with-temp-buffer
      (insert entry)
      (goto-char (point-min))
      (TeX-fold--bib-abbrev-entry-at-point))))

(defun TeX-fold-cite-display (keys &rest _args)
  "Fold display for a \\cite{KEYS} macro.
KEYS are the citation key(s), as a comma-delimited list.  Return string
of the form \"[XYZ99]\" or \"[XYZ99, Optional Citation Text]\", formed
using authors' last names and the the publication year."
  (let* ((citation (car (TeX-fold-macro-nth-arg
                         1 (point)
                         (TeX-fold-item-end (point) 'macro)
                         '(?\[ . ?\]))))
         (key-list (split-string keys "[ \f\t\n\r\v,]+"))
         (references (delq nil (mapcar #'TeX-fold--bib-abbrev key-list)))
         (joined-references (string-join references ", ")))
    (concat "["
            (if (string-empty-p joined-references)
                "c" joined-references)
            (when citation
              (format ", %s" citation))
            "]")))

;;; Utilities

(defun TeX-fold-make-overlay (ov-start ov-end type display-string-spec
                                       &optional display-string)
  "Make a TeX-fold overlay extending from OV-START to OV-END.
TYPE is a symbol which is used to describe the content to hide and may
be `macro' for macros, `math' for math macro, `env' for environments, or
`misc' for miscellaneous constructs like quotes and dashes.
DISPLAY-STRING-SPEC is the original specification of the display
string in the variables `TeX-fold-macro-spec-list' and alikes.
See its doc string for detail.
If DISPLAY-STRING is provided, it will be used directly as the overlay's
display property."
  ;; Calculate priority before the overlay is instantiated.  We don't
  ;; want `TeX-overlay-prioritize' to pick up a non-prioritized one.
  (let ((priority (TeX-overlay-prioritize ov-start ov-end))
        (ov (make-overlay ov-start ov-end (current-buffer) t nil)))
    (overlay-put ov 'category 'TeX-fold)
    (overlay-put ov 'priority priority)
    (overlay-put ov 'evaporate t)
    (when type
      (overlay-put ov 'TeX-fold-type type))
    (overlay-put ov 'TeX-fold-display-string-spec display-string-spec)
    (when display-string
      (overlay-put ov 'display display-string))
    ov))

(defun TeX-fold-item-end (start type)
  "Return the end of an item of type TYPE starting at START.
TYPE can be either `env' for environments, `macro' for macros or
`math' for math macros."
  (save-excursion
    (cond ((and (eq type 'env)
                (eq major-mode 'ConTeXt-mode))
           (goto-char start)
           (ConTeXt-find-matching-stop)
           (point))
          ((and (eq type 'env)
                (eq major-mode 'Texinfo-mode))
           (goto-char (1+ start))
           (Texinfo-find-env-end)
           (point))
          ((eq type 'env)
           (goto-char (1+ start))
           (LaTeX-find-matching-end)
           (point))
          (t
           (goto-char start)
           (TeX-find-macro-end)))))

(defun TeX-fold-overfull-p (ov-start ov-end display-string)
  "Return t if an overfull line will result after adding an overlay.
The overlay extends from OV-START to OV-END and will display the
string DISPLAY-STRING."
  (and
   (save-excursion
     (goto-char ov-end)
     (search-backward "\n" ov-start t))
   (not (string-match "\n" display-string))
   (> (+ (- ov-start
            (save-excursion
              (goto-char ov-start)
              (line-beginning-position)))
         (length display-string)
         (- (save-excursion
              (goto-char ov-end)
              (line-end-position))
            ov-end))
      (current-fill-column))))

(defun TeX-fold-macro-nth-arg (n macro-start &optional macro-end delims)
  "Return a property list of the argument number N of a macro.
The start of the macro to examine is given by MACRO-START, its
end optionally by MACRO-END.  With DELIMS the type of delimiters
can be specified as a cons cell containing the opening char as
the car and the closing char as the cdr.  The chars have to have
opening and closing syntax as defined in
`TeX-search-syntax-table'.

The first item in the returned list is the string specified in
the argument, with text properties.  The second item is for
backward compatibility and always nil."
  (save-excursion
    (let* ((macro-end (or macro-end
                          (save-excursion (goto-char macro-start)
                                          (TeX-find-macro-end))))
           (open-char (if delims (car delims) ?{))
           (open-string (char-to-string open-char))
           (close-char (if delims (cdr delims) ?}))
           ;; (close-string (char-to-string close-char))
           content-start content-end)
      (goto-char macro-start)
      (if (condition-case nil
              (progn
                (while (> n 0)
                  (skip-chars-forward (concat "^" open-string) macro-end)
                  (when (= (point) macro-end)
                    (error nil))
                  (setq content-start (progn
                                        (skip-chars-forward
                                         (concat open-string " \t"))
                                        (point)))
                  (goto-char
                   (if (save-restriction
                         (widen)
                         ;; `widen' accomodates the following issue:
                         ;; with point on the `v' in `\end{verbatim}',
                         ;; LaTeX-verbatim-p returns nil normally, but t
                         ;; with region narrowed to avoid the
                         ;; corresponding `\begin{verbatim}'.
                         (TeX-verbatim-p))
                       (cond ((derived-mode-p 'LaTeX-mode)
                              (cdr (LaTeX-verbatim-macro-boundaries)))
                             ;; FIXME: When other modes implement a
                             ;; nontrivial `TeX-verbatim-p-function', we
                             ;; should return the appropriate endpoint
                             ;; here.
                             )
                     (if delims
                         (with-syntax-table
                             (TeX-search-syntax-table open-char close-char)
                           (scan-lists (point) 1 1))
                       (TeX-find-closing-brace))))
                  (setq content-end (save-excursion
                                      (backward-char)
                                      (skip-chars-backward " \t")
                                      (point)))
                  (setq n (1- n)))
                t)
            (error nil))
          (list (TeX-fold-buffer-substring content-start content-end))
        nil))))

(defun TeX-fold-buffer-substring (start end)
  "Return the contents of buffer from START to END as a string.
Like `buffer-substring' but copy overlay display strings as well."
  ;; Swap values of `start' and `end' if necessary.
  (when (> start end) (let ((tmp start)) (setq start end end tmp)))
  (let ((overlays (overlays-in start end))
        result)
    ;; Get rid of overlays not under our control or not completely
    ;; inside the specified region.
    (dolist (ov overlays)
      (when (or (not (eq (overlay-get ov 'category) 'TeX-fold))
                (< (overlay-start ov) start)
                (> (overlay-end ov) end))
        (setq overlays (remove ov overlays))))
    (if (null overlays)
        (buffer-substring start end)
      ;; Sort list according to ascending starts.
      (setq overlays (sort (copy-sequence overlays)
                           (lambda (a b)
                             (< (overlay-start a) (overlay-start b)))))
      ;; Get the string from the start of the region up to the first overlay.
      (setq result (buffer-substring start (overlay-start (car overlays))))
      (let (ov)
        (while overlays
          (setq ov (car overlays)
                overlays (cdr overlays))
          ;; Add the display string of the overlay.
          (setq result (concat result (overlay-get ov 'display)))
          ;; Remove overlays contained in the current one.
          (dolist (elt overlays)
            (when (< (overlay-start elt) (overlay-end ov))
              (setq overlays (remove elt overlays))))
          ;; Add the string from the end of the current overlay up to
          ;; the next overlay or the end of the specified region.
          (setq result (concat result (buffer-substring (overlay-end ov)
                                                        (if overlays
                                                            (overlay-start
                                                             (car overlays))
                                                          end))))))
      result)))

(defun TeX-fold-make-help-echo (start end)
  "Return a string to be used as the help echo of folded overlays.
The text between START and END will be used for this but cropped
to the length defined by `TeX-fold-help-echo-max-length'.  Line
breaks will be replaced by spaces."
  (let* ((spill (+ start TeX-fold-help-echo-max-length))
         (lines (split-string (buffer-substring start (min end spill)) "\n"))
         (result (pop lines)))
    (dolist (line lines)
      ;; Strip leading whitespace
      (when (string-match "^[ \t]+" line)
        (setq line (replace-match "" nil nil line)))
      ;; Strip trailing whitespace
      (when (string-match "[ \t]+$" line)
        (setq line (replace-match "" nil nil line)))
      (setq result (concat result " " line)))
    (when (> end spill) (setq result (concat result "...")))
    result))

(defun TeX-fold-update-at-point ()
  "Update all TeX-fold overlays at point displaying computed content."
  (let (overlays)
    ;; Get all overlays at point under our control.
    (dolist (ov (overlays-at (point)))
      (when (and (eq (overlay-get ov 'category) 'TeX-fold)
                 (numberp (overlay-get ov 'TeX-fold-display-string-spec)))
        (cl-pushnew ov overlays)))
    (when overlays
      ;; Sort list according to descending starts.
      (setq overlays (sort (copy-sequence overlays)
                           (lambda (a b)
                             (> (overlay-start a) (overlay-start b)))))
      (dolist (ov overlays)
        (TeX-fold-hide-item ov)))))


;;; Removal

(defun TeX-fold-clearout-buffer ()
  "Permanently show all macros in the buffer."
  (interactive)
  (TeX-fold-clearout-region (point-min) (point-max)))

(defun TeX-fold-clearout-paragraph ()
  "Permanently show all macros in the paragraph point is located in."
  (interactive)
  (save-excursion
    (let ((end (progn (LaTeX-forward-paragraph) (point)))
          (start (progn (LaTeX-backward-paragraph) (point))))
      (TeX-fold-clearout-region start end))))

(defun TeX-fold-clearout-section ()
  "Permanently show all macros in the section point is located in."
  (interactive)
  (save-mark-and-excursion
    (LaTeX-mark-section)
    (let ((start (point))
          (end (mark)))
      (TeX-fold-clearout-region start end))))

(defun TeX-fold-clearout-region (start end)
  "Permanently show all macros in region starting at START and ending at END."
  (interactive "r")
  (let ((overlays (overlays-in start end)))
    (TeX-fold-remove-overlays overlays)))

(defun TeX-fold-clearout-item ()
  "Permanently show the macro on which point currently is located."
  (interactive)
  (let ((overlays (overlays-at (point))))
    (TeX-fold-remove-overlays overlays)))

(defun TeX-fold-remove-overlays (overlays)
  "Remove all overlays set by TeX-fold in OVERLAYS.
Return non-nil if a removal happened, nil otherwise."
  (let (found)
    (while overlays
      (when (eq (overlay-get (car overlays) 'category) 'TeX-fold)
        (delete-overlay (car overlays))
        (setq found t))
      (setq overlays (cdr overlays)))
    found))


;;; Toggling

(defun TeX-fold-expand-spec (spec ov-start ov-end)
  "Expand instances of {<num>}, [<num>], <<num>>, and (<num>).
Replace them with the respective macro argument."
  (let ((spec-list (split-string spec "||"))
        (delims '((?\{ . ?\}) (?\[ . ?\]) (?< . ?>) (?\( . ?\)))))
    (cl-labels
        ((expand (spec &optional index)
           ;; If there is something to replace and the closing delimiter
           ;; matches the opening one…
           (if-let* (((string-match "\\([[{<(]\\)\\([1-9]\\)\\([]}>)]\\)"
                                    spec index))
                     (open (string-to-char (match-string 1 spec)))
                     (num (string-to-number (match-string 2 spec)))
                     (close (string-to-char (match-string 3 spec)))
                     ((equal close (cdr (assoc open delims)))))
               ;; … then replace it and move on.  Otherwise, it must have been
               ;; a spurious spec, so abort.
               (when-let* ((arg (car (save-match-data
                                       (TeX-fold-macro-nth-arg
                                        num ov-start ov-end (assoc open delims)))))
                           (spec* (replace-match arg nil t spec)))
                 (expand spec*
                         (+ (match-end 0) (- (length spec*) (length spec)))))
             ;; Nothing to replace: return the (completed) spec.
             spec)))
      (or (cl-loop for elt in spec-list
                   do (when-let* ((expanded (expand elt)))
                        (cl-return expanded)))
          TeX-fold-ellipsis))))

(defun TeX-fold-hide-item (ov)
  "Hide a single macro or environment.
That means, put respective properties onto overlay OV."
  (let* ((ov-start (overlay-start ov))
         (ov-end (overlay-end ov))
         (spec (overlay-get ov 'TeX-fold-display-string-spec))
         (computed (cond
                    ((stringp spec)
                     (TeX-fold-expand-spec spec ov-start ov-end))
                    ((functionp spec)
                     (let (arg arg-list
                               (n 1))
                       (while (setq arg (TeX-fold-macro-nth-arg
                                         n ov-start ov-end))
                         (unless (member (car arg) arg-list)
                           (setq arg-list (append arg-list (list (car arg)))))
                         (setq n (1+ n)))
                       (or (condition-case nil
                               (save-excursion
                                 (goto-char ov-start)
                                 (apply spec arg-list))
                             (error nil))
                           "[Error: No content or function found]")))
                    (t (or (TeX-fold-macro-nth-arg spec ov-start ov-end)
                           "[Error: No content found]"))))
         (display-string (if (listp computed) (car computed) computed))
         ;; (face (when (listp computed) (cadr computed)))
         )

    (if (eq computed 'abort)
        ;; Abort folding if computed result is the symbol `abort'.
        ;; This allows programmatic customization.
        ;; Suggested by Paul Nelson <ultrono@gmail.com>.
        ;; <URL:https://lists.gnu.org/r/auctex/2023-08/msg00026.html>
        (progn (delete-overlay ov)
               t ; so that `TeX-fold-dwim' "gives up"
               )
      ;; Do nothing if the overlay is empty.
      (when (and ov-start ov-end)
        ;; Cater for zero-length display strings.
        (when (string= display-string "") (setq display-string TeX-fold-ellipsis))
        ;; Add a linebreak to the display string and adjust the overlay end
        ;; in case of an overfull line.
        (when (TeX-fold-overfull-p ov-start ov-end display-string)
          (setq display-string (concat display-string "\n"))
          (move-overlay ov ov-start (save-excursion
                                      (goto-char ov-end)
                                      (skip-chars-forward " \t")
                                      (point))))
        (overlay-put ov 'mouse-face 'highlight)
        (when font-lock-mode
          ;; Add raise adjustment for superscript and subscript.
          ;; (bug#42209)
          (setq display-string
                (propertize display-string
                            'display (get-text-property ov-start 'display))))
        (overlay-put ov 'display display-string)
        (when font-lock-mode
          (overlay-put ov 'face TeX-fold-folded-face))
        (unless (zerop TeX-fold-help-echo-max-length)
          (overlay-put ov 'help-echo (TeX-fold-make-help-echo
                                      (overlay-start ov) (overlay-end ov))))))))

(defun TeX-fold-show-item (ov)
  "Show a single LaTeX macro or environment.
Remove the respective properties from the overlay OV."
  (overlay-put ov 'mouse-face nil)
  (overlay-put ov 'display nil)
  (overlay-put ov 'help-echo nil)
  (when font-lock-mode
    (overlay-put ov 'face TeX-fold-unfolded-face)))

(defun TeX-fold-auto-reveal-p (mode)
  "Decide whether to auto-reveal.
Return non-nil if folded region should be auto-opened.
See `TeX-fold-auto-reveal' for definitions of MODE."
  (cond ((symbolp mode)
         (and (boundp mode)
              (symbol-value mode)))
        ((consp mode)
         (apply (car mode) (cdr mode)))
        (t mode)))

(defun TeX-fold-arrived-via (&rest list)
  "Indicate auto-opening.
Return non-nil if called by one of the commands in LIST."
  (memq this-command list))

;; Copy and adaption of `reveal-post-command' from reveal.el in GNU
;; Emacs on 2004-07-04.
(defun TeX-fold-post-command ()
  ;; `with-local-quit' is not supported in XEmacs.
  (condition-case nil
      (let ((inhibit-quit nil))
        (condition-case err
            (let* ((spots (TeX-fold-partition-list
                           (lambda (x)
                             ;; We refresh any spot in the current
                             ;; window as well as any spots associated
                             ;; with a dead window or a window which
                             ;; does not show this buffer any more.
                             (or (eq (car x) (selected-window))
                                 (not (window-live-p (car x)))
                                 (not (eq (window-buffer (car x))
                                          (current-buffer)))))
                           TeX-fold-open-spots))
                   (old-ols (mapcar #'cdr (car spots))))
              (setq TeX-fold-open-spots (cdr spots))
              (when (or disable-point-adjustment
                        global-disable-point-adjustment
                        (TeX-fold-auto-reveal-p TeX-fold-auto-reveal))
                ;; Open new overlays.
                (dolist (ol (nconc (when (and TeX-fold-unfold-around-mark
                                              (TeX-active-mark))
                                     (overlays-at (mark)))
                                   (overlays-at (point))))
                  (when (eq (overlay-get ol 'category) 'TeX-fold)
                    (push (cons (selected-window) ol) TeX-fold-open-spots)
                    (setq old-ols (delq ol old-ols))
                    (TeX-fold-show-item ol))))
              ;; Close old overlays.
              (dolist (ol old-ols)
                (when (and (eq (current-buffer) (overlay-buffer ol))
                           (not (rassq ol TeX-fold-open-spots)))
                  (if (and (>= (point) (overlay-start ol))
                           (<= (point) (overlay-end ol)))
                      ;; Still near the overlay: keep it open.
                      (push (cons (selected-window) ol) TeX-fold-open-spots)
                    ;; Really close it.
                    (TeX-fold-hide-item ol)))))
          (error (message "TeX-fold: %s" err))))
    (quit (setq quit-flag t))))


;;; Misc

;; Copy and adaption of `cvs-partition' from pcvs-util.el in GNU Emacs
;; on 2004-07-05 to make tex-fold.el mainly self-contained.
(defun TeX-fold-partition-list (p l)
  "Partition a list L into two lists based on predicate P.
The function returns a `cons' cell where the `car' contains
elements of L for which P is true while the `cdr' contains
the other elements.  The ordering among elements is maintained."
  (let (car cdr)
    (dolist (x l)
      (if (funcall p x) (push x car) (push x cdr)))
    (cons (nreverse car) (nreverse cdr))))


;;; The mode

;;;###autoload
(define-minor-mode TeX-fold-mode
  "Minor mode for hiding and revealing macros and environments.

Called interactively, with no prefix argument, toggle the mode.
With universal prefix ARG (or if ARG is nil) turn mode on.
With zero or negative ARG turn mode off."
  :init-value nil
  :lighter nil
  :keymap (list (cons TeX-fold-command-prefix TeX-fold-keymap))
  (if TeX-fold-mode
      (progn
        ;; The value t causes problem when body text is hidden in
        ;; outline-minor-mode. (bug#36651)
        ;; In addition, it's better not to override user preference
        ;; without good reason.
        ;; (set (make-local-variable 'search-invisible) t)
        (add-hook 'post-command-hook #'TeX-fold-post-command nil t)
        (add-hook 'LaTeX-fill-newline-hook #'TeX-fold-update-at-point nil t)
        (add-hook 'TeX-after-insert-macro-hook
                  (lambda ()
                    (when (and TeX-fold-mode TeX-fold-auto)
                      (save-excursion
                        (backward-char)
                        (or (TeX-fold-item 'macro)
                            (TeX-fold-item 'math)
                            (TeX-fold-item 'env))))))
        ;; Update the `TeX-fold-*-spec-list-internal' variables.
        (dolist (elt '("macro" "env" "math"))
          (set (intern (format "TeX-fold-%s-spec-list-internal" elt))
               ;; Append the value of `TeX-fold-*-spec-list' to the
               ;; mode-specific `<mode-prefix>-fold-*-spec-list' variable.
               (append (symbol-value (intern (format "TeX-fold-%s-spec-list"
                                                     elt)))
                       (let ((symbol (intern (format "%s-fold-%s-spec-list"
                                                     (TeX-mode-prefix) elt))))
                         (when (boundp symbol)
                           (symbol-value symbol)))))))
    ;; (kill-local-variable 'search-invisible)
    (remove-hook 'post-command-hook #'TeX-fold-post-command t)
    (remove-hook 'LaTeX-fill-newline-hook #'TeX-fold-update-at-point t)
    (TeX-fold-clearout-buffer))
  (TeX-set-mode-name))

;;;###autoload
(defalias 'tex-fold-mode #'TeX-fold-mode)

;;; Miscellaneous folding

;; This section provides functions for use in
;; `TeX-fold-region-functions'.

;;;; Verbatim constructs

(defun TeX-fold--verb-data (&rest _args)
  "Return data describing verbatim macro at point.
Returns list of the form (START END CONTENT).  This should be called
only in LaTeX modes."
  (when-let* ((boundaries (LaTeX-verbatim-macro-boundaries))
              (bound-start (car boundaries))
              (bound-end (cdr boundaries))
              (end-delim-char (char-before bound-end))
              (start-delim-char (if (= end-delim-char ?\})
                                    ?\{
                                  end-delim-char))
              (start-delim (char-to-string start-delim-char))
              (verb-arg-start
               (1+ (progn
                     (goto-char bound-end)
                     (if (string= start-delim TeX-grop)
                         (progn (backward-sexp) (point))
                       (forward-char -1)
                       (search-backward start-delim bound-start t)))))
              (verb-arg-end (1- bound-end)))
    (list bound-start
          bound-end
          (buffer-substring verb-arg-start verb-arg-end))))

(defun TeX-fold-verbs (start end)
  "In LaTeX modes, fold verbatim macros between START and END.
Replaces the verbatim content with its own text."
  (when (derived-mode-p 'LaTeX-mode)
    (save-excursion
      (goto-char start)
      (let ((re (concat (regexp-quote TeX-esc)
                        (regexp-opt
                         (append
                          (LaTeX-verbatim-macros-with-braces)
                          (LaTeX-verbatim-macros-with-delims))))))
        (while (let ((case-fold-search nil))
                 (re-search-forward re end t))
          (when-let* ((data (TeX-fold--verb-data))
                      (verb-start (nth 0 data))
                      (verb-end (nth 1 data))
                      (verb-content (nth 2 data)))
            (TeX-fold-make-overlay
             verb-start verb-end
             'misc
             (lambda (&rest _args)
               (nth 2 (TeX-fold--verb-data)))
             verb-content)))))))

;;;; Quotes

(defcustom TeX-fold-open-quote "“"
  "Folded version of opening quote."
  :type 'string
  :package-version '(auctex . "14.0.8"))

(defcustom TeX-fold-close-quote "”"
  "Folded verison of closing quote."
  :type 'string
  :package-version '(auctex . "14.0.8"))

(defcustom TeX-fold-quotes-on-insert nil
  "Non-nil means to automatically fold LaTeX quotes when they are inserted.
Consulted by `TeX-insert-quote'."
  :type 'boolean
  :package-version '(auctex . "14.0.8"))

(defun TeX-fold-quotes (start end)
  "Fold LaTeX quotes between START and END.
Replaces opening and closing quotes with `TeX-fold-open-quote' and
`TeX-fold-close-quote', respectively, except in math environments,
verbatim contexts and comments."
  (pcase-let ((`(,open-quote ,close-quote _) (TeX-get-quote-characters)))
    (save-excursion
      (goto-char start)
      (let ((regexp (regexp-opt (list open-quote close-quote))))
        (while (re-search-forward regexp end t)
          (let ((str (if (string= (match-string-no-properties 0) open-quote)
                         TeX-fold-open-quote
                       TeX-fold-close-quote))
                (quote-start (match-beginning 0))
                (quote-end (match-end 0)))
            (unless (or (and (fboundp 'font-latex-faces-present-p)
                             (font-latex-faces-present-p
                              '(tex-math
                                font-latex-verbatim-face
                                font-latex-math-face
                                font-lock-comment-face)
                              quote-start))
                        (texmathp))
              (TeX-fold-make-overlay quote-start quote-end 'misc str str))))))))

(provide 'tex-fold)

;;; tex-fold.el ends here

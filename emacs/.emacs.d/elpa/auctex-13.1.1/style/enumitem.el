;;; enumitem.el --- AUCTeX style for `enumitem.sty' (v3.9)  -*- lexical-binding: t; -*-

;; Copyright (C) 2015--2021 Free Software Foundation, Inc.

;; Author: Arash Esbati <arash@gnu.org>
;; Maintainer: auctex-devel@gnu.org
;; Created: 2014-10-20
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; This file adds support for `enumitem.sty' (v3.9) from 2019/06/20.
;; `enumitem.sty' is part of TeXLive.

;; Tassilo Horn's `minted.el' was a major source of inspiration for
;; this style, many thanks to him (also for patiently answering my
;; many other questions, incl. the stupid ones.)

;; If things do not work or when in doubt, press `C-c C-n'.  Comments
;; for improvement are welcome.

;;; Code:

(require 'tex)
(require 'latex)
(eval-when-compile
  (require 'cl-lib))

;; Silence the compiler:
(declare-function font-latex-add-keywords
                  "font-latex"
                  (keywords class))

(defvar LaTeX-enumitem-key-val-options
  `(;; 3.1 Label and cross references format
    ("label"  ("\\alph*"  "\\Alph*"  "\\arabic*"
               "\\roman*" "\\Roman*" "\\value*"))
    ("label*" ("\\alph*"  "\\Alph*"  "\\arabic*"
               "\\roman*" "\\Roman*" "\\value*"))
    ("ref"    ("\\alph*"  "\\Alph*"  "\\arabic*"
               "\\roman*" "\\Roman*" "\\value*"))
    ("font" ,(mapcar (lambda (mac)
                       (concat TeX-esc mac))
                     '(;; family
                       "rmfamily" "sffamily" "ttfamily"
                       ;; series
                       "mdseries" "bfseries"
                       ;; shape
                       "upshape" "itshape" "slshape" "scshape"
                       ;; size
                       "tiny"  "scriptsize" "footnotesize"
                       "small" "normalsize" "large"
                       "Large" "LARGE" "huge" "Huge"
                       ;; reset macro
                       "normalfont")))
    ("format" ,(mapcar (lambda (mac)
                         (concat TeX-esc mac))
                       '(;; family
                         "rmfamily" "sffamily" "ttfamily"
                         ;; series
                         "mdseries" "bfseries"
                         ;; shape
                         "upshape" "itshape" "slshape" "scshape"
                         ;; size
                         "tiny"  "scriptsize" "footnotesize"
                         "small" "normalsize" "large"
                         "Large" "LARGE" "huge" "Huge"
                         ;; reset macro
                         "normalfont")))
    ("align" ("left" "right" "parleft"))
    ;; 3.2 Horizontal spacing of labels
    ("labelindent" ("*" "!"))
    ("left")
    ("leftmargin"  ("*" "!"))
    ("itemindent"  ("*" "!"))
    ("labelsep"    ("*" "!"))
    ("labelwidth"  ("*" "!"))
    ("widest")
    ("widest*")
    ("labelsep*")
    ("labelindent*")
    ("rightmargin")
    ;; Vertical Spacing
    ("topsep")
    ("partopsep")
    ("parsep")
    ("itemsep")
    ;; 3.3 Numbering, stopping, and resuming
    ("start")
    ("resume")
    ("resume*")
    ;; 3.4 Series
    ("series")
    ;; 3.5 Penalties
    ("beginpenalty")
    ("midpenalty")
    ("endpenalty")
    ;; 3.6 Injecting code
    ("before")
    ("before*")
    ("after")
    ("after*")
    ("first")
    ("first*")
    ;; 3.7 Description styles
    ("style" ("standard" "unboxed" "nextline" "sameline" "multiline"))
    ;; 3.8 Compact lists
    ("noitemsep")
    ("nosep")
    ;; 3.9 Wide lists
    ("wide")
    ;; 4 Inline lists
    ("itemjoin")
    ("itemjoin*")
    ("afterlabel")
    ("mode" ("boxed" "unboxed")))
  "Key=value options for enumitem macros and environments.")

(defun LaTeX-enumitem-key-val-options ()
  "Return an updated list of key=vals from enumitem package."
  (append
   ;; New keys are valueless, so take them as is:
   (when (LaTeX-enumitem-SetEnumitemKey-list)
     (LaTeX-enumitem-SetEnumitemKey-list))
   ;; New values defined available keys: We have to collect predefined
   ;; values (if any) from `LaTeX-enumitem-key-val-options' (stored in
   ;; `vals-predefined') and user-defined values (stored in
   ;; `vals-parsed') which were parsed and added to `result' in the
   ;; previous run of `dolist' and then combine them as value to a
   ;; key.
   (when (LaTeX-enumitem-SetEnumitemValue-list)
     (let (result)
       (dolist (keyvals (LaTeX-enumitem-SetEnumitemValue-list) result)
         (let* ((key (nth 1 keyvals))
                (val (nth 2 keyvals))
                (vals-predefined
                 (cadr (assoc key LaTeX-enumitem-key-val-options)))
                (vals-parsed (cadr (assoc key result))))
           ;; Remove entry in `result' if there is one for the `key':
           (when (assoc key result)
             (setq result (assq-delete-all (car (assoc key result))
                                           result)))
           ;; Add the entry to `result'; also remove any duplicates
           (cl-pushnew (list key (TeX-delete-duplicate-strings
                                  (append vals-parsed
                                          vals-predefined
                                          (list val))))
                       result :test #'equal)))))
   ;; New values to `align' key: We collect the predefined ones from
   ;; `LaTeX-enumitem-key-val-options' in `vals-predefined' and
   ;; prepend them to newly parsed ones:
   (when (LaTeX-enumitem-SetLabelAlign-list)
     (let* ((key "align")
            (vals (mapcar #'car (LaTeX-enumitem-SetLabelAlign-list)))
            (vals-predefined (cadr
                              (assoc key LaTeX-enumitem-key-val-options))))
       `(("align" ,(TeX-delete-duplicate-strings
                    (append vals-predefined vals))))))
   ;; Predefined key=vals:
   LaTeX-enumitem-key-val-options))

;; Setup for \newlist:

(TeX-auto-add-type "enumitem-newlist" "LaTeX")

(defvar LaTeX-enumitem-newlist-regexp
  '("\\\\newlist{\\([^}]+\\)}{\\([^}]+\\)}"
    (1 2) LaTeX-auto-enumitem-newlist)
  "Matches the arguments of `\\newlist' from `enumitem' package.")

;; Setup for \SetLabelAlign:

(TeX-auto-add-type "enumitem-SetLabelAlign" "LaTeX")

(defvar LaTeX-enumitem-SetLabelAlign-regexp
  '("\\\\SetLabelAlign{\\([^}]+\\)}"
    1 LaTeX-auto-enumitem-SetLabelAlign)
  "Matches the argument of `\\SetLabelAlign' from `enumitem' package.")

;; Setup for \SetEnumitemKey:

(TeX-auto-add-type "enumitem-SetEnumitemKey" "LaTeX")

(defvar LaTeX-enumitem-SetEnumitemKey-regexp
  '("\\\\SetEnumitemKey{\\([^}]+\\)}"
    1 LaTeX-auto-enumitem-SetEnumitemKey)
  "Matches the arguments of `\\SetEnumitemKey' from `enumitem' package.")

;; Setup for \SetEnumitemValue:

(TeX-auto-add-type "enumitem-SetEnumitemValue" "LaTeX")

;; Upon Tassilo's recommendation, we include also `0' so that we can
;; use the function `LaTeX-enumitem-SetEnumitemValue-list' while we
;; make sure that `TeX-auto-list-information' doesn't remove multiple
;; defined values to a specific key.  For this reason, we also ignore
;; the 3. argument to the `\SetEnumitemValue' macro (i.e., a third
;; {\\([^}]+\\)} in regex) so that we don't pollute the generated
;; `docname.el' with unnecessary (La)TeX code.
(defvar LaTeX-enumitem-SetEnumitemValue-regexp
  '("\\\\SetEnumitemValue{\\([^}]+\\)}{\\([^}]+\\)}"
    (0 1 2) LaTeX-auto-enumitem-SetEnumitemValue)
  "Matches the arguments of `\\SetEnumitemValue' from `enumitem' package.")

;; Plug them into the machinery.
(defun LaTeX-enumitem-auto-prepare ()
  "Clear various `LaTeX-enumitem-*' before parsing."
  (setq LaTeX-auto-enumitem-newlist          nil
        LaTeX-auto-enumitem-SetLabelAlign    nil
        LaTeX-auto-enumitem-SetEnumitemKey   nil
        LaTeX-auto-enumitem-SetEnumitemValue nil))

(defun LaTeX-enumitem-auto-cleanup ()
  "Move parsing results into right places for further usage."
  ;; \newlist{<name>}{<type>}{<max-depth>}
  ;; env=<name>, type=<type>, ignored=<max-depth>
  (dolist (env-type (LaTeX-enumitem-newlist-list))
    (let* ((env  (car env-type))
           (type (cadr env-type)))
      (LaTeX-add-environments (list env 'LaTeX-enumitem-env-with-opts))
      ;; Tell AUCTeX about parsed description like environments.
      (when (or (string-equal type "description")
                (string-equal type "description*"))
        (add-to-list 'LaTeX-item-list `(,env . LaTeX-item-argument)))
      ;; Add new env's to `ispell-tex-skip-alist': skip the opt. arg:
      (TeX-ispell-skip-setcdr `((,env ispell-tex-arg-end 0))))))

(add-hook 'TeX-auto-prepare-hook #'LaTeX-enumitem-auto-prepare t)
(add-hook 'TeX-auto-cleanup-hook #'LaTeX-enumitem-auto-cleanup t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(defun LaTeX-enumitem-env-with-opts (env)
  "Insert ENV provided by `enumitem' package."
  (LaTeX-insert-environment
   env
   (let ((opts (TeX-read-key-val t (LaTeX-enumitem-key-val-options))))
     (when (and opts (not (string-equal opts "")))
       (format "[%s]" opts))))
  (if (TeX-active-mark)
      (progn
        (LaTeX-find-matching-begin)
        (end-of-line 1))
    (end-of-line 0))
  (delete-char 1)
  (when (looking-at (concat "^[ \t]+$\\|"
                            "^[ \t]*" TeX-comment-start-regexp "+[ \t]*$"))
    (delete-region (point) (line-end-position)))
  (delete-horizontal-space)
  ;; Deactivate the mark here in order to prevent `TeX-parse-macro'
  ;; from swapping point and mark and the \item ending up right after
  ;; \begin{...}.
  (deactivate-mark)
  (LaTeX-insert-item)
  ;; The inserted \item may have outdented the first line to the
  ;; right.  Fill it, if appropriate.
  (when (and auto-fill-function
             (not (looking-at "$"))
             (not (assoc env LaTeX-indent-environment-list))
             (> (- (line-end-position) (line-beginning-position))
                (current-fill-column)))
    (LaTeX-fill-paragraph nil)))

(defun LaTeX-arg-enumitem-SetLabelAlign (optional)
  "Ask and insert a new type (value) for the \"align\" key.
Insert value in brackets if OPTIONAL is non-nil."
  (let ((val (TeX-read-string "Alignment: ")))
    (TeX-argument-insert val optional)
    (LaTeX-add-enumitem-SetLabelAligns val)))

(defun LaTeX-arg-enumitem-SetEnumitemKey (optional)
  "Ask and insert a new key and its replacement.
Insert key and value in brackets if OPTIONAL is non-nil."
  (let ((key     (TeX-read-string "New Key: "))
        (replace (TeX-read-key-val optional
                                   (LaTeX-enumitem-key-val-options)
                                   "Replacement")))
    (TeX-argument-insert key     optional)
    (TeX-argument-insert replace optional)
    (LaTeX-add-enumitem-SetEnumitemKeys key)))

;; In `LaTeX-enumitem-SetEnumitemValue-regexp', we match (0 1 2).
;; When adding a new `key=val', we need something unique for `0'-match
;; until the next `C-c C-n'.  We mimic that regex-match bei concat'ing
;; the elements and pass the result to
;; `LaTeX-add-enumitem-SetEnumitemValues'.  It will vanish upon next
;; invocation of `C-c C-n'.
(defun LaTeX-arg-enumitem-SetEnumitemValue (optional)
  "Ask and insert for a new value added to an existing key.
Insert key and value in brackets if OPTIONAL is non-nil."
  (let ((key (completing-read  "Key: " LaTeX-enumitem-key-val-options))
        (val (TeX-read-string "String value: ")))
    (TeX-argument-insert key optional)
    (TeX-argument-insert val optional)
    (LaTeX-add-enumitem-SetEnumitemValues
     (list (concat "\\SetEnumitemValue{" key "}{" val "}")
           key val))))

(defun LaTeX-arg-enumitem-setlist (_optional)
  "Insert the arguments of `\\setlist' macro from `enumitem' package.
This function inserts three arguments.  The first optional
argument is only available when the package option `sizes' is
provided.  OPTIONAL is ignored."
  ;; First opt. argument:
  (when (LaTeX-provided-package-options-member "enumitem" "sizes")
    (let* ((TeX-arg-opening-brace "<")
           (TeX-arg-closing-brace ">")
           (sizes '("script" "tiny" "footnote" "small" "normal"
                    "large" "Large" "LARGE" "huge" "Huge"))
           (size (completing-read
                  (TeX-argument-prompt t nil "Size")
                  (append
                   (mapcar (lambda (x) (concat "-" x)) sizes)
                   (mapcar (lambda (x) (concat x "-")) sizes)
                   sizes))))
      (TeX-argument-insert size t)))
  ;; Second opt. argument:
  (TeX-argument-insert
   (mapconcat #'identity
              (TeX-completing-read-multiple
               (TeX-argument-prompt t nil "Environment(s), level(s)")
               (append
                (when (LaTeX-provided-package-options-member
                       "enumitem" "includedisplayed")
                  '("trivlist"))
                (mapcar #'car (LaTeX-enumitem-newlist-list))
                '("1" "2" "3" "4")))
              ",")
   t)
  ;; Mandatory argument:
  (TeX-argument-insert
   (TeX-read-key-val nil (LaTeX-enumitem-key-val-options))
   nil))

(TeX-add-style-hook
 "enumitem"
 (lambda ()

   ;; Add enumitem to the parser.
   (TeX-auto-add-regexp LaTeX-enumitem-newlist-regexp)
   (TeX-auto-add-regexp LaTeX-enumitem-SetEnumitemKey-regexp)
   (TeX-auto-add-regexp LaTeX-enumitem-SetEnumitemValue-regexp)
   (TeX-auto-add-regexp LaTeX-enumitem-SetLabelAlign-regexp)

   ;; Add the standard environments:
   (LaTeX-add-enumitem-newlists '("itemize"     "itemize")
                                '("enumerate"   "enumerate")
                                '("description" "description"))

   ;; Add the starred versions with the 'inline' package option:
   (when (LaTeX-provided-package-options-member "enumitem" "inline")
     (LaTeX-add-enumitem-newlists '("itemize*"     "itemize*")
                                  '("enumerate*"   "enumerate*")
                                  '("description*" "description*")))

   ;; Standard env's take key-val as optional argument.
   (LaTeX-add-environments
    '("itemize"      LaTeX-enumitem-env-with-opts)
    '("enumerate"    LaTeX-enumitem-env-with-opts)
    '("description"  LaTeX-enumitem-env-with-opts))

   ;; Make inline env's available with package option "inline"
   (when (LaTeX-provided-package-options-member "enumitem" "inline")
     (LaTeX-add-environments
      '("itemize*"     LaTeX-enumitem-env-with-opts)
      '("enumerate*"   LaTeX-enumitem-env-with-opts)
      '("description*" LaTeX-enumitem-env-with-opts))
     (add-to-list 'LaTeX-item-list '("description*" . LaTeX-item-argument)))

   (TeX-add-symbols
    ;; 6 Size dependent settings
    '("SetEnumitemSize" 2)

    ;; 7 Cloning the basic lists
    ;; The easy way would be:
    ;; '("newlist"
    ;;   "Name" (TeX-arg-eval
    ;;           completing-read "Type: "
    ;;                 '(("itemize")  ("enumerate")  ("description")
    ;;                   ("itemize*") ("enumerate*") ("description*")))
    ;;  "Max-depth")
    ;; But we go the extra mile to improve the user experience and add
    ;; the arguments directly to appropriate lists.
    ;; \newlist{<name>}{<type>}{<max-depth>}
    '("newlist"
      (TeX-arg-eval
       (lambda ()
         (let ((name (TeX-read-string "Name: "))
               (type (completing-read
                      "Type: "
                      (mapcar #'cadr (LaTeX-enumitem-newlist-list))))
               (depth (TeX-read-string "Max-depth: ")))
           (when (or (string-equal type "description")
                     (string-equal type "description*"))
             (add-to-list 'LaTeX-item-list `(,name . LaTeX-item-argument)))
           (LaTeX-add-environments `(,name LaTeX-enumitem-env-with-opts))
           (LaTeX-add-enumitem-newlists (list name type))
           (TeX-ispell-skip-setcdr `((,name ispell-tex-arg-end 0)))
           (TeX-argument-insert name nil)
           (TeX-argument-insert type nil)
           (format "%s" depth)))))

    ;; \renewlist{<name>}{<type>}{<max-depth>}
    '("renewlist"
      (TeX-arg-eval completing-read "Name: "
                    (mapcar #'car (LaTeX-enumitem-newlist-list)))
      (TeX-arg-eval completing-read "Type: "
                    (mapcar #'cadr (LaTeX-enumitem-newlist-list)))
      "Max-depth")

    ;; \setlist<size>[<names,levels>]{<key-vals>}
    '("setlist" LaTeX-arg-enumitem-setlist)

    ;; \setlist*[<names,levels>]{<key-vals>}
    '("setlist*"
      [TeX-arg-eval mapconcat #'identity
                    (TeX-completing-read-multiple
                     (TeX-argument-prompt t nil "Environment(s), level(s)")
                     (append
                      (when (LaTeX-provided-package-options-member "enumitem"
                                                                   "includedisplayed")
                        '("trivlist"))
                      (mapcar #'car (LaTeX-enumitem-newlist-list))
                      '("1" "2" "3" "4")))
                    ","]
      (TeX-arg-key-val (LaTeX-enumitem-key-val-options))) )

   ;; General commands:
   (TeX-add-symbols

    ;; Ask for an Integer and insert it.
    '("setlistdepth" "Integer")

    ;; Just add the braces and let the user do the rest.
    '("AddEnumerateCounter" 3)
    '("AddEnumerateCounter*" 3)

    ;; "\restartlist" only works for lists defined with "resume" key.
    ;; We will not extract that information and leave that to users.
    ;; For completion, extract enumerated environments from
    ;; `LaTeX-enumitem-newlist-list' and add "enumerate" to them.
    '("restartlist"
      (TeX-arg-eval
       (lambda ()
         (let ((enums '("enumerate")))
           (when (LaTeX-provided-package-options-member "enumitem" "inline")
             (cl-pushnew "enumerate*" enums :test #'equal))
           (dolist (env-type (LaTeX-enumitem-newlist-list))
             (let ((env   (car env-type))
                   (type  (cadr env-type)))
               (when (or (string-equal type "enumerate")
                         (string-equal type "enumerate*"))
                 (cl-pushnew env enums :test #'equal))))
           (completing-read "List name: " enums)))))

    ;; "Align" is added as new value to "align" key in key-val list.
    '("SetLabelAlign" LaTeX-arg-enumitem-SetLabelAlign t)

    ;; "Key" will be parsed and added to key-val list.
    '("SetEnumitemKey" LaTeX-arg-enumitem-SetEnumitemKey)

    ;; "Key" and "Value" are added to our key-val list.
    '("SetEnumitemValue" LaTeX-arg-enumitem-SetEnumitemValue "Replacement")

    ;; v3.6 has a macro for visual debugging.
    '("DrawEnumitemLabel" 0))

   ;; Setting enumerate short label
   (when (LaTeX-provided-package-options-member "enumitem" "shortlabels")
     (TeX-add-symbols
      '("SetEnumerateShortLabel"
        (TeX-arg-eval completing-read "Key: "
                      '("A" "a" "I" "i" "1"))
        "Replacement")))

   ;; Add \labelindent to list of known lengths:
   (LaTeX-add-lengths "labelindent")

   ;; Fontification
   (when (and (featurep 'font-latex)
              (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("newlist"             "{{{")
                                ("renewlist"           "{{{")
                                ("SetEnumitemSize"     "{{")
                                ("AddEnumerateCounter" "*{{{")
                                ("SetLabelAlign"       "{{")
                                ("SetEnumitemKey"      "{{" )
                                ("SetEnumitemValue"    "{{{"))
                              'function)
     ;; Cater for additional optionals arg <size> based on package
     ;; option 'sizes':
     (font-latex-add-keywords
      (if (LaTeX-provided-package-options-member "enumitem" "sizes")
          '(("setlist" "*<[{"))
        '(("setlist" "*[{")))
      'function)

     (font-latex-add-keywords '(("restartlist"            "{" )
                                ("setlistdepth"           "{" )
                                ("SetEnumerateShortLabel" "{{"))
                              'variable)))
 TeX-dialect)

(defvar LaTeX-enumitem-package-options
  '("inline" "shortlabels" "loadonly" "sizes"
    "ignoredisplayed" "includedisplayed")
  "Package options for the enumitem package.")

;;; enumitem.el ends here

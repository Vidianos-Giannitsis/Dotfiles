;;; toml-respects-json.el --- TOML (Tom's Obvious, Minimal Language) parser

;; Copyright (C) 2013 by Wataru MIYAGUNI

;; Author: Wataru MIYAGUNI <gonngo@gmail.com>
;; URL: https://github.com/gongo/emacs-toml
;; Keywords: toml parser
;; Version: 10.0.1

;; MIT License
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:

;; This is a library for parsing TOML (Tom's Obvious, Minimal
;; Language).

;; Learn all about TOML here: https://github.com/mojombo/toml

;; Inspired by json.el.  thanks!!
;;
;; This is a fork of the original package. This one follows convention per json.el
;; The fork repo is maintained here: https://github.com/FelipeLema/emacs-toml/tree/follow-json.el-repr
;; This code has been modified to avoid name clash

;;; Code:

(require 'parse-time)

(defconst toml-respects-json->special-escape-characters
  '(?b ?t ?n ?f ?r ?\" ?\/ ?\\)
  "Characters which are escaped in TOML.

\\b     - backspace       (U+0008)
\\t     - tab             (U+0009)
\\n     - linefeed        (U+000A)
\\f     - form feed       (U+000C)
\\r     - carriage return (U+000D)
\\\"     - quote           (U+0022)
\\\/     - slash           (U+002F)
\\\\     - backslash       (U+005C)

notes:

 Excluded four hex (\\uXXXX).  Do check in `toml-respects-json:read-escaped-char'")

(defconst toml-respects-json->read-table
  (let ((table
         '((?t  . toml-respects-json:read-boolean)
           (?f  . toml-respects-json:read-boolean)
           (?\[ . toml-respects-json:read-array)
	   (?{  . toml-respects-json:read-inline-table)
           (?\" . toml-respects-json:read-string))))
    (mapc (lambda (char)
            (push (cons char 'toml-respects-json:read-start-with-number) table))
          '(?- ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
    table))

(defconst toml-respects-json->regexp-datetime
  "\\([0-9]\\{4\\}\\)-\\(0[1-9]\\|1[0-2]\\)-\\(0[1-9]\\|[1-2][0-9]\\|3[0-1]\\)T\\([0-1][0-9]\\|2[0-4]\\):\\([0-5][0-9]\\):\\([0-5][0-9]\\)Z"
  "Regular expression for a datetime (Zulu time format).")

(defconst toml-respects-json->regexp-numeric
  "\\(-?[0-9]+[\\.0-9\\]*\\)"
  "Regular expression for a numeric.")

;; Error conditions

(put 'toml-respects-json-error 'error-message "Unknown TOML error")
(put 'toml-respects-json-error 'error-conditions '(toml-respects-json-error error))

(put 'toml-respects-json-respects-json-string-error 'error-message "Bad string")
(put 'toml-respects-json-string-error 'error-conditions
     '(toml-respects-json-string-error toml-respects-json-error error))

(put 'toml-respects-json-string-escape-error 'error-message "Bad escaped string")
(put 'toml-respects-json-string-escape-error 'error-conditions
     '(toml-respects-json-string-escape-error toml-respects-json-string-error toml-respects-json-error error))

(put 'toml-respects-json-string-unicode-escape-error 'error-message "Bad unicode escaped string")
(put 'toml-respects-json-string-unicode-escape-error 'error-conditions
     '(toml-respects-json-string-unicode-escape-error
       toml-respects-json-string-escape-error
       toml-respects-json-string-error toml-respects-json-error error))

(put 'toml-respects-json-boolean-error 'error-message "Bad boolean")
(put 'toml-respects-json-boolean-error 'error-conditions
     '(toml-respects-json-boolean-error toml-respects-json-error error))

(put 'toml-respects-json-datetime-error 'error-message "Bad datetime")
(put 'toml-respects-json-datetime-error 'error-conditions
     '(toml-respects-json-datetime-error toml-respects-json-error error))

(put 'toml-respects-json-numeric-error 'error-message "Bad numeric")
(put 'toml-respects-json-numeric-error 'error-conditions
     '(toml-respects-json-numeric-error toml-respects-json-error error))

(put 'toml-respects-json-start-with-number-error 'error-message "Bad start-with-number")
(put 'toml-respects-json-start-with-number-error 'error-conditions
     '(toml-respects-json-start-with-number-error toml-respects-json-error error))

(put 'toml-respects-json-array-error 'error-message "Bad array")
(put 'toml-respects-json-array-error 'error-conditions
     '(toml-respects-json-array-error toml-respects-json-error error))

(put 'toml-respects-json-key-error 'error-message "Bad key")
(put 'toml-respects-json-key-error 'error-conditions
     '(toml-respects-json-key-error toml-respects-json-error error))

(put 'toml-respects-json-keygroup-error 'error-message "Bad keygroup")
(put 'toml-respects-json-keygroup-error 'error-conditions
     '(toml-respects-json-keygroup-error toml-respects-json-error error))

(put 'toml-respects-json-value-error 'error-message "Bad readable value")
(put 'toml-respects-json-value-error 'error-conditions
     '(toml-respects-json-value-error toml-respects-json-error error))

(put 'toml-respects-json-redefine-keygroup-error 'error-message "Redefine keygroup error")
(put 'toml-respects-json-redefine-keygroup-error 'error-conditions
     '(toml-respects-json-redefine-keygroup-error toml-respects-json-error error))

(put 'toml-respects-json-redefine-key-error 'error-message "Redefine key error")
(put 'toml-respects-json-redefine-key-error 'error-conditions
     '(toml-respects-json-redefine-key-error toml-respects-json-error error))

(defun toml-respects-json:assoc (keys hash)
  "Example:

  (toml-respects-json:assoc '(\"servers\" \"alpha\" \"ip\") hash)"
  (let (element)
    (catch 'break
      (dolist (k keys)
        (unless (toml-respects-json:alistp hash) (throw 'break nil))
        (setq element (assoc k hash))
        (if element
            (setq hash (cdr element))
          (throw 'break nil)))
      element)))

(defun toml-respects-json:alistp (alist)
  (if (listp alist)
      (catch 'break
        (dolist (al alist)
          (unless (consp al) (throw 'break nil)))
        t)
    nil))

(defun toml-respects-json:end-of-line-p ()
  (looking-at "$"))

(defun toml-respects-json:end-of-buffer-p ()
  (eq (point) (point-max)))

(defun toml-respects-json:get-char-at-point ()
  (char-after (point)))

(defun toml-respects-json:seek-beginning-of-next-line ()
  "Move point to beginning of next line."
  (forward-line)
  (beginning-of-line))

(defun toml-respects-json:seek-readable-point ()
  "Move point forward, stopping readable point. (toml-respects-json->read-table).

Skip target:

- whitespace (Tab or Space)
- comment line (start with hash symbol)"
  (toml-respects-json:seek-non-whitespace)
  (while (and (not (toml-respects-json:end-of-buffer-p))
              (char-equal (toml-respects-json:get-char-at-point) ?#))
    (end-of-line)
    (unless (toml-respects-json:end-of-buffer-p)
      (toml-respects-json:seek-beginning-of-next-line)
      (toml-respects-json:seek-non-whitespace))))

(defun toml-respects-json:seek-non-whitespace ()
  "Move point forward, stopping before a char end-of-buffer or not in whitespace (tab and space)."
  (if (re-search-forward "[^ \t\n]" nil t)
      (backward-char)
    (re-search-forward "[ \t\n]+\\'" nil t)))

(defun toml-respects-json:search-forward (regexp)
  "Search forward from point for regular expression REGEXP.
Move point to the end of the occurrence found, and return point."
  (when (looking-at regexp)
    (forward-char (length (match-string-no-properties 0)))
    t))

(defun toml-respects-json:read-char (&optional char-p)
  "Read character at point.  Set point to next point.
If CHAR-P is nil, return character as string,
and not nil, return character as char.

Move point a character forward."
  (let ((char (toml-respects-json:get-char-at-point)))
    (forward-char)
    (if char-p char
      (char-to-string char))))

(defun toml-respects-json:read-escaped-char ()
  "Read escaped character at point.  Return character as string.
Move point to the end of read characters."
  (unless (eq ?\\ (toml-respects-json:read-char t))
    (signal 'toml-respects-json-string-escape-error (list (point))))
  (let* ((char (toml-respects-json:read-char t))
         (special (memq char toml-respects-json->special-escape-characters)))
    (cond
     (special (concat (list ?\\ char)))
     ((and (eq char ?u)
           (toml-respects-json:search-forward "[0-9A-Fa-f]\\{4\\}"))
      (concat "\\u" (match-string 0)))
     (t (signal 'toml-respects-json-string-unicode-escape-error (list (point)))))))

(defun toml-respects-json:read-string ()
  "Read string at point that surrounded by double quotation mark.
Move point to the end of read strings."
  (unless (eq ?\" (toml-respects-json:get-char-at-point))
    (signal 'toml-respects-json-string-error (list (point))))
  (let ((characters '())
        (char (toml-respects-json:read-char)))
    (while (not (eq char ?\"))
      (when (toml-respects-json:end-of-line-p)
        (signal 'toml-respects-json-string-error (list (point))))
      (push (if (eq char ?\\)
                (toml-respects-json:read-escaped-char)
              (toml-respects-json:read-char))
            characters)
      (setq char (toml-respects-json:get-char-at-point)))
    (forward-char)
    (apply 'concat (nreverse characters))))

(defun toml-respects-json:read-boolean ()
  "Read boolean at point.  Return t or nil.
Move point to the end of read boolean string."
  (cond
   ((toml-respects-json:search-forward "true") t)
   ((toml-respects-json:search-forward "false") :false)
   (t
    (signal 'toml-respects-json-boolean-error (list (point))))))

(defun toml-respects-json:read-datetime ()
  "Read datetime at point.
Return time list (seconds, minutes, hour, day, month and year).
Move point to the end of read datetime string."
  (unless (toml-respects-json:search-forward toml-respects-json->regexp-datetime)
    (signal 'toml-respects-json-datetime-error (list (point))))
  (let ((seconds (string-to-number (match-string-no-properties 6)))
        (minutes (string-to-number (match-string-no-properties 5)))
        (hour    (string-to-number (match-string-no-properties 4)))
        (day     (string-to-number (match-string-no-properties 3)))
        (month   (string-to-number (match-string-no-properties 2)))
        (year    (string-to-number (match-string-no-properties 1))))
    (list seconds minutes hour day month year)))

(defun toml-respects-json:read-numeric ()
  "Read numeric (integer or float) at point.  Return numeric.
Move point to the end of read numeric string."
  (unless (toml-respects-json:search-forward toml-respects-json->regexp-numeric)
    (signal 'toml-respects-json-numeric-error (list (point))))
  (let ((numeric (match-string-no-properties 0)))
    (if (with-temp-buffer
          (insert numeric)
          (goto-char (point-min))
          (search-forward "." nil t 2)) ;; e.g. "0.21.1" is NG
        (signal 'toml-respects-json-numeric-error (list (point)))
      (string-to-number numeric))))

(defun toml-respects-json:read-start-with-number ()
  "Read string that start with number at point.
Move point to the end of read string."
  (cond
   ((looking-at toml-respects-json->regexp-datetime) (toml-respects-json:read-datetime))
   ((looking-at toml-respects-json->regexp-numeric) (toml-respects-json:read-numeric))
   (t
    (signal 'toml-respects-json-start-with-number-error (list (point))))))

(defun toml-respects-json:read-array ()
  (unless (eq ?\[ (toml-respects-json:get-char-at-point))
    (signal 'toml-respects-json-array-error (list (point))))
  (mark-sexp)
  (forward-char)
  (let (elements char-after-read)
    (while (not (char-equal (toml-respects-json:get-char-at-point) ?\]))
      (push (toml-respects-json:read-value) elements)
      (toml-respects-json:seek-readable-point)
      (setq char-after-read (toml-respects-json:get-char-at-point))
      (unless (char-equal char-after-read ?\])
        (if (char-equal char-after-read ?,)
            (progn
              (forward-char)
              (toml-respects-json:seek-readable-point))
          (signal 'toml-respects-json-array-error (list (point))))))
    (forward-char)
    (vconcat
     (nreverse elements))))

(defun toml-respects-json:read-inline-table ()
  (unless (eq ?{ (toml-respects-json:get-char-at-point))
    (signal 'toml-respects-json-inline-table-error (list (point))))
  (forward-char)
  (let (elements char-after-read)
    (while (not (char-equal (toml-respects-json:get-char-at-point) ?}))
      (let ((key (toml-respects-json:read-key))
	    (value (toml-respects-json:read-value)))
	(push `(,key . ,value) elements))
      (toml-respects-json:seek-readable-point)
      (setq char-after-read (toml-respects-json:get-char-at-point))
      (unless (char-equal char-after-read ?})
        (if (char-equal char-after-read ?,)
            (progn
              (forward-char)
              (toml-respects-json:seek-readable-point))
          (signal 'toml-respects-json-array-error (list (point))))))
    (forward-char)
    (nreverse elements)))

(defun toml-respects-json:read-value ()
  (toml-respects-json:seek-readable-point)
  (if (toml-respects-json:end-of-buffer-p) nil
    (let ((read-function (cdr (assq (toml-respects-json:get-char-at-point) toml-respects-json->read-table))))
      (if (functionp read-function)
          (funcall read-function)
        (signal 'toml-respects-json-value-error (list (point)))))))

(defun toml-respects-json:read-keygroup ()
  (toml-respects-json:seek-readable-point)
  (let (keygroup)
    (while (and (not (toml-respects-json:end-of-buffer-p))
                (char-equal (toml-respects-json:get-char-at-point) ?\[))
      (if (toml-respects-json:search-forward "\\[\\([a-zA-Z][a-zA-Z0-9_\\.-]*\\)\\]")
          (let ((keygroup-string (match-string-no-properties 1)))
            (when (string-match "\\(_\\|\\.\\)\\'" keygroup-string)
              (signal 'toml-respects-json-keygroup-error (list (point))))
            (setq keygroup (split-string (match-string-no-properties 1) "\\.")))
        (signal 'toml-respects-json-keygroup-error (list (point))))
      (toml-respects-json:seek-readable-point))
    keygroup))

(defun toml-respects-json:read-key ()
  (toml-respects-json:seek-readable-point)
  (if (toml-respects-json:end-of-buffer-p) nil
    (if (toml-respects-json:search-forward "\\([a-zA-Z][a-zA-Z0-9_-]*\\) *= *")
        (let ((key (match-string-no-properties 1)))
          (when (string-match "_\\'" key)
            (signal 'toml-respects-json-key-error (list (point))))
          key)
      (signal 'toml-respects-json-key-error (list (point))))))

(defun toml-respects-json:make-hashes (keygroup key value hashes)
  (let ((keys (append keygroup (list key))))
    (toml-respects-json:make-hashes-of-alist hashes keys value)))

(defun toml-respects-json:make-hashes-of-alist (hashes keys value)
  (let* ((key (car keys))
         (descendants (cdr keys))
         (element (assoc key hashes))
         (children (cdr element)))
    (setq hashes (delete element hashes))
    (if descendants
        (progn
          (setq element (toml-respects-json:make-hashes-of-alist children descendants value))
          (add-to-list 'hashes (cons key element)))
      (progn
        (add-to-list 'hashes (cons key value))))))

(defun toml-respects-json:read ()
  "Parse and return the TOML-RESPECTS-JSON object following point."
  (let (current-keygroup
        current-key
        current-value
        hashes keygroup-history)
    (while (not (toml-respects-json:end-of-buffer-p))
      (toml-respects-json:seek-readable-point)

      ;; Check re-define keygroup
      (let ((keygroup (toml-respects-json:read-keygroup)))
        (when keygroup
          (if (and (not (eq keygroup current-keygroup))
                   (member keygroup keygroup-history))
              (signal 'toml-respects-json-redefine-keygroup-error (list (point)))
            (setq current-keygroup keygroup))))
      (add-to-list 'keygroup-history current-keygroup)

      (let ((elm (toml-respects-json:assoc current-keygroup hashes)))
        (when (and elm (not (toml-respects-json:alistp (cdr elm))))
          (signal 'toml-respects-json-redefine-key-error (list (point)))))

      ;; Check re-define key (with keygroup)
      (setq current-key (toml-respects-json:read-key))
      (when (toml-respects-json:assoc (append current-keygroup (list current-key)) hashes)
        (signal 'toml-respects-json-redefine-key-error (list (point))))

      (setq current-value (toml-respects-json:read-value))
      (setq hashes (toml-respects-json:make-hashes current-keygroup
                                       current-key
                                       current-value
                                       hashes))

      (toml-respects-json:seek-readable-point))
    hashes))

(defun toml-respects-json:read-from-string (string)
  "Read the TOML-RESPECTS-JSON object contained in STRING and return it."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (toml-respects-json:read)))

(defun toml-respects-json:read-from-file (file)
  "Read the TOML-RESPECTS-JSON object contained in FILE and return it."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (toml-respects-json:read)))

(provide 'toml-respects-json)

;;; toml-respects-json.el ends here

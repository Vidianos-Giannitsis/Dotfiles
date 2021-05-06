;;; math-at-point.el --- Compute arithmetic at point using quick-calc  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Shankar Rao

;; Author: Shankar Rao <shankar.rao@gmail.com>
;; URL: https://github.com/~shankar2k/math-at-point
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: calc, matching

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provide the function math-at-point that evaluates the
;; arithmetic expression at point using calc-eval, displays the result in the
;; minibuffer, copies the result into the kill ring, and can optionally be
;; inserted into the buffer after the expression.
;;
;; See documentation on https://github.com/shankar2k/math-at-point.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'cl-lib)

;;;; Variables


(rx-define map-unsigned-number
  (or (and (one-or-more digit)
           (optional (and "." (zero-or-more digit))))
      (and "." (one-or-more digit))))


(defvar map-simple-math-regexp
  (rx (and (optional (in "-" "+"))       ; optional sign in front of expression
           (zero-or-more (and map-unsigned-number
                              (zero-or-more blank) ; some blanks
                              (in "+" "-" "*" "/" "^") ; an arithmetic operation
                              (zero-or-more blank))) ; some blanks
           map-unsigned-number))
   "Regular expression for a simple algebraic expression.
involving decimal numbers, the operations +,-,*, /, and ^, and an
arbitrary amount of whitespace, all on one line.")


(defvar map-whole-string-math-regexp
  (concat (rx line-start (zero-or-more blank))
          map-simple-math-regexp
          (rx (zero-or-more blank) line-end))
  "Regular expression for a simple math expression matching a whole string.

The simple algebraic expression must not contain parens, but can
have an arbitrary amount of whitespace at the beginning and end.
Used by ``map--zero-out-balanced-parens''.")


(defvar map-number-regexp
  (rx (and (optional "-") map-unsigned-number))
  "Regular expression for a number.")


;;;; Functions


(defun map--balanced-paren-positions (str)
  "Return a list of positions of all balanced parens in STR.
The list is in reverse order, and has the property for any two
balanced paren pairs A and B, if A is contained in B then A's
position will occur before B's position in the list."
  (let (parens end)
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (and (search-forward "(" nil t)
                  (condition-case err
                      (setq end (scan-sexps (1- (point)) 1))
                    (scan-error nil)))
        (push (cons (match-beginning 0) end) parens)))
    parens))


(defun map--zero-out-balanced-parens (str)
  "Replace all math expressions delimited by parens in STR with \"0\"s.

For example, for the string:

\" 6.23+(3.789/(5-4)) + 6.4*(2 - (5+3) *736.83 ) /2000\",

this function returns the string:

\" 6.23+0000000000000 + 6.4*00000000000000000000 /2000\"."
  (cl-loop for (beg . end) in (map--balanced-paren-positions str)
           ;; the indices beg and end of a pair of balanced parens returned by
           ;; map--balanced-paren-positions are actually one character to the
           ;; right of each paren. Thus, to get the start and end indices of
           ;; the contents of the parens, we can use beg as is, and we have to
           ;; subtract 2 from end.
           do (when (string-match-p map-whole-string-math-regexp
                                    (substring str beg (- end 2)))
                  (setf (substring str (1- beg) (1- end))
                        (make-string (- end beg) ?0)))
           finally return str))

;;;; Commands


;;;###autoload
(defun math-at-point-simple (&optional insert)
  "Evaluate the simple math expression at point with `calc-eval'.

A simple math expression consists of decimal numbers, and the
operations +, -, *, /, and ^, and can be interspersed with
whitespace. A simple math expression cannot contain parens. The
whole expression must on the current line. 

The result is displayed in the minibuffer and copied into the
kill ring so that it can be pasted with ``yank''. If the point is
not inside a simple math expression, then instead run
``quick-calc''. 

If optional prefix argument INSERT is provided,
then insert the evaluation result after the expression, prefixed
by \"=\". If there was already a previous result, then replace
it."
  (interactive "P")
  (let ((p (point)))
    (beginning-of-line)
    (cl-loop while (re-search-forward map-simple-math-regexp
                                      (line-end-position) t)
             when (<= (match-beginning 0) p (match-end 0))
             return (let* ((m-string (match-string 0))
                           (result (calc-eval m-string)))
                      (if insert
                        (if (looking-at (concat "=" map-number-regexp))
                            (replace-match (concat "=" result))
                          (insert "=" result))
                        (goto-char p))
                      (kill-new result)
                      (message "Result %s => %s" m-string result))
             finally do (goto-char p) (quick-calc insert))))


;;;###autoload
(defun math-at-point (&optional insert)
  "Evaluate the math expression at point with `calc-eval'.

A math expression consists of decimal numbers, the operations +,
-, *, /, ^, and parentheses, and can be interspersed with
whitespace. The whole expression must be fully contained in the
current line. 

The result is displayed in the minibuffer and copied into the
kill ring (so that it can be pasted with ``yank''). If the point
is not within a math expression, then instead run `quick-calc'.

If optional prefix argument INSERT is provided, then insert the
evaluation result after the expression, prefixed by \"=\". If
there was already a previous result, then replace it."
  (interactive "P")
  (let* ((l-beg (line-beginning-position))
         (rel-p (- (point) l-beg))
         (this-line (thing-at-point 'line t))
         (zero-line (map--zero-out-balanced-parens this-line)))
    (cl-loop for pos = 0 then (match-end 0)
             while (and (string-match map-simple-math-regexp zero-line pos)
                        (< pos (length this-line)))
             when (<= (match-beginning 0) rel-p (match-end 0))
             return (let* ((m-end (match-end 0))
                           (m-string (substring this-line (match-beginning 0) m-end))
                           (result (save-match-data (calc-eval m-string))))
                      (when insert
                        (goto-char (+ l-beg m-end))
                        (if (looking-at (concat "=" map-number-regexp))
                            (replace-match (concat "=" result))
                          (insert "=" result)))
                      (kill-new result)
                      (message "Result %s => %s" m-string result))
             finally do (quick-calc insert))))


;;;; Footer

(provide 'math-at-point)

;;; math-at-point.el ends here

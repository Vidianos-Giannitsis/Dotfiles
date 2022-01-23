;;; maxima-font-lock.el --- Syntax highlighting for maxima.el              -*- lexical-binding: t; -*-

;; Copyright: (C) 2001 Jay Belanger
;; Copyright: (C) 2020 Fermin Munoz

;; Author: William F. Schelter
;;         Jay Belanger
;;         Fermin Munoz
;; Maintainer: Fermin Munoz <fmfs@posteo.net>
;; Revision: 1.16
;; Created: 30 April 2020
;; Version: 0.7.6
;; Keywords: maxima, tools, math
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

;;; This file is used for font-lock for maxima.el
;;
;; The keywords are divided into the following groups, following the
;; Maxima info files(NOT ALL OF THEN ALL IMPLEMENTED):
;; Functions (font-lock-builtin-face or font-lock-keyword-face)
;; Variables (font-lock-keyword-face)
;; Constants (font-lock-constant-face)
;; Keywords (font-lock-keyword-face)
;; Declarations (font-lock-keyword-face)
;; Operators (font-lock-keyword-face)
;; Property (font-lock-keyword-face)
;; Macros (font-lock-keyword-face)
;; Special operators (font-lock-keyword-face)
;; Special symbols (font-lock-keyword-face)

;;; Code:

(require 'font-lock)
(require 's)

(defvar maxima-font-lock-keywords-directory
  (format "%skeywords" (let* ((directory (file-name-directory load-file-name)))
			 ;; straight.el workaround
			 (if (s-contains-p "straight" directory)
			     (s-replace "build" "repos" directory)
			   directory)))
  "Keywords definition directory.")

(defvar maxima-font-lock-keywords-categories
  '("functions"
    "constants"
    "global"
    "graphic"
    "object"
    "operators"
    "options"
    "plot"
    "properties"
    "scene"
    "special"
    "system_variables")
  "Keywords categories.
Base on the types assigned by the maxima info manual.")

(defvar maxima-font-lock-functions
  (with-temp-buffer
    (insert-file-contents (format "%s/functions" maxima-font-lock-keywords-directory))
    (split-string (buffer-string) "\n" t)))


(defvar maxima-font-lock-match-functions
  (concat "\\<\\("
          (regexp-opt maxima-font-lock-functions)
	  "\\)\\>" )
  "Regexp to match the maxima functions.")

(defvar maxima-font-lock-constants
  (with-temp-buffer
    (insert-file-contents (format "%s/constants" maxima-font-lock-keywords-directory))
    (split-string (buffer-string) "\n" t)))

(defvar maxima-font-lock-match-constants
  (concat "\\<\\("
          (regexp-opt maxima-font-lock-constants)
	  "\\)\\>" )
  "Regexp to match the maxima constants.")

(defvar maxima-font-lock-operators
  (with-temp-buffer
    (insert-file-contents (format "%s/operators" maxima-font-lock-keywords-directory))
    (split-string (buffer-string) "\n" t)))

(defvar maxima-font-lock-match-operators
  (regexp-opt maxima-font-lock-operators t)
  "Regexp to match the maxima operators.")

(defvar maxima-font-lock-match-numbers
  "\\<\\([0-9]+\\)\\>"
  "Regexp to match the maxima numbers.")

(defvar maxima-font-lock-system-variables
  (with-temp-buffer
    (insert-file-contents (format "%s/system_variables" maxima-font-lock-keywords-directory))
    (split-string (buffer-string) "\n" t)))

(defvar maxima-font-lock-match-system-variables
  (concat "\\<\\("
          (regexp-opt maxima-font-lock-system-variables)
	  "\\)\\>" )
  "Regexp to match the maxima system variables.")

(defvar maxima-font-lock-properties
  (with-temp-buffer
    (insert-file-contents (format "%s/properties" maxima-font-lock-keywords-directory))
    (split-string (buffer-string) "\n" t)))

(defvar maxima-font-lock-match-properties
  (concat "\\<\\("
          (regexp-opt maxima-font-lock-properties)
	  "\\)\\>" )
  "Regexp to match the maxima properties.")

(defvar maxima-font-lock-special
  (with-temp-buffer
    (insert-file-contents (format "%s/special" maxima-font-lock-keywords-directory))
    (split-string (buffer-string) "\n" t)))

(defvar maxima-font-lock-match-special
  (concat "\\<\\("
          (regexp-opt maxima-font-lock-special)
	  "\\)\\>" )
  "Regexp to match the maxima special constructs.")


(defvar maxima-font-lock-keywords
  `((,maxima-font-lock-match-functions . font-lock-builtin-face)
    (,maxima-font-lock-match-constants . font-lock-constant-face)
    (,maxima-font-lock-match-numbers . font-lock-constant-face)
    (,maxima-font-lock-match-operators . font-lock-keyword-face)
    (,maxima-font-lock-match-system-variables . font-lock-keyword-face)
    (,maxima-font-lock-match-properties . font-lock-keyword-face)
    (,maxima-font-lock-match-special . font-lock-keyword-face))
  "Default expressions to highlight in Maxima mode.")

(defun maxima-font-lock-setup ()
  "Set up maxima font."
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '((maxima-font-lock-keywords)
          nil t)))

(add-hook 'maxima-mode-hook #'maxima-font-lock-setup)

(provide 'maxima-font-lock)
;;; maxima-font-lock.el ends here

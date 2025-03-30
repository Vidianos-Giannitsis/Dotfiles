;;; mastodon-widget.el --- Widget utilities -*- lexical-binding: t -*-

;; Copyright (C) 2020-2024 Marty Hiatt
;; Author: Marty Hiatt <mousebot@disroot.org>
;; Maintainer: Marty Hiatt <mousebot@disroot.org>
;; Homepage: https://codeberg.org/martianh/mastodon.el

;; This file is not part of GNU Emacs.

;; This file is part of mastodon.el.

;; mastodon.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; mastodon.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mastodon.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; some widget utilities for mastodon.el

;;; Code:

(require 'cl-lib)

(defvar mastodon-widget-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-2] 'widget-button-click)
    (define-key map [down-mouse-1] 'widget-button-click)
    (define-key map [touchscreen-begin] 'widget-button-click)
    ;; The following definition needs to avoid using escape sequences that
    ;; might get converted to ^M when building loaddefs.el
    (define-key map [(control ?m)] 'widget-button-press)
    map)
  "Keymap containing useful binding for buffers containing widgets.
Recommended as a parent keymap for modes using widgets.
Note that such modes will need to require wid-edit.")

(defface mastodon-widget-face
  '((t :inherit font-lock-function-name-face :weight bold :underline t))
  "Face for widgets."
  :group 'mastodon)

(defun mastodon-widget--return-item-widgets (list)
  "Return a list of item widgets for each item, a string, in LIST."
  (cl-loop for x in list
           collect `(choice-item :value ,x :format "%[%v%] "
                                 :keymap ,mastodon-widget-keymap)))

(defun mastodon-widget--format (str &optional padding newline)
  "Return a widget format string for STR, its name.
PADDING is an integer, for how much right-side padding to add."
  (concat "%[" (propertize str
                           'face 'mastodon-widget-face
                           'mastodon-tab-stop t)
          "%]: %v"
          (make-string padding ? )
          (if newline "\n" "")))

(defun mastodon-widget--create (kind type value notify-fun
                                     &optional newline)
  "Return a widget of KIND, with TYPE elements, and default VALUE.
KIND is a string, either Listing, Sort, Items, or Inbox, and will
be used for the widget's tag.
VALUE is a string, a member of TYPE.
NOTIFY-FUN is the widget's notify function."
  (let* ((val-length (length (if (symbolp value)
                                 (symbol-name value)
                               value)))
         (type-list (if (symbolp type)
                        (symbol-value type)
                      type))
         (longest (apply #'max
                         (mapcar #'length
                                 (if (symbolp (car type-list))
                                     (mapcar #'symbol-name type-list)
                                   type-list))))
         (padding (- longest val-length)))
    (if (not (member value type-list))
        (user-error "%s is not a member of %s" value type-list)
      (widget-create
       'menu-choice
       :tag kind
       :value value
       :args (mastodon-widget--return-item-widgets type-list)
       :help-echo (format "Select a %s kind" kind)
       :format (mastodon-widget--format kind padding newline)
       :notify notify-fun
       ;; eg format of notify-fun:
       ;; (lambda (widget &rest ignore)
       ;;   (let ((value (widget-value widget))
       ;;         (tag (widget-get widget :tag)))
       ;;     (notify-fun value)))
       :keymap mastodon-widget-keymap))))

(provide 'mastodon-widget)
;;; mastodon-widget.el ends here

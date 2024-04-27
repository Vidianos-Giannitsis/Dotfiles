;;; tab-jump-out.el --- Use tab to jump out of delimiter pairs  -*- lexical-binding: true -*-
;;
;; Copyright (C) 2015 Zhang Kai Yu
;; Copyright (C) 2023 Michael Kleehammer
;;
;; Version: 2.0.1
;; Author: Zhang Kai Yu <yeannylam@gmail.com>
;; Maintainer: Michael Kleehammer <michael@kleehammer.com>
;; URL: https://github.com/mkleehammer/tab-jump-out
;; Keywords: convenience
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is NOT part of GNU Emacs.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;
;; This package provide the ability to use tab to jump out of delimiter pairs.
;; Just run `tab-jump-out-global-mode' or `tab-jump-out-mode' and press TAB XD.

;;; Code:
;;
;; This package provides an Emacs minor mode that causes TAB to "jump out" of
;; parentheses, quotes, and similar pairs using the tab key.  When the cursor is
;; one of these characters, pressing tab causes point to move forward and skip
;; over the character.
;;
;; I use Emacs' `electric-pair-mode` which means entering an open parenthesis
;; causes the closing one to be entered also, with the cursor in between.  When
;; I finish typing the contents, the cursor is now on the closing parentheses
;; and I want to move out.  The electric-pair-mode allows you to type the ending
;; char (closing parenthesis, closing quote, etc.) and it will step over it
;; instead of inserting another.
;;
;; This mode simplifies this step by using the same key, tab, to step out
;; regardless of the closing character the cursor is on.  If it is a quote,
;; press tab; one of } ] >, just press tab.  It is a small convenience, but I
;; find it very handy.s
;;
;; The characters it will jump over are in tab-jump-out-delimiters and default
;; to:
;;
;;     } ] ) > : ; ` ' "
;;
;; If you need to indent a line that starts with one of these characters,
;; remember that Emacs' will indent the entire line if you press TAB anywhere on
;; the line in most programming modes.  If pressing TAB jumps over a character
;; and you wanted to indent, just press TAB again.  In modes that do not support
;; this, you may need to toggle the mode off or use the spacebar.
;;
;; Installation
;;
;; The tab-jump-out package is available on MELPA, so you can install with:
;;
;;     M-x package-install [RET] tab-jump-outs [RET]
;;
;; If you are using use-package, you can use this to enable globally:
;;
;;    (use-package tab-jump-out
;;      :ensure t
;;      :config (tab-jump-out-global-mode 1))
;;
;; If you want to install it only in some modes, don't enable global mode.  Add
;; a hook to the packages you want to use it in and call `(tab-jump-out-mode)`.
;;
;; With ya-snippet
;;
;; When editing a snippet template, tab moves between fields.  If the character
;; after a field is one tab-jump-out would normally jump over, it does so and
;; disables template editing.  For example, a Python function template might
;; look like this:
;;     def {1}({2}):
;;         {0}
;;
;; On solution is to disable tab-jump-out while expanding a template like so:
;;
;;     (add-hook 'yas-before-expand-snippet-hook (lambda() (tab-jump-out-mode -1)))
;;     (add-hook 'yas-after-exit-snippet-hook (lambda() (tab-jump-out-mode 1)))
;;
;; This unconditionally re-enables it instead of checking if it was enabled.  If
;; you sometimes disable it or you don't use it globally, use a local variable
;; that is set in the before hook if the mode was on.  Only re-enable it in the
;; exit hook of the local variable is set.

(defgroup tab-jump-out nil
  "Custom group for `tab-jump-out-mode'."
  :group 'editing
  :prefix "tab-jump-out-")

(defvar-local tab-jump-out-delimiters '("}" "]" ")" ">" ":" ";" "`" "'" "\"")
  ;; I'm not crazy about this description, but I want "jump" and "out" in it.
  ;; Normally I'd call the action "jumping over", but I've called it "move past"
  ;; so we don't have the word "jump" in there twice which sounds too busy.
  "The characters that tab will move past to jump out.")

(defun tab-jump-out-fallback ()
  "Fallback behavior of `tab-jump-out'."
  (let ((fallback-behavior (tab-jump-out-original-keybinding)))
    (if fallback-behavior
        (call-interactively fallback-behavior))))

(defun tab-jump-out-original-keybinding ()
  "Get current keys' binding as if `tab-jump-out-' didn't exist."
  ;; Copied from yasnippet
  ;;
  ;; Normally we're called by the Tab key, so this is looking for the function
  ;; that is normally bound to Tab.  We find it by temporarily turning off
  ;; tab-jump-out-mode and searching the keymaps.
  (let* ((tab-jump-out-mode nil)
         (keys (this-single-command-keys)))
    (or (key-binding keys t)
        (key-binding (tab-jump-out--fallback-translate-input keys) t))))

(defun tab-jump-out--fallback-translate-input (keys)
  "Emulate `read-key-sequence', at least what I think it does.

KEYS should be an untranslated key vector.  Returns a translated
vector of keys."
  ;; Copied from yasnippet
  (let ((retval [])
        (i 0))
    (while (< i (length keys))
      (let ((j i)
            (translated local-function-key-map))
        (while (and (< j (length keys))
                    translated
                    (keymapp translated))
          (setq translated (cdr (assoc (aref keys j) (remove 'keymap translated)))
                j (1+ j)))
        (setq retval (vconcat retval (cond ((symbolp translated)
                                            `[,translated])
                                           ((vectorp translated)
                                            translated)
                                           (t
                                            (substring keys i j)))))
        (setq i j)))
    retval))

;;;###autoload
(defun tab-jump-out ()
  "Use tab to jump out."
  (interactive)
  (if (and (char-after)
           (member (char-to-string (char-after)) tab-jump-out-delimiters))
      (forward-char 1)
    (tab-jump-out-fallback)))

(defvar tab-jump-out-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] 'tab-jump-out)
    map)
  "Keymap for `tab-jump-out-mode'.")

;;;###autoload
(define-minor-mode tab-jump-out-mode
  "A minor mode that allows you to jump out with tab."
  :group 'editing
  :keymap tab-jump-out-mode-map)

;;;###autoload
(define-globalized-minor-mode tab-jump-out-global-mode tab-jump-out-mode
  (lambda () (tab-jump-out-mode 1))
  :group 'editing)


(provide 'tab-jump-out)
;;; tab-jump-out.el ends here

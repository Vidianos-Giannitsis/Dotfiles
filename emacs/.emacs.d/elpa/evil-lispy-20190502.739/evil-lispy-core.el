(require 'lispy)
(require 'evil)
(require 'evil-lispy-customization)

(defun evil-lispy/state-entry ()
  (remove-hook 'activate-mark-hook #'evil-visual-activate-hook t)
  (lispy-mode 1))

(defun evil-lispy/state-exit ()
  (when (region-active-p) (deactivate-mark))
  (add-hook 'activate-mark-hook #'evil-visual-activate-hook nil t)
  (lispy-mode -1))

(defun evil-lispy/enter-state (direction extra-direction)
  "Return a lambda which enters Lispy state at the DIRECTION side of
the current form.  DIRECTION must be either 'left or 'right."
  (let ((f (intern (concat "lispy-" (symbol-name direction))))
        (g (intern (concat "lispy-" (symbol-name extra-direction)))))
    `(lambda ()
       (interactive)
       (when (looking-at lispy-left) (forward-char))
       (let ((pos (point)))
         (,f 1)
         (when (eq (point) pos) (,g 1)))
       (evil-lispy-state))))

(fset 'evil-lispy/enter-state-left (evil-lispy/enter-state 'left 'backward))
(fset 'evil-lispy/enter-state-right (evil-lispy/enter-state 'right 'forward))

(defun evil-lispy/enter-marked-state ()
  "Enters `lispy-state' with the current symbol under point marked."
  (interactive)
  (evil-lispy-state)
  (lispy-mark-symbol))

(defun evil-lispy/enter-visual-state ()
  "If we're in visual state, enter `lispy-state' with the current region
selected."
  (interactive)
  (let ((start (region-beginning))
        (end (region-end))
        (pos (point)))
    (evil-lispy-state)
    (set-mark (if (eq pos start) end start))))

(defun evil-lispy/enter-insert-state (direction extra-direction)
  "Return a lambda which enters Insert state at the DIRECTION side of
the current form.  DIRECTION must be either 'left or 'right."
  `(lambda ()
     (interactive)
     (funcall (evil-lispy/enter-state ',direction ',extra-direction))
     (evil-insert-state)
     (cond
      ((eq ',direction 'left)
       (forward-char)
       (unless (looking-at "\s")
         (insert ?\s)
         (backward-char)))
      ((eq ',direction 'right)
       (backward-char)
       (unless (looking-back "\s")
         (insert ?\s))))))

(fset 'evil-lispy/enter-insert-state-left
      (evil-lispy/enter-insert-state 'left 'backward))
(fset 'evil-lispy/enter-insert-state-right
      (evil-lispy/enter-insert-state 'right 'forward))

(defmacro evil-lispy/defnonstring-action (function-name
                                          action
                                          &rest args-to-action)
  "Define a function that will insert the pressed key in comments and strings,
or call ACTION (a function) otherwise, with ARGS-TO-ACTION."
  (declare (indent 1))
  `(defun ,function-name (arg)
     (interactive "p")
     (if (lispy--in-string-or-comment-p)
         (self-insert-command arg)
       (apply (quote ,action) ,args-to-action))))

(evil-lispy/defnonstring-action evil-lispy/insert-to-lispy-right
  evil-lispy/enter-state-right)
(evil-lispy/defnonstring-action evil-lispy/insert-to-lispy-left
  evil-lispy/enter-state-left)

;; ——— Operations ——————————————————————————————————————————————————————————————

(defun evil-lispy/describe ()
  (interactive)
  (save-excursion
    (lispy-mark-symbol)
    (lispy-describe-inline)))

(evil-define-state lispy
  "An evil state for Lispy, a precision editing mode for Lisp."
  :tag "<L>"
  :message "Entering evil-lispy state. Press ESC to get out and f1 for help."
  :cursor evil-lispy-cursor
  :entry-hook (evil-lispy/state-entry)
  :exit-hook (evil-lispy/state-exit)
  nil)

(defvar evil-lispy-mode-map (make-sparse-keymap))

(define-minor-mode evil-lispy-mode
  "A minor mode for integrating Evil and Lispy."
  :lighter " evil-lispy"
  :keymap evil-lispy-mode-map
  :after-hook (evil-normal-state))


(provide 'evil-lispy-core)

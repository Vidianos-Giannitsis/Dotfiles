(require 'lispy)
(require 'evil-lispy-core)

(define-key evil-lispy-state-map [escape] 'evil-normal-state)
(define-key evil-lispy-state-map (kbd "<f1>") 'evil-lispy-show-help)

(evil-define-key 'normal evil-lispy-mode-map
  "(" #'evil-lispy/enter-state-left
  ")" #'evil-lispy/enter-state-right
  (kbd "C-SPC") #'evil-lispy/enter-marked-state
  "<i" #'evil-lispy/enter-insert-state-left
  ">A" #'evil-lispy/enter-insert-state-right
  ">a" #'evil-lispy/enter-insert-state-right)

(evil-define-key 'visual evil-lispy-mode-map
  (kbd "RET") #'evil-lispy/enter-visual-state)

(evil-define-key 'normal evil-lispy-mode-map
  "K" #'evil-lispy/describe
  (kbd "M-k") #'lispy-kill-sentence
  (kbd "C-1") #'evil-lispy/describe
  (kbd "C-2") #'lispy-arglist-inline)

(evil-define-key 'insert evil-lispy-mode-map
  "(" #'lispy-parens

  "[" #'lispy-brackets
  "}" #'lispy-brackets

  "{" #'lispy-braces
  "\"" #'lispy-quotes
  ";" #'lispy-comment

  ;; ( should always insert parentheses
  ")" #'evil-lispy/insert-to-lispy-right
  "[" #'evil-lispy/insert-to-lispy-left
  "]" #'evil-lispy/insert-to-lispy-right

  (kbd "DEL") #'lispy-delete-backward
  (kbd "M-k") #'lispy-kill-sentence
  (kbd "C-1") #'lispy-describe-inline
  (kbd "C-2") #'lispy-arglist-inline)

(define-key lispy-mode-map "o" 'special-lispy-different)
(define-key lispy-mode-map "d" 'special-lispy-other-mode)
(define-key lispy-mode-map "i" 'special-lispy-flow)
(define-key lispy-mode-map "f" 'special-lispy-tab)

(provide 'evil-lispy-keybinds)

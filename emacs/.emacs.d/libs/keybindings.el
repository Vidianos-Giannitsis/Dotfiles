(general-create-definer my-leader-def
			 :prefix "SPC")

 (my-leader-def
  :states 'normal
  :keymaps 'override
   "!" 'shell-command
   "P" 'package-install
   "o" '(inferior-octave :which-key "octave")
   "D" 'dired
   "d" '(:ignore t :which-key "Dired functions")
   "q" '(:ignore t :which-key "Quickmarks")
   "t" 'toggle-truncate-lines
   "T" 'org-babel-tangle
   "RET" 'vterm-toggle
   "<C-return>" 'vterm 
   "b" 'persp-counsel-switch-buffer
   "a" 'org-agenda
   "g" 'pdf-view-goto-page
   "H" 'split-window-horizontally
   "V" 'split-window-vertically
   "C" '(calc-dispatch :which-key "Open the M-x calc menu")
   "R" 'recover-this-file
   "m" 'magit
   "B" 'ivy-bibtex
   "r" '(:ignore t :which-key "Org Roam/Ref commands")
   "j" '(:ignore t :which-key "Daily notes")
   "h" 'counsel-imenu
   "c" '(:ignore t :which-key "Calendar Commands")
   "l" '(linum-mode :which-key "Line numbers")
   "i" 'ielm
   "s" '(:ignore t :which-key "Counsel-spotify commands")
   "e" '(:ignore t :which-key "Evaluate Emacs-Lisp")
   "f" '(ace-window :which-key "Switch focus")
   "w" '(:ignore t :which-key "Motion commands")
   "W" 'wolfram-alpha)

 (general-define-key
  :states 'normal
  :keymaps 'override
  :prefix "SPC c"
  "b" 'cfw:open-calendar-buffer
  "o" '(cfw:open-org-calendar :which-key "Open calendar with scheduled to-dos")
  "g" '(cfw:git-open-calendar :which-key "Open calendar with git commit history"))

 (general-define-key
  :states 'normal
  :keymaps 'override
  :prefix "SPC e"
  "b" 'eval-buffer
  "e" 'eval-expression
  "f" 'eval-defun)

 (general-define-key
  :states 'normal
  :keymaps 'override
  :prefix "SPC d"
  "f" 'counsel-find-file
  "j" '(dired-jump :which-key "Open dired in the current buffer's directory")
  "d" 'deft
  "o" 'mediator-open-file)

 (general-define-key
  :states 'normal
  :keymaps 'override
  :prefix "SPC q"
  "c" '((lambda() (interactive)(find-file "~/.emacs.d/README.org")) :which-key "Literate Emacs config")
  "k" '((lambda() (interactive)(find-file "~/.emacs.d/libs/keybindings.org")) :which-key "Emacs keybindings config file")
  "u" '((lambda() (interactive)(dired "~/Documents/4o_εξάμηνο")) :which-key "University folder")
  "q" '((lambda() (interactive)(find-file "~/.config/qtile/README.org")) :which-key "Literate Qtile config")
  "h" '((lambda() (interactive)(dired "~")) :which-key "Home directory")
  "o" '((lambda() (interactive)(dired "~/Documents/Octave")) :which-key "Octave directory")
  "s" '((lambda() (interactive)(dired "~/.emacs.d/snippets/org-mode")) :which-key "Org snippets"))

 (general-define-key
  :states 'normal
  :keymaps 'override
  :prefix "SPC r"
  "f" 'org-roam-node-find
;  "b" 'orb-insert
  "B" 'isbn-to-bibtex
  "a" 'doi-utils-add-bibtex-entry-from-doi
 ; "i" '(org-roam-jump-to-index :which-key "Go to the master index file")
  "G" 'org-roam-graph
  "g" 'counsel-rg
  ;"s" 'org-roam-server-mode
  "c" 'org-roam-capture)

 (general-define-key
  :states 'normal
  :keymaps 'override
  :prefix "SPC j"
  "f" '(:ignore t :which-key "Find daily note")
  "c" '(:ignore t :which-key "Capture daily note")
  "c t" 'org-roam-dailies-capture-today
  "f t" 'org-roam-dailies-find-today
  "c d" 'org-roam-dailies-capture-date
  "f d" 'org-roam-dailies-find-date)

 (general-define-key
  :states 'normal
  :keymaps 'override
  :prefix "SPC s"
  "n" 'counsel-spotify-next
  "p" 'counsel-spotify-previous
  "t" 'counsel-spotify-toggle-play-pause
  "s" '(:ignore t :which-key "Search for")
  "s t" 'counsel-spotify-search-track
  "s p" 'counsel-spotify-search-playlist
  "s a" 'counsel-spotify-search-artist)

 (general-define-key
  :states 'normal
  :keymaps 'override
  :prefix "SPC w"
  "h" 'evil-window-left
  "j" 'evil-window-down
  "k" 'evil-window-up
  "l" 'evil-window-right
  "i" 'evil-window-increase-width
  "I" 'evil-window-increase-height
  "d" 'evil-window-decrease-width
  "D" 'evil-window-decrease-height
  "=" 'balance-windows)

(general-define-key
 :keymaps 'override
 :prefix "C-h"
 "f" 'helpful-callable
 "v" 'helpful-variable
 "k" 'helpful-key)

(general-define-key
 :states 'normal
 :keymaps 'override
 "u" 'undo-tree-undo
 "C-r" 'undo-tree-redo
 "=" 'math-at-point)

(general-define-key
 :states 'insert
 :keymaps 'override
 "<M-tab>" 'tab-jump-out)

(general-define-key
 :states 'visual
 :keymaps 'override
 "e r" 'eval-region
 "m" 'org-marginalia-mark)

(general-define-key
 :states 'motion
 :keymaps 'override
 "j" 'evil-next-visual-line
 "k" 'evil-previous-visual-line)

(general-define-key
 :keymaps 'override
 "M-b" 'ebuku
 "M-C-r" 'restart-emacs
 "M-m" 'man)

(general-create-definer org-leader-def
      :prefix ",")

    (org-leader-def
     :states 'normal
     :keymaps 'org-mode-map
     "l" 'org-latex-preview
     "n" 'org-noter
     "e" 'org-export-dispatch
     "t" '(:ignore t :which-key "To-do management")
     "y" 'org-download-clipboard
     "r" '(:ignore t :which-key "Org Roam/Ref commands")
     "h" '(org-cycle-hide-drawers :which-key "Hide properties drawers")
     "s" 'org-store-link
     "I" 'org-insert-link
     "S" '(org-svg-pdf-export :which-key "Export svg files to pdf")
     "i" 'org-toggle-inline-images
     "p" 'org-tree-slide-mode
     "p" '(org-plot/gnuplot :which-key "Plot table data")
     "f" 'org-footnote-action
     "S" '(lab-skeleton :which-key "Insert my lab report template")
     "T" '(toc-org-mode :which-key "Insert ToC")
     "b" 'org-beamer-select-environment
     "m" '(:ignore t :which-key "Org-Marginalia commands"))

(general-define-key
 :states 'normal
 :keymaps 'org-mode-map
 :prefix ", m"
 "o" 'org-marginalia-open
 "n" 'org-marginalia-next
 "p" 'org-marginalia-previous
 "r" 'org-marginalia-remove
 "t" 'org-marginalia-toggle)

(general-define-key
 :states 'normal
 :keymaps 'org-mode-map
 :prefix ", t"
 "s" 'org-schedule
 "c" 'org-todo
 "p" 'org-priority
 "v" 'org-tags-view
 "t" 'org-set-tags-command)

(general-define-key
 :states 'normal
 :keymaps 'org-mode-map
 :prefix ", r"
 "i" 'org-roam-node-insert
 "c" 'org-ref-ivy-insert-cite-link
 "r" 'org-ref-ivy-insert-ref-link
 "l" 'org-ref-ivy-insert-label-link)

(general-define-key
 :states 'normal
 :keymaps 'org-mode-map
 "`" 'org-roam-buffer-toggle)

(general-define-key
 :states 'normal
 :keymaps 'org-mode-map
 "C-j" '(org-tree-slide-move-next-tree :which-key "Next Slide")
 "C-k" '(org-tree-slide-move-previous-tree :which-key "Previous Slide"))

(general-define-key
 :states 'normal
 :keymaps 'pdf-view-mode-map
 "i" 'org-noter-insert-note
 "c" 'kill-current-buffer
 "a" '(:ignore t :which-key "Add annotation")
 "a t" 'pdf-annot-add-text-annotation
 "a m" 'pdf-annot-add-markup-annotation)

(define-key dired-mode-map (kbd "+") nil)
(general-define-key
 :states 'normal
 :keymaps 'dired-mode-map
 "C-+" 'dired-create-directory
 "+" 'dired-create-empty-file
 "h" 'dired-up-directory
 "l" 'dired-find-file
 "H" 'dired-hide-dotfiles-mode
 "y" 'dired-ranger-copy
 "p" 'dired-ranger-paste
 "g s" 'dired-toggle-sudo
 "s" '(:ignore t :which-key "Dired-subtree functions")
 "s i" 'dired-subtree-insert
 "s r" 'dired-subtree-remove
 "s n" 'dired-subtree-narrow)

(general-define-key
 :states 'normal
 :keymaps 'Info-mode-map
 "J" 'Info-scroll-up
 "K" 'Info-scroll-down)

(provide 'keybindings)

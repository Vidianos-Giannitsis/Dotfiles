(general-create-definer my-leader-def
			:prefix "SPC")

(my-leader-def
 :states 'normal
 :keymaps 'override
  "!" 'shell-command
  "p" 'package-install
  "o" '(inferior-octave :which-key "octave")
  "D" 'dired
  "d" '(:ignore t :which-key "Dired functions")
  "d f" 'counsel-find-file
  "d h" 'dired-hide-dotfiles-mode
  "d j" '(dired-jump :which-key "Open dired in the current buffer's directory")
  "q" '(:ignore t :which-key "Quickmarks")
  "q c" '((lambda() (interactive)(find-file "~/.emacs.d/README.org")) :which-key "Quickmark to literate Emacs config")
  "q k" '((lambda() (interactive)(find-file "~/.emacs.d/libs/keybindings.org")) :which-key "Quickmark to Emacs keybindings config file")
  "q u" '((lambda() (interactive)(dired "~/Documents/4o_εξάμηνο")) :which-key "Quickmark to Uni folder")
  "q q" '((lambda() (interactive)(find-file "~/.config/qtile/README.org")) :which-key "Quickmark to literate Qtile config")
  "q h" '((lambda() (interactive)(dired "~")) :which-key "Quickmark to home directory")
  "q o" '((lambda() (interactive)(dired "~/Documents/Octave")) :which-key "Quickmark to Octave directory")
  "t" 'toggle-truncate-lines
  "T" 'org-babel-tangle
  "RET" 'vterm-toggle
  "<C-return>" 'vterm 
  "b" 'counsel-switch-buffer
  "a" 'org-agenda
  "g" 'pdf-view-goto-page
  "H" 'split-window-horizontally
  "V" 'split-window-vertically
  "C" '(calc-dispatch :which-key "Open the M-x calc menu")
  "w" 'wolfram-alpha
  "R" 'recover-this-file
  "m" 'magit
  "B" 'ivy-bibtex
  "r" '(:ignore t :which-key "Org Roam/Ref commands")
  "r f" 'org-roam-find-file
  "r c" 'orb-insert
  "r b" 'isbn-to-bibtex
  "r a" 'doi-utils-add-bibtex-entry-from-doi
  "j" '(:ignore t :which-key "Daily notes")
  "j f" '(:ignore t :which-key "Find daily note")
  "j c" '(:ignore t :which-key "Capture daily note")
  "j c t" 'org-roam-dailies-capture-today
  "j f t" 'org-roam-dailies-find-today
  "j c d" 'org-roam-dailies-capture-date
  "j f d" 'org-roam-dailies-find-date
  "h" 'counsel-imenu
  "c" '(:ignore t :which-key "Calendar Commands")
  "c b" 'cfw:open-calendar-buffer
  "c o" '(cfw:open-org-calendar :which-key "Open calendar with scheduled to-dos")
  "c g" '(cfw:git-open-calendar :which-key "Open calendar with git commit history")
  "r i" '(org-roam-jump-to-index :which-key "Go to the master index file")
  "l" '(linum-mode :which-key "Line numbers")
  "i" 'ielm
  "s" '(:ignore t :which-key "Counsel-spotify commands")
  "s n" 'counsel-spotify-next
  "s p" 'counsel-spotify-previous
  "s t" 'counsel-spotify-toggle-play-pause
  "s s" '(:ignore t :which-key "Search for")
  "s s t" 'counsel-spotify-search-track
  "s s p" 'counsel-spotify-search-playlist
  "s s a" 'counsel-spotify-search-artist
  "e" '(:ignore t :which-key "Evaluate Emacs-Lisp")
  "e b" 'eval-buffer
  "e e" 'eval-expression
  "e f" 'eval-defun)

(general-create-definer org-leader-def
      :prefix ",")

    (org-leader-def
     :states 'normal
     :keymaps 'org-mode-map
     "l" 'org-latex-preview
     "n" 'org-noter
     "e" 'org-export-dispatch
     "t" '(:ignore t :which-key "To-do management")
     "t s" 'org-schedule
     "t c" 'org-todo
     "t m" '(org-make-todo :which-key "Initialise to-do item")
     "t p" 'org-priority
     "t v" 'org-tags-view
     "t t" 'org-set-tags-command
     "y" 'org-download-clipboard
     "r" '(:ignore t :which-key "Org Roam/Ref commands")
     "r i" 'org-roam-insert
     "r c" 'org-ref-ivy-insert-cite-link
     "r r" 'org-ref-ivy-insert-ref-link
     "r l" 'org-ref-ivy-insert-label-link
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
     "b" 'org-beamer-select-environment)

(general-define-key
 :states 'normal
 :keymaps 'org-mode-map
 "`" 'org-roam)

(general-define-key
 :states 'normal
 :keymaps 'pdf-view-mode-map
 "i" 'org-noter-insert-note
 "c" 'kill-current-buffer
 "a" '(:ignore t :which-key "Add annotation")
 "a t" 'pdf-annot-add-text-annotation
 "a m" 'pdf-annot-add-markup-annotation)

  (global-set-key (kbd "M-b") 'ebuku)
  (global-set-key (kbd "M-C-r") 'restart-emacs)
  (global-set-key (kbd "M-m") 'man)

(general-define-key
 :states 'normal
 :keymaps 'dired-mode-map
 "C-+" 'dired-create-empty-file
 "h" 'dired-up-directory
 "l" 'dired-find-file)

(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)

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
 "e r" 'eval-region)

(general-define-key
 :states 'normal
 :keymaps 'org-mode-map
 "C-j" '(org-tree-slide-move-next-tree :which-key "Next Slide")
 "C-k" '(org-tree-slide-move-previous-tree :which-key "Previous Slide"))

(provide 'keybindings)

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
  "q" '(my/quickmarks-hydra/body :which-key "Quickmarks")
  "t" '(:ignore t :which-key "Move between vterms")
  "T" 'org-babel-tangle
  "RET" 'vterm-toggle
  "<C-return>" 'named-vterm 
  "b" 'counsel-switch-buffer
  "a" 'org-agenda
  "h" 'split-window-horizontally
  "v" 'split-window-vertically
  "c" '(calc-dispatch :which-key "Open the calc dispatcher")
  "R" '(:ignore t :which-key "REPLs")
  "m" 'magit
  "B" 'ivy-bibtex
  "r" '(my/roam-ref-hydra/body :which-key "Org Roam/Ref")
  "j" '(:ignore t :which-key "Daily notes")
  "H" 'counsel-imenu
  "C" '(:ignore t :which-key "Calendar Commands")
  "l" '(:ignore t :which-key "Org Links")
  "L" '(linum-mode :which-key "Line numbers")
  "s" '(:ignore t :which-key "Counsel-spotify")
  "e" '(:ignore t :which-key "Evaluate Emacs-Lisp")
  "f" '(ace-window :which-key "Switch focus")
  "φ" '(ace-window :which-key "Switch focus")
  "w" '(:ignore t :which-key "Motion commands")
  "W" 'wolfram-alpha
  "n" 'winner-redo
  "p" 'winner-undo
  "i" '(my/org-pandoc-hydra/body :which-key "Org Pandoc Import")
  "y" 'ivy-yasnippet
  "z" '(:ignore t :which-key "Zetteldesk")
  "g" 'counsel-rg
  "E" 'eperiodic)

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix "SPC l"
 "o" 'org-open-at-point
 "n" 'org-next-link
 "p" 'org-previous-link)

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix "SPC R"
 "o" '(inferior-octave :which-key "Octave")
 "p" '(run-python :which-key "Python")
 "j" '(ein:jupyter-server-start :which-key "Jupyter Notebook")
 "g" '(run-gnuplot :which-key "Gnuplot")
 "e" '(ielm :which-key "Emacs Lisp"))

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix "SPC C"
 "b" 'cfw:open-calendar-buffer
 "o" '(cfw:open-org-calendar :which-key "Open calendar with scheduled to-dos")
 "g" '(cfw:git-open-calendar :which-key "Open calendar with git commit history"))

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix "SPC e"
 "b" 'eval-buffer
 "e" 'eval-expression
 "f" 'eval-defun
 "s" 'eval-last-sexp)

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix "SPC z"
 "b" 'zetteldesk-switch-to-buffer
 "a" 'zetteldesk-add-to-desktop
 "A" 'zetteldesk-add-node-to-desktop
 "r" 'zetteldesk-remove-from-desktop
 "R" 'zetteldesk-remove-node-from-desktop
 "n" 'zetteldesk-node-find
 "S" 'zetteldesk-create-scratch-buffer
 "s" 'zetteldesk-switch-to-scratch-buffer
 "i" 'zetteldesk-insert-node-contents)

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix "SPC d"
 "f" 'counsel-find-file
 "j" '(dired-jump :which-key "Open dired in the current buffer's directory")
 "d" 'deft
 "o" 'mediator-open-file)

(pretty-hydra-define my/quickmarks-hydra (:color blue :title "Quickmarks")
  ("Emacs Stuff"
   (("c" (lambda() (interactive)(find-file "~/.emacs.d/README.org")) "Literate Emacs Config")
    ("k" (lambda() (interactive)(find-file "~/.emacs.d/libs/keybindings.org")) "Emacs keybindings")
    ("s" (lambda() (interactive)(dired "~/.emacs.d/snippets/org-mode")) "Org-mode snippets")
    ("r" (lambda() (interactive)(find-file "~/.emacs.d/libs/zettelkasten.org")) "Org-Roam and friends")
    ("S" (lambda() (interactive)(find-file "~/scratchpad.org")) "Emacs Scratchpad file"))

   "University"
   (("u" (lambda() (interactive)(dired "~/Documents/5o_εξάμηνο")) "University Documents folder")
    ("B" (lambda() (interactive)(find-file "~/Sync/My_Library.bib")) "Master Bibliography file")
    ("e" (lambda() (interactive)(find-file "~/org_roam/erasmus-08-10-21.org")) "Erasmus index file")
    ("o" (lambda() (interactive)(dired "~/Documents/Octave")) "Octave scripts directory"))

   "General Computer Things"
   (("h" (lambda() (interactive)(dired "~")) "Home directory")
    ("q" (lambda() (interactive)(find-file "~/.config/qtile/README.org")) "Literate Qtile config")
    ("w" (lambda() (interactive)(find-file "~/startpage/script/var.js")) "Web Start page source")
    ("b" (lambda() (interactive)(dired "~/Books")) "Books Directory"))
   ))

(pretty-hydra-define my/roam-ref-hydra (:color blue :title "Org Roam and Org Ref")
  ("Note Specific Commands"
   (("f" org-roam-node-find "org-roam-node-find")
    ("i" (lambda () (interactive)(find-file "~/org_roam/index.org")) "Master index file for org_roam")
    ("n" ivy-bibtex-with-notes "Find Bibliography Note")
    ("p" org-roam-find-permanent-node "Find Permanent Note")
    ("B" org-roam-node-find-by-backlinks "Sort by Backlink Count")
    ("t" org-roam-node-find-todos "Find Fleeting Note"))

   "References"
   (("I" isbn-to-bibtex "Get ref from isbn")
    ("d" doi-utils-add-bibtex-entry-from-doi "Get ref from DOI")
    ("r" org-roam-ref-find "Find Reference")
    ("c" org-ref-insert-cite-link "Insert Citation")
    ("h" org-ref-insert-link-hydra/body "Org Ref Insert-Link Hydra")
    ("b" org-ref-bibtex-hydra/body "Org Ref Bibtex Hydra"))

   "General Org Roam Commands"
   (("G" org-roam-ui-mode "Open the Org Roam UI")
    ("s" org-roam-db-sync "Sync the Org Roam db")
    ("g" counsel-rg "Search regex in the org-roam db")
    ("C" (lambda() (interactive) (org-roam-capture nil "d")) "Capture with default template")
    ("D" org-roam-buffer-display-dedicated "Dedicated Org Roam buffer"))
   )
  )

(general-define-key
 :states 'normal
 :keymaps 'override
 :prefix "SPC j"
 "c" 'org-journal-new-entry
 "n" 'org-journal-next-entry
 "p" 'org-journal-previous-entry
 "s" 'org-journal-search
 "t" 'org-journal-open-current-journal-file
 "d" 'org-journal-new-date-entry
 "S" 'org-journal-new-scheduled-entry)

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
 :states 'normal
 :keymaps 'override
 :prefix "SPC t"
 "n" 'vterm-toggle-forward
 "N" 'vterm-toggle-backward)

(pretty-hydra-define my/org-pandoc-hydra (:color blue :title "Import files to Org")
  ("Import to org file"
   (("i" org-pandoc-import-to-org "General Import")
    ("c" org-pandoc-import-csv-to-org "Import CSV")
    ("d" org-pandoc-import-docx-to-org "Import DOCX")
    ("o" org-pandoc-import-odt-to-org "Import ODT")
    ("l" org-pandoc-import-latex-to-org "Import Latex"))

   "Import to temporary org buffer"
   (("I" org-pandoc-import-as-org "General Import")
    ("C" org-pandoc-import-csv-as-org "Import CSV")
    ("D" org-pandoc-import-docx-as-org "Import DOCX")
    ("O" org-pandoc-import-odt-as-org "Import ODT")
    ("L" org-pandoc-import-latex-as-org "Import Latex"))))

(general-define-key
 :keymaps 'override
 :prefix "C-h"
 "f" 'helpful-callable
 "v" 'helpful-variable
 "k" 'helpful-key)

(general-define-key
 :states 'normal
 :keymaps 'global
 "u" 'undo-tree-undo
 "C-r" 'undo-tree-redo
 "/" 'swiper
 "M-g" 'toggle-input-method
 "M-SPC" 'org-mark-ring-goto
 "<menu>" 'elfeed
 "M-r" 'counsel-linux-app
 "C-γ" 'keyboard-quit
 "θ" 'undo-tree-undo
 "C-ρ" 'undo-tree-redo
 "ο" 'evil-open-below
 "M-λ" 'org-metaright
 "Μ-η" 'org-metaleft)

(general-define-key
 :states 'insert
 :keymaps 'override
 "<M-tab>" 'tab-jump-out
 "M-g" 'toggle-input-method)

(general-define-key
 :states 'visual
 :keymaps 'global
 "e r" 'eval-region
 "m" 'org-marginalia-mark)

(general-define-key
 :states 'motion
 :keymaps 'override
 "j" 'evil-next-visual-line
 "k" 'evil-previous-visual-line
 "ξ" 'evil-next-visual-line
 "κ" 'evil-previous-visual-line
 "η" 'evil-backward-char
 "λ" 'evil-forward-char)

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
     "λ" 'org-latex-preview
     "n" 'org-noter
     "e" 'org-export-dispatch
     "t" '(:ignore t :which-key "To-do management")
     "y" 'org-download-clipboard
     "r" '(:ignore t :which-key "Org Roam/Ref commands")
     "ρ ι" 'org-roam-node-insert
     "H" '(org-cycle-hide-drawers :which-key "Hide properties drawers")
     "S" 'org-store-link
     "I" 'org-insert-link
     "s" '(:ignore t :which-key "SVG commands/Inkscape")
     "i" 'org-toggle-inline-images
     "P" 'org-tree-slide-mode
     "p" '(org-plot/gnuplot :which-key "Plot table data")
     "f" 'org-footnote-action
     "L" '(lab-skeleton :which-key "Insert my lab report template")
     "h" 'hw-skeleton
     "u" '(uo-lab-skeleton :which-key "Unit Operations lab template")
     "T" '(toc-org-mode :which-key "Insert ToC")
     "b" 'org-beamer-select-environment
     "z" '(:ignore t :which-key "Zetteldesk")
     "m" '(:ignore t :which-key "Org-Marginalia commands"))

(general-define-key
 :states 'normal
 :keymaps 'org-mode-map
 :prefix ", s"
 "i" 'insert-svg
 "l" 'svglatex
 "p" 'org-svg-pdf-export)

(general-define-key
 :states 'normal
 :keymaps 'org-mode-map
 :prefix ", z"
 "i" 'zetteldesk-node-insert
 "r" 'zetteldesk-remove-backlinks-from-desktop
 "b" 'zetteldesk-add-backlinks-to-desktop
 "p" 'zetteldesk-node-insert-if-poi)

(general-define-key
 :states 'normal
 :keymaps 'org-marginalia-mode-map
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
 "t" 'org-todo
 "p" 'org-priority
 "v" 'org-tags-view
 "T" 'org-set-tags-command
 "e" 'org-set-effort
 "d" 'org-deadline)

(general-define-key
 :states 'normal
 :keymaps 'org-mode-map
 :prefix ", r"
 "i" 'org-roam-node-insert
 "a" 'org-roam-alias-add
 "f" 'org-roam-init-fleeting-note
 "I" 'org-id-get-create
 "d" 'org-id-delete-entry
 "r" 'org-roam-ref-add
 "b" 'org-roam-backlink-files)

(general-define-key
 :states 'normal
 :keymaps 'org-mode-map
 "C-j" '(org-tree-slide-move-next-tree :which-key "Next Slide")
 "C-k" '(org-tree-slide-move-previous-tree :which-key "Previous Slide")
 "`" 'org-roam-buffer-without-latex
 "=" 'math-at-point
 "α" 'evil-append
 "ι" 'evil-insert)

(general-define-key
 :states 'normal
 :keymaps 'pdf-view-mode-map
 "i" 'org-noter-insert-note
 "c" 'kill-current-buffer
 "a" '(:ignore t :which-key "Add annotation")
 "a t" 'pdf-annot-add-text-annotation
 "a m" 'pdf-annot-add-markup-annotation
 ", g" 'pdf-view-goto-page)

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
 "s n" 'dired-subtree-narrow
 "<C-return>" 'helm-dired-open)

(define-key calc-mode-map (kbd "/") nil)
(general-define-key
 :states 'normal
 :keymaps 'calc-mode-map
 "/" 'calc-divide)

(general-define-key
 :states 'normal
 :keymaps 'Info-mode-map
 ", c" '((lambda() (interactive) (org-roam-capture nil "i")) :which-key "org-roam-capture info-template")
 ", h" 'Info-history
 ", f" 'Info-history-forward
 ", b" 'Info-history-back)

(general-define-key
 :states 'normal
 :keymaps 'ebib-index-mode-map
 "/" 'ebib-jump-to-entry)

(provide 'keybindings)

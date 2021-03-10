(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(let ((default-directory  "~/.emacs.d/libs/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  (auto-package-update-delete-old-versions t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(gcmh-mode 1)

(load-theme 'doom-solarized-dark t)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(setq ring-bell-function 'ignore)

(which-key-mode 1)

(setq inhibit-startup-screen t)
(add-hook 'after-init-hook 'dired-jump)

(ivy-mode 1)
(all-the-icons-ivy-setup)
(global-set-key (kbd "M-x") #'counsel-M-x)

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(require 'dash-functional)
(require 'helpful)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config (setq doom-modeline-minor-modes nil
		doom-modeline-enable-word-count t))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq org-hide-emphasis-markers t)

(setq evil-collection-setup-minibuffer t)
(setq evil-want-keybinding nil)
(when (require 'evil-collection nil t)
  (evil-collection-init))

(unless (package-installed-p 'evil)
  (package-install 'evil))

(require 'evil)
(evil-mode 1)

(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

(add-to-list 'load-path
	     "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

(require 'general)
(require 'vterm-toggle)

(setq large-file-warning-threshold nil)
(setq vc-follow-symlinks t)

(set-face-attribute 'org-document-title nil :font "Source Code Pro" :weight 'bold :height 1.3)
    (dolist (face '((org-level-1 . 1.2)
		    (org-level-2 . 1.1)
		    (org-level-3 . 1.05)
		    (org-level-4 . 1.0)
		    (org-level-5 . 1.1)
		    (org-level-6 . 1.1)
		    (org-level-7 . 1.1)
		    (org-level-8 . 1.1)))
      (set-face-attribute (car face) nil :font "Source Code Pro" :weight 'regular :height (cdr face)))

(defun org-toggle-emphasis ()
  "Toggle hiding/showing of org emphasize markers."
  (interactive)
  (if org-hide-emphasis-markers
      (set-variable 'org-hide-emphasis-markers nil)
    (set-variable 'org-hide-emphasis-markers t)))

(setq counsel-spotify-client-id "0df2796a793b41dc91711eb9f85c0e77")
(setq counsel-spotify-client-secret "bcdbb823795640248ff2c29eedadb800")

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
  "r" '(:ignore t :which-key "Org-Roam commands")
  "r f" 'org-roam-find-file
  "r b" 'orb-insert
  "j" '(:ignore t :which-key "Daily notes")
  "j f" '(:ignore t :which-key "Find daily note")
  "j c" '(:ignore t :which-key "Capture daily note")
  "j c t" 'org-roam-dailies-capture-today
  "j f t" 'org-roam-dailies-find-today
  "j c d" 'org-roam-dailies-capture-date
  "j f d" 'org-roam-dailies-find-date
  "h" '(counsel-imenu :which-key "Jump to heading")
  "c" '(:ignore t :which-key "Calendar Commands")
  "c b" 'cfw:open-calendar-buffer
  "c o" '(cfw:open-org-calendar :which-key "Open calendar with scheduled to-dos")
  "c g" '(cfw:git-open-calendar :which-key "Open calendar with git commit history")
  "r i" '(org-roam-jump-to-index :which-key "Go to the master index file")
  "l" '(linum-mode :which-key "Line numbers")
  "e" 'ielm
  "s" '(:ignore t :which-key "Counsel-spotify commands")
  "s n" 'counsel-spotify-next
  "s p" 'counsel-spotify-previous
  "s t" 'counsel-spotify-toggle-play-pause
  "s s" '(:ignore t :which-key "Search for")
  "s s t" 'counsel-spotify-search-track
  "s s p" 'counsel-spotify-search-playlist
  "s s a" 'counsel-spotify-search-artist)

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
     "t m" '(ad/org-make-todo :which-key "Initialise to-do item")
     "t p" 'org-priority
     "t v" 'org-tags-view
     "t t" 'org-set-tags-command
     "y" 'org-download-clipboard
     "r" '(:ignore t :which-key "Org-Roam commands")
     "r i" 'org-roam-insert
     "h" '(org-cycle-hide-drawers :which-key "Hide properties drawers")
     "s" 'org-store-link
     "I" 'org-insert-link
     "S" '(ad/org-svg-pdf-export :which-key "Export svg files to pdf")
     "i" 'org-toggle-inline-images
     "p" 'org-tree-slide-mode
     "j" '(org-tree-slide-move-next-tree :which-key "Next Slide")
     "k" '(org-tree-slide-move-previous-tree :which-key "Previous Slide")
     "p" '(org-plot/gnuplot :which-key "Plot table data")
     "f" 'org-footnote-action)

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
  (global-set-key (kbd "M-d") (lambda() (interactive)(find-file "~/.emacs.d/README.org")))
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
 "C-r" 'undo-tree-redo)

(require 'dired-x)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfile
  :hook (dired-mode . dired-hide-dotfiles-mode))

(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode))

(setq all-the-icons-dired-monochrome nil)

(show-paren-mode 1)
(electric-pair-mode 1)
(setq wolfram-alpha-app-id "U9PERG-KTPL49AWA2")
(global-undo-tree-mode 1)

(use-package magit-todos-mode
  :hook magit-mode)
(require 'calfw-git)
(require 'calfw-org)

(use-package openwith
  :config
  (setq openwith-associations
	(list
	 (list (openwith-make-extension-regexp
		'("mpg" "mpeg" "mp3" "mp4"
		  "avi" "wmv" "wav" "mov" "flv"
		  "ogm" "ogg" "mkv"))
		"mpv"
		'(file))
(list (openwith-make-extension-regexp
		'("xbm" "pbm" "pgm" "ppm" "pnm"
		  "gif" "bmp" "tif"))
		  "sxiv"
		  '(file))
	 (list (openwith-make-extension-regexp
		'("docx" "doc" "xlsx" "xls" "ppt" "odt" "ods"))
	       "libreoffice"
	       '(file))))
	(openwith-mode 1))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("M-p" . projectile-command-map)))

(add-hook 'org-mode-hook #'(lambda ()
			       (org-superstar-mode)
			       (org-superstar-configure-like-org-bullets)))

(use-package org-download
  :after org)

(require 'calctex)
(add-hook 'calc-embedded-new-formula-hook 'calctex-mode)

(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

(require 'org-tree-slide)

(add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))

(setq org-format-latex-options '(:foreground default :background default :scale 1.3 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers))

(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

(setq org-odt-preferred-output-format "docx")

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'(lambda ()
								   (let ((org-confirm-babel-evaluate nil))
								     (org-babel-tangle))))
					      'run-at-end 'only-in-org-mode))

(add-hook 'org-mode-hook '(lambda ()
			    (toggle-truncate-lines)
			    (org-latex-preview)
			    (org-toggle-inline-images)))

(setq org-noter-always-create-frame nil)

(defun org-cycle-hide-drawers (state)
  "Hide all the :PROPERTIES: drawers when called with the 'all argument. Mainly for hiding them in crammed org-noter files"
  (interactive "MEnter 'all for hiding :PROPERTIES: drawers in an org buffer: ")
  (when (and (derived-mode-p 'org-mode)
	     (not (memq state '(overview folded contents))))
    (save-excursion
      (let* ((globalp (memq state '(contents all)))
	     (beg (if globalp
		    (point-min)
		    (point)))
	     (end (if globalp
		    (point-max)
		    (if (eq state 'children)
		      (save-excursion
			(outline-next-heading)
			(point))
		      (org-end-of-subtree t)))))
	(goto-char beg)
	(while (re-search-forward org-drawer-regexp end t)
	  (save-excursion
	    (beginning-of-line 1)
	    (when (looking-at org-drawer-regexp)
	      (let* ((start (1- (match-beginning 0)))
		     (limit
		       (save-excursion
			 (outline-next-heading)
			   (point)))
		     (msg (format
			    (concat
			      "org-cycle-hide-drawers:  "
			      "`:END:`"
			      " line missing at position %s")
			    (1+ start))))
		(if (re-search-forward "^[ \t]*:END:" limit t)
		  (outline-flag-region start (point-at-eol) t)
		  (user-error msg))))))))))

(org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (python . t)
     (haskell . t)
     (octave . t)
     (latex . t)
     (gnuplot . t)
)
   )

(setq org-todo-keywords
	'((sequence "TODO(t)"
		    "ACTIVE(a)"
		    "NEXT(n)"
		    "WAIT(w)"
		    "|"
		    "DONE(d@)"
		    "CANCELLED(c@)"
		    )))

  (setq org-agenda-files
	  '("~/org_roam"))

(defun ad/org-make-todo ()
  "Set todo keyword, priority, effort and tags for a todo item. This is very useful for initialising todo items"
  (interactive)
  (org-todo)
  (org-priority)
  (org-set-effort)
  (org-set-tags-command))

(org-super-agenda-mode 1)

(add-hook 'org-agenda-mode-hook 'toggle-truncate-lines)

(setq org-agenda-custom-commands
      '(("q" "Quick Check for the day"
	 ((agenda "" ((org-agenda-span 'day)
		      (org-super-agenda-groups
		       '((:name "Today"
				:time-grid t
				:date today
				:scheduled today)))))
	 (alltodo "" ((org-agenda-overriding-header "")
		       (org-super-agenda-groups
			'((:name "What I've been doing"
				 :todo "ACTIVE")
			  (:name "Plans for the foreseeable future"
				 :todo "NEXT")
			  (:name "You GOTTA check this one out"
				 :priority "A")
			  (:name "As easy as they get"
				 :effort< "0:10")
			  (:discard (:anything))))))))
	("u" "University Projects"
	 ((alltodo "" ((org-agenda-overriding-header "")
		       (org-super-agenda-groups
			'((:name "Currently Working on"
				 :and (:tag "University" :todo "ACTIVE"))
			  (:name "This one's next (probably)"
				 :and (:priority "A" :tag "University"))
			  (:name "Medium Priority Projects"
				 :and (:tag "University" :priority "B"))
			  (:name "Trivial Projects, I'ma do them at some point though :D"
				 :and (:tag "University" :priority "C"))
			  (:discard (:not (:tag "University")))))))))
	("e" "Emacs Projects"
	 ((alltodo "" ((org-agenda-overriding-header "")
		       (org-super-agenda-groups
			'((:name "Configuring Emacs, the Present"
				 :and (:tag "Emacs" :todo "ACTIVE")
				 :and (:tag "Emacs" :todo "NEXT"))
			  (:name "What to add, What to add??"
				 :and (:tag "Emacs" :priority "A"))
			  (:name "Wow, this one's easy, lets do it"
				 :and (:tag "Emacs" :effort< "0:15"))
			  (:discard (:not (:tag "Emacs")))
			  (:name "But wait, this was only the beginning. The real fun starts here!"
				 :anything)))))))))

(setq org-roam-directory "~/org_roam")

(add-hook 'after-init-hook 'org-roam-mode)
(add-hook 'after-init-hook 'org-roam-bibtex-mode)

(setq bibtex-completion-bibliography
      '("~/org_roam/Zotero_library.bib"))
(setq reftex-default-bibliography '("~/org_roam/Zotero_library.bib"))

(setq bibtex-completion-additional-search-fields '(keywords abstract))

(setq orb-preformat-keywords
      '("citekey" "title" "author" "keywords" "abstract" "entry-type" "file")
      orb-process-file-keyword t
      orb-file-field-extensions '("pdf"))

(setq org-roam-dailies-directory "~/org_roam/daily")

(setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)

(ivy-add-actions
 'ivy-bibtex
 '(("p" ivy-bibtex-open-any "Open pdf, url or DOI")))

(setq orb-templates
      '(("r" "ref" plain (function org-roam-capture--get-point)
	 ""
	 :file-name "${citekey}"
	 :head "#+TITLE: ${title}\nglatex\n#+ROAM_KEY: ${ref}

* Ref Info
:PROPERTIES:
:Custom_ID: ${citekey}
:AUTHOR: ${author}
:NOTER_DOCUMENT: ${file} ;
:END:
#+BEGIN_abstract\n${abstract}\n#+END_abstract

- tags ::
- keywords :: ${keywords}")))

(setq org-roam-capture-templates
      '(("d" "default" plain (function org-roam-capture--get-point)
	"%?"
	:file-name "%<%d-%m-%Y %H:%M>-${slug}"
	:unnarrowed t
	:head "#+title: ${title}\nglatex\n
- tags ::  ")))

(setq org-roam-dailies-capture-templates
      '(("l" "lesson" entry
	 #'org-roam-capture--get-point
	 "* %?"
	 :file-name "daily/%<%Y-%m-%d>"
	 :head "#+title: Fleeting notes for %<%Y-%m-%d>\n"
	 :olp ("Lesson notes"))

	("b" "bibliography" entry
	 #'org-roam-capture--get-point
	 "* %?"
	 :file-name "daily/%<%Y-%m-%d>"
	 :head "#+title: Fleeting notes for %<%Y-%m-%d>\n"
	 :olp ("Notes on Articles, Books, etc."))

	("g" "general" entry
	 #'org-roam-capture--get-point
	 "* %?"
	 :file-name "daily/%<%Y-%m-%d>"
	 :head "#+title: Fleeting notes for %<%Y-%m-%d>\n"
	 :olp ("Random general notes"))

	("w" "workout" entry
	 #'org-roam-capture--get-point
	 "* %?"
	 :file-name "daily/%<%Y-%m-%d>"
	 :head "#+title: Fleeting notes for %<%Y-%m-%d>\n"
	 :olp ("Workout Regimes"))))

(defun ad/org-inkscape-img ()
    (interactive "P")
    (setq string (read-from-minibuffer "Insert image name: "))
    ;; if images folder not exists create it
    (setq dirname (concat (f-base (buffer-file-name)) "-org-img"))
    (if (not (file-directory-p dirname))
	(make-directory dirname))
     ;; if file doesn't exist create it
     (if (not (file-exists-p (concat "./" dirname "/" string ".svg")))
     (progn
	 (setq command (concat "echo " "'<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><svg xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:cc=\"http://creativecommons.org/ns#\" xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:svg=\"http://www.w3.org/2000/svg\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:sodipodi=\"http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd\" xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\" width=\"164.13576mm\" height=\"65.105995mm\" viewBox=\"0 0 164.13576 65.105995\" version=\"1.1\" id=\"svg8\" inkscape:version=\"1.0.2 (e86c8708, 2021-01-15)\" sodipodi:docname=\"disegno.svg\"> <defs id=\"defs2\" /> <sodipodi:namedview id=\"base\" pagecolor=\"#ffffff\" bordercolor=\"#666666\" borderopacity=\"1.0\" inkscape:zoom=\"1.2541194\" inkscape:cx=\"310.17781\" inkscape:cy=\"123.03495\"z inkscape:window-width=\"1440\" inkscape:window-height=\"847\" inkscape:window-x=\"1665\" inkscape:window-y=\"131\" inkscape:window-maximized=\"1\"  inkscape:current-layer=\"svg8\" /><g/></svg>' >> " dirname "/" string ".svg; inkscape " dirname "/" string ".svg"))
	    (shell-command command)
	    (concat "#+begin_export latex\n\\begin{figure}\n\\centering\n\\def\\svgwidth{0.9\\columnwidth}\n\\import{" "./" dirname "/}{" string ".pdf_tex" "}\n\\end{figure}\n#+end_export"))
	;; if file exists opens it
	(progn
	    (setq command (concat "inkscape " dirname "/" string ".svg"))
	    (shell-command command)
	    (concat "" ""))))

(add-to-list 'org-latex-packages-alist '("" "booktabs"))
(add-to-list 'org-latex-packages-alist '("" "import"))

(defun ad/org-svg-pdf-export ()
  (interactive)
  (setq dirname (concat (f-base (buffer-file-name)) "-org-img"))
  (if (file-directory-p dirname)
      (progn
	(setq command (concat "/usr/bin/inkscape -D --export-latex --export-type=\"pdf\" " dirname "/" "*.svg"))
	(shell-command command))))

;(add-to-list 'org-export-before-processing-hook #'org-svg-pdf-export)

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'company-mode-hook '(lambda ()
				(add-to-list 'company-backends 'company-math-symbols-latex)
				(setq company-math-allow-latex-symbols-in-faces t)
				(add-to-list 'company-backends 'company-bibtex)
				(setq company-bibtex-bibliography '("~/org_roam/Zotero_library.bib"))
				(setq company-minimum-prefix-length 2)))

(require 'ebuku)
(require 'evil-collection-ebuku)

(add-hook 'ebuku-mode-hook 'evil-collection-ebuku-setup)

(use-package pdf-tools
    :mode (("\\.pdf\\'" . pdf-view-mode))
    :config
    ;(define-key pdf-view-mode-map [remap quit-window] #'kill-current-buffer)
    (progn
      (pdf-tools-install))
    )

(add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)

(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(require 'eaf)

(require 'eaf-evil)

(setq eaf-wm-focus-fix-wms '("qtile"))

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'ielm-mode-hook 'eldoc-mode)

;; CUSTOM VARIABLES
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" default))
 '(package-selected-packages
   '(evil-collection openwith sequences cl-lib-highlight helm-system-packages async-await popup-complete helm-fuzzy-find evil-space yapfify yaml-mode ws-butler winum which-key web-mode web-beautify vterm volatile-highlights vi-tilde-fringe uuidgen use-package toc-org tagedit spaceline solarized-theme slim-mode scss-mode sass-mode restart-emacs request rainbow-delimiters pyvenv pytest pyenv-mode py-isort pug-mode pspp-mode popwin pip-requirements persp-mode pcre2el paradox org-projectile-helm org-present org-pomodoro org-mime org-download org-bullets open-junk-file neotree move-text mmm-mode markdown-toc magit macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint json-mode js2-refactor js-doc intero indent-guide hy-mode hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-hoogle helm-flx helm-descbinds helm-css-scss helm-ag haskell-snippets gruvbox-theme google-translate golden-ratio gnuplot gh-md flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu emmet-mode elisp-slime-nav dumb-jump diminish define-word cython-mode csv-mode company-ghci company-ghc column-enforce-mode coffee-mode cmm-mode clean-aindent-mode auto-highlight-symbol auto-compile auctex-latexmk anaconda-mode aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

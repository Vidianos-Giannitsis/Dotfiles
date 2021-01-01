(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'package-archives (package-initialize))
(unless package-archive-contents
  (package-refresh-contents))

(let ((default-directory  "~/.emacs.d/libs/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(load-theme 'solarized-dark t)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(setq ring-bell-function 'ignore)

(which-key-mode 1)

(setq inhibit-startup-screen t)
(add-hook 'after-init-hook 'dired-jump)

(global-set-key (kbd "M-x") 'helm-M-x)

(ido-mode 1)

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

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

(show-paren-mode 1)
(electric-pair-mode 1)

(add-to-list 'load-path
	     "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

(require 'general)
(require 'vterm-toggle)

(add-hook 'ibuffer-mode-hook 'all-the-icons-ibuffer-mode)

(setq truncate-partial-width-windows nil)

(setq wolfram-alpha-app-id "U9PERG-KTPL49AWA2")

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'org-roam)

(use-package org-download
  :after org)

(require 'calctex)
(add-hook 'calc-embedded-new-formula-hook 'calctex-mode)

(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
(add-hook 'org-mode-hook 'org-zotxt-mode)

(add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
(setq org-noter-always-create-frame nil)

(setq org-format-latex-options '(:foreground default :background default :scale 1.3 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers))

(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

(setq org-odt-preferred-output-format "docx")

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
	  '("~/project_management"))

(defun org-make-todo ()
  (interactive)
  (org-todo)
  (org-priority)
  (org-set-effort)
  (org-set-tags-command))

(setq org-agenda-start-with-log-mode t)
(setq org-log-into-drawer t)

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

(org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (python . t)
     (haskell . t)
     (octave . t)
     (latex . t)
)
   )

(require 'dired-x)
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))


(use-package dired-hide-dotfile
  :hook (dired-mode . dired-hide-dotfiles-mode))

(require 'eaf)

(require 'eaf-evil)
;(setq eaf-evil-leader-key "SPC")

(setq eaf-wm-focus-fix-wms '("qtile"))

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

(general-create-definer my-leader-def
			:prefix "SPC")

(my-leader-def
 :states 'normal
 :keymaps 'override
  "!" 'shell-command
  "p" 'package-install
  "o" 'inferior-octave
  "d" 'dired
  "h" 'dired-hide-dotfiles-mode
  "t" 'toggle-truncate-lines
  "j" 'dired-jump
  "T" 'org-babel-tangle
  "RET" 'vterm-toggle
  "<C-return>" 'vterm 
  "b" 'ibuffer
  "a" 'org-agenda
  "g" 'pdf-view-goto-page
  "H" 'split-window-horizontally
  "V" 'split-window-vertically
  "c" 'calc-dispatch
  "w" 'wolfram-alpha)

(general-create-definer org-leader-def
      :prefix ",")

    (org-leader-def
     :states 'normal
     :keymaps 'org-mode-map
     "l" 'org-latex-preview
     "n" 'org-noter
     "s" 'org-schedule
     "t" 'org-todo
     "m" 'org-make-todo
     "e" 'org-export-dispatch
     "p" 'org-priority
     "v" 'org-tags-view
     "T" 'org-set-tags-command
     "y" 'org-download-clipboard
     "z i" 'org-zotxt-insert-reference-link
     "z o" 'org-zotxt-open-attachment
     "z n" 'org-zotxt-noter)

(general-define-key
 :states 'normal
 :keymaps 'pdf-view-mode-map
 "i" 'org-noter-insert-note
 "c" 'kill-current-buffer)

  (global-set-key (kbd "M-b") 'ebuku)
  (global-set-key (kbd "M-C-r") 'restart-emacs)
  (global-set-key (kbd "M-d") (lambda() (interactive)(find-file "~/.emacs.d/README.org")))
  (global-set-key (kbd "M-t") (lambda() (interactive)(find-file "~/project_management/emacs.org")))

(add-hook 'dired-mode-hook
	  (lambda () (local-set-key (kbd "C-+") #'dired-create-empty-file)))

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

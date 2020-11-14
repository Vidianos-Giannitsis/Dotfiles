(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(add-to-list 'load-path "~/.emacs.d/libs/")

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

(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(show-paren-mode 1)
(electric-pair-mode 1)

(add-to-list 'load-path
	     "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

(unless (package-installed-p 'evil)
  (package-install 'evil))

(require 'evil-leader)
(global-evil-leader-mode)
(require 'evil)
(evil-mode 1)

(use-package pdf-tools
    :mode (("\\.pdf\\'" . pdf-view-mode))
    :config
    (progn
      (pdf-tools-install))
    :hook
    (pdf-view-mode . (lambda () (local-set-key (kbd "J") #'pdf-view-next-line-or-next-page)))
    (pdf-view-mode . (lambda () (local-set-key (kbd "K") #'pdf-view-previous-line-or-previous-page)))
    (pdf-view-mode . (lambda () (local-set-key (kbd "k") #'pdf-view-scroll-down-or-previous-page)))
    (pdf-view-mode . (lambda () (local-set-key (kbd "j") #'pdf-view-scroll-up-or-next-page)))
    (pdf-view-mode . (lambda () (local-set-key (kbd "h") #'pdf-view-previous-page)))
    (pdf-view-mode . (lambda () (local-set-key (kbd "l") #'pdf-view-next-page)))
    (pdf-view-mode . (lambda () (local-set-key (kbd "m") #'pdf-view-midnight-minor-mode)))
    )

(add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)

(org-babel-do-load-languages
     'org-babel-load-languages
     '(
       (python . t)
       (haskell . t)
       (octave . t)
       (latex . t)
  )
     )

  ;;(require 'literate-calc-mode)

  (require 'org-bullets)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

  (add-hook 'org-mode-hook
	    (lambda () (local-set-key (kbd "M-h") nil)))

  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
  (setq org-noter-always-create-frame nil)

(setq org-todo-keywords
      '((sequence "TODO"
		  "IMPORTANT"
		  "MAYBE"
		  "ON HOLD"
		  "STARTED"
		  "|"
		  "CANCELLED"
		  "DONE"
		  )))

(require 'dired-x)
(add-hook 'dired-mode-hook
	  (lambda () (local-set-key (kbd "M-RET") #'dired-display-file)))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))


(use-package dired-hide-dotfile
  :hook (dired-mode . dired-hide-dotfiles-mode))

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "f" 'helm-find-files
  "<SPC>" 'helm-M-x
  "!" 'shell-command
  "p" 'package-install
  "r" 'helm-recentf
  "o" 'inferior-octave
  "j" 'dired-jump
  "d" 'dired
  "h" 'dired-hide-dotfiles-mode
  "t" 'toggle-truncate-lines
  "T" 'org-babel-tangle
  "RET" 'vterm
  "b" 'switch-to-buffer
  "n" 'org-noter)

(global-set-key (kbd "M-h") 'split-window-horizontally)
(global-set-key (kbd "M-v") 'split-window-vertically)

(global-set-key (kbd "M-C-r") 'restart-emacs)
(global-set-key (kbd "M-d") (lambda() (interactive)(find-file "~/.emacs.d/README.org")))
(global-set-key (kbd "M-t") (lambda() (interactive)(find-file "~/todo.org")))

(global-set-key (kbd "M-m") 'which-key-show-major-mode)

;; CUSTOM VARIABLES
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" default))
 '(package-selected-packages
   '(markdown-mode csv haskell-mode evil-collection openwith sequences cl-lib-highlight helm-system-packages async-await popup-complete helm-fuzzy-find evil-space yapfify yaml-mode ws-butler winum which-key web-mode web-beautify vterm volatile-highlights vi-tilde-fringe uuidgen use-package toc-org tagedit spaceline solarized-theme slim-mode scss-mode sass-mode restart-emacs request rainbow-delimiters pyvenv pytest pyenv-mode py-isort pug-mode pspp-mode popwin pip-requirements persp-mode pcre2el paradox org-projectile-helm org-present org-pomodoro org-mime org-download org-bullets open-junk-file neotree move-text mmm-mode markdown-toc magit macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint json-mode js2-refactor js-doc intero indent-guide hy-mode hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-hoogle helm-flx helm-descbinds helm-css-scss helm-ag haskell-snippets gruvbox-theme google-translate golden-ratio gnuplot gh-md flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu emmet-mode elisp-slime-nav dumb-jump diminish define-word cython-mode csv-mode company-ghci company-ghc column-enforce-mode coffee-mode cmm-mode clean-aindent-mode auto-highlight-symbol auto-compile auctex-latexmk anaconda-mode aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line))
 '(truncate-lines nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
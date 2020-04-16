;;; package -- Matt's Emacs configuration
;;; Commentary:
;;; Code:

;;; User info

(setq user-full-name "Matthew Wittmann"
      user-mail-address "mcwitt@gmail.com")

;;; File paths

;; Path to additional elisp files
(add-to-list 'load-path "~/.emacs.d/elisp/")

;; Relocate custom file
(setq custom-file "~/.emacs.d/emacs-custom.el")

;; Create backup files in system temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; UI

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; Default font
(add-to-list 'default-frame-alist
             `(font . ,(if (string-equal system-type "darwin")
                           "Fira Code-13"
                         "FiraCode")))

(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 80))
(setq split-height-threshold 100)

;; Prompt for y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Editing

(setq-default indent-tabs-mode nil) ; Don't use tabs
(show-paren-mode 1)                 ; Show matching parens

(defun increment-number-at-point ()
  "Increment integer at the cursor position."
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(global-set-key (kbd "C-c +") 'increment-number-at-point)

;;; Package settings

(require 'package)

;; Make impure packages archives unavailable (packages are managed by Nix)
(setq package-archives nil)

(setq package-enable-at-startup nil)
(package-initialize)

(eval-when-compile
  (require 'use-package))

;;; Packages

;; Try to utilize use-package wherever possible

(use-package imenu
  :bind ("C-c i" . imenu))

;; Helper for keybindings
(use-package general)

;; Vim emulation
(use-package evil
  :general
  (:states '(normal insert)
           :keymaps 'comint-mode-map
           "C-j" 'comint-next-input
           "C-k" 'comint-previous-input)
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  :config
  (evil-mode 1))

;; Bind key combination to ESC
(use-package evil-escape
  :init
  (setq-default evil-escape-key-sequence "fd")
  :config
  (evil-escape-mode 1))

;; Themes
(use-package doom-themes
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-org-config))

(use-package zenburn-theme
  :config (load-theme 'zenburn t))

(use-package all-the-icons)

(use-package ivy
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-c C-r" . ivy-resume)
         (:map ivy-minibuffer-map
               ("C-j" . ivy-next-line)
               ("C-k" . ivy-previous-line)
               ("C-h" . ivy-backward-kill-word))
         (:map ivy-switch-buffer-map
               ("C-j" . ivy-next-line)
               ("C-k" . ivy-previous-line)))
  :config
  (ivy-mode 1))

;; Counsel
(use-package counsel
  :bind (("C-s" . swiper)
         ("M-x" . counsel-M-x)
         ("C-x l" . counsel-locate)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-r" . counsel-recentf)
         ("C-c t" . counsel-load-theme)))

;; Integration with tramp-mode (remote file editing)
(use-package counsel-tramp
  :after tramp
  :bind ("C-c f" . counsel-tramp))

;; Display key binding hints in minibuffer
(use-package which-key
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

;; Project tools
(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (setq projectile-require-project-root nil)
  (setq projectile-completion-system 'ivy)
  (setq projectile-project-search-path '("~/src/"))
  (require 'subr-x)  ;; work around bug similar to https://github.com/alphapapa/org-protocol-capture-html/issues/7
  :config
  (projectile-mode 1))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode 1))

;; Find files with content matching regex
(use-package ripgrep)

;; Completion
(use-package company
  :bind
  (:map company-active-map
        ("C-j" . company-select-next)
        ("C-k" . company-select-previous)
        ("jk" . company-complete))
  :hook (after-init . global-company-mode)
  :config
  (company-mode 1))

;; direnv integration
;; (updates environment based on local .envrc)
(use-package direnv
  :config (direnv-mode))

;; Deal with parens in pairs
(use-package smartparens)

(use-package evil-smartparens
  :after smartparens
  :hook (smartparens-enabled . evil-smartparens-mode))

;; Surround text objects with parens, brackets, quotes, etc.
(use-package evil-surround
  :config (global-evil-surround-mode 1))

;; Relative line numbering
(use-package nlinum-relative
  :config (nlinum-relative-setup-evil)
  :hook (prog-mode . nlinum-relative-mode))

;; Fira Code ligatures
(use-package fira-code-mode
  :hook haskell-mode)

;; highlighting TODO items in comments
(use-package hl-todo
  :config (global-hl-todo-mode 1))

;; Tools for working with git
(use-package magit
  :bind ("C-x g" . magit-status))

(use-package evil-magit
  :after (evil magit))

;; Browse/edit remote files via ssh and ftp
(use-package browse-at-remote
  :bind ("C-c b" . browse-at-remote))

;; Tree view
(use-package treemacs
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    ;; (treemacs-resize-icons 44)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind (:map global-map
              ("M-0"       . treemacs-select-window)
              ("C-x t 1"   . treemacs-delete-other-windows)
              ("C-x t t"   . treemacs)
              ("C-x t B"   . treemacs-bookmark)
              ("C-x t C-t" . treemacs-find-file)
              ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil)

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit)

;; Syntax checking
(use-package flycheck
  :init (setq ispell-program-name "aspell")
  :config (global-flycheck-mode t))

;; Language Server Protocol support
(use-package lsp-mode
  :commands lsp
  :hook (haskell-mode . lsp)
  :init (setq lsp-keymap-prefix "C-c l")
  :config
  (setq gc-cons-threshold 100000000))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package company-lsp
  :commands company-lsp)

(use-package lsp-ivy
  :commands (lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol))

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

;; TeX
(use-package tex
  :config (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools")))

;; Minor mode for editing LaTeX inside of org documents
(use-package cdlatex
  :hook (org-mode . turn-on-org-cdlatex))

;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

;; Haskell
(use-package haskell-mode
  :hook (haskell-mode . interactive-haskell-mode)
  :bind (:map haskell-mode-map ("C-c C-h" . 'haskell-hoogle-lookup-from-local))
  :init (setq haskell-interactive-popup-errors nil))

(use-package lsp-haskell
  :config
  (setq lsp-haskell-process-path-hie "ghcide")
  (setq lsp-haskell-process-args-hie '())

  ;; HACK: won't always want hlint chained after lsp, but ok for now
  (flycheck-add-next-checker 'lsp '(warning . haskell-hlint))

  ;; Comment/uncomment this line to see interactions between lsp client/server.
  ;;(setq lsp-log-io t)
  )

;; Jupyter (REPL, org-babel integration)
(use-package jupyter
  :bind ("C-c j" . jupyter-run-repl)
  :general
  (:keymaps 'jupyter-repl-mode-map
            :states '(normal insert)
            "C-j" 'jupyter-repl-history-next-matching
            "C-k" 'jupyter-repl-history-previous-matching))

;; Python
(use-package anaconda-mode
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)))

(use-package company-anaconda
  :after company anaconda-mode
  :config (add-to-list 'company-backends 'company-anaconda))

(use-package pyenv-mode
  :hook python-mode)

;; json
(use-package json-mode
  :hook (json-mode . flycheck-mode))

;; yaml
(use-package yaml-mode)

;; Major mode for viewing log files
(use-package logview)

;; Send HTTP requests
(use-package restclient)

(use-package company-restclient
  :after restclient)

(use-package ob-restclient
  :after org-babel restclient)

;; pinentry
(use-package pinentry
  :config (pinentry-start))

;; Gist export
(use-package gist
  :bind ("C-c g" . gist-region-or-buffer-private))

;; Snippet tool
(use-package yasnippet
  :config (yas-global-mode 1))

(use-package yasnippet-snippets)

;; Org (environment for outlining, todos, literate programming)
(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c o l" . org-store-link)
         ("C-c o s" . mcw:save-and-sync-org-notes)
         ("C-c o b" . mcw:display-bookmarks-in-side-window)
         (:map org-mode-map
               ("C-c C-c" .
                (lambda ()
                  (interactive)
                  (org-ctrl-c-ctrl-c)
                  (org-display-inline-images)))))
  :commands mcw:save-and-sync-org-notes
  :hook ((org-mode . turn-on-flyspell)
         (org-mode . mcw:sync-org-notes))
  :init
  (defvar mcw:org-notes-directory
    (file-name-as-directory "~/src/org-notes/"))
  (defvar mcw:org-notes-inbox-file
    (concat mcw:org-notes-directory "inbox.org"))
  (defvar mcw:org-notes-gtd-file
    (concat mcw:org-notes-directory "gtd.org"))
  (defvar mcw:org-notes-someday-file
    (concat mcw:org-notes-directory "someday.org"))
  (defvar mcw:org-notes-bookmarks-file
    (concat mcw:org-notes-directory "bookmarks.org"))
  (defvar mcw:org-notes-journal-file
    (concat mcw:org-notes-directory "journal.org"))
  (defvar mcw:org-notes-notes-directory
    (concat mcw:org-notes-directory (file-name-as-directory "notes")))

  (defun mcw:sync-org-notes ()
    "Sync org notes repo with upstream."
    (let ((default-directory mcw:org-notes-directory))
      (shell-command "git-sync")))

  (defun mcw:display-bookmarks-in-side-window ()
    "Display org-notes bookmarks file in a side window."
    (interactive)
    (select-window
     (display-buffer-in-side-window
      (find-file-noselect mcw:org-notes-bookmarks-file) nil)))

  (setq org-startup-indented t)
  (setq org-todo-keywords '((sequence "TODO" "NEXT" "BLOCKED" "REVIEW" "|" "DONE")))
  (setq org-tag-persistent-alist '(("PROJECT" . ?P) (:startgroup) ("@home" . ?h) ("@work" . ?w)))
  (setq org-capture-templates
        '(("t" "Todo" entry
           (file mcw:org-notes-inbox-file)
           "* TODO %?\n%U\n")
          ("b" "Bookmark" entry
           (file+headline mcw:org-notes-bookmarks-file "Bookmarks")
           "* [[%^{url}][%?]]\n%U\n")
          ("j" "Journal" entry
           (file+datetree mcw:org-notes-journal-file)
           "* %?\nEntered on %U\n  %i\n  %a")))
  (setq org-stuck-projects
        '("LEVEL=2+PROJECT-TODO=DONE"
          ("NEXT")
          nil
          ""))
  (setq org-tags-sort-function #'string<)
  (setq org-confirm-babel-evaluate nil)

  :config
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
  (add-to-list 'org-file-apps-gnu '(t . "xdg-open %s")) ;; use xdg-open as default (replaces mailcap)
  (add-to-list 'org-modules 'org-habit)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (emacs-lisp . t)
     (haskell . t)
     (jupyter . t)
     (latex . t)
     (R . t)
     (restclient . t)
     (shell . t)))

  ;; Export settings
  (require 'ox-md)     ;; enable Markdown export
  (require 'ox-beamer) ;; enable Beamer presentation export
  (with-eval-after-load 'ox-latex
    (setq org-latex-listings 'minted)
    (setq org-latex-pdf-process '("latexmk -f -pdf -shell-escape -output-directory=%o %f"))
    (add-to-list 'org-latex-packages-alist '("newfloat" "minted")))

  (defun mcw:save-and-sync-org-notes ()
    "Save all org buffers and sync gtd repo."
    (interactive)
    (org-save-all-org-buffers)
    (mcw:sync-org-notes)))

(use-package org-agenda
  :bind (:map org-agenda-mode-map
              ;; minimal set of evil movements in org-agenda
              ("j" . evil-next-line)
              ("k" . evil-previous-line)
              ("C-u" . evil-scroll-page-up)
              ("C-d" . evil-scroll-page-down)
              ("C-w h" . evil-window-left)
              ("C-w l" . evil-window-right))
  :init
  (setq org-agenda-files
        (list mcw:org-notes-inbox-file
              mcw:org-notes-gtd-file
              mcw:org-notes-journal-file))
  (setq org-agenda-custom-commands
        '(("i" "Inbox" alltodo "" ((org-agenda-files `(,mcw:org-notes-inbox-file))))
          ("p" "Projects" tags "LEVEL=2+PROJECT")
          ("n" "Next tasks"  tags-todo "TODO=\"NEXT\"")))
  (setq org-refile-targets '((mcw:org-notes-gtd-file . (:maxlevel . 2))
                             (mcw:org-notes-someday-file . (:maxlevel . 2))
                             (mcw:org-notes-bookmarks-file . (:maxlevel . 1))))
  (setq org-enforce-todo-dependencies t))

;; enable Github-flavored Markdown export
(use-package ox-gfm)

;; Note-taking
(use-package deft
  :after org
  :bind ("C-c d" . deft)
  :init
  (setq deft-extension "org")
  (setq deft-directory mcw:org-notes-notes-directory))

;; fast viewing and searching for PDF files
(use-package pdf-tools
  :mode  ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . (lambda () (linum-mode -1)))
  :bind (:map pdf-view-mode-map
              ("j" . pdf-view-next-line-or-next-page)
              ("k" . pdf-view-previous-line-or-previous-page))
  :config
  (pdf-tools-install)
  (setq pdf-view-use-scaling t)
  (setq-default pdf-view-display-size 'fit-page))

;; tools for notes and annotations linked to PDFs
;; (use-package interleave)
(use-package org-noter
  :after pdf-tools
  :bind (:map pdf-view-mode-map ("C-c C-n" . org-noter))
  :init
  (defvar mcw:library-notes-directory "~/src/library/notes/")
  :config
  (add-to-list 'org-noter-notes-search-path mcw:org-notes-notes-directory)
  (add-to-list 'org-noter-notes-search-path mcw:library-notes-directory))

;; Nix
(use-package nix-mode)

;; Proof General
(use-package proof-general
  :bind (:map proof-mode-map
              ("C-c C-'" . proof-goto-point) ; useful for Kinesis keyboard
              ))

;; ERC (IRC client)
(use-package erc
  :bind ("C-c e" . mcw:erc-freenode)
  :commands mcw:erc-freenode
  :init
  (setq erc-prompt-for-password nil) ; get login from ~/.authinfo.gpg
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  (setq erc-autojoin-channels-alist
        '(("#emacs"
           "#haskell"
           "#nixos"
           "#org-mode"
           "#python"
           "freenode.net")))
  (setq erc-autojoin-timing 'ident)

  :config
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling)
  (erc-update-modules)

  (defun mcw:erc-freenode ()
    "Connect to freenode with ERC."
    (interactive)
    (erc :server "irc.freenode.net" :port 6667 :nick "mcwitt")))

(use-package erc-hl-nicks
  :after erc)

(use-package erc-image
  :after erc)

(use-package format-all
  :bind ("C-c C-f" . format-all-buffer))

(use-package esup
  :commands (esup))

(use-package ess)

(use-package org-evil
  :hook (org-mode . org-evil-mode))

(provide 'init)
;;; init.el ends here

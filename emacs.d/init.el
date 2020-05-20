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

(setq-default indent-tabs-mode nil) ; Don't insert tabs for indentation
(show-paren-mode 1)                 ; Highlight matching parens
(column-number-mode)                ; Display column numbers

;; Highlight end-of-line whitespace only in prog-mode
(add-hook 'prog-mode-hook (lambda () (setq-local show-trailing-whitespace t)))

;;;; Packages

;;; Configuration tools
(require 'package)

;; Make impure packages archives unavailable (packages are managed by Nix)
(setq package-archives nil)

(setq package-enable-at-startup nil)
(package-initialize)

(eval-when-compile
  (require 'use-package))

;; Helper for keybindings
(use-package general)


;;; Global modes

(use-package undo-tree
  :delight
  :init
  (undo-tree-mode))

(use-package imenu
  :bind ("C-c i" . imenu))

;; Display key binding hints in minibuffer
(use-package which-key
  :delight
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

;; Deal with parens in pairs
(use-package smartparens)

;; Visually identify matching parens
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Relative line numbering
(use-package nlinum-relative
  :config (nlinum-relative-setup-evil)
  :hook (prog-mode . nlinum-relative-mode))


;;; Evil (vim emulation)

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

;; Collection of vim-like bindings for major modes
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Bind key combination to ESC
(use-package evil-escape
  :delight
  :init
  (setq-default evil-escape-key-sequence "fd")
  :config
  (evil-escape-mode 1))

(use-package evil-smartparens
  :after smartparens
  :hook (smartparens-enabled . evil-smartparens-mode))

;; Surround text objects with parens, brackets, quotes, etc.
(use-package evil-surround
  :config (global-evil-surround-mode 1))


;;; Themes

(use-package doom-themes
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-org-config))

(use-package zenburn-theme
  :config (load-theme 'zenburn t))

(use-package all-the-icons)


;;; Ivy (command completion framework)

(use-package ivy
  :delight
  :bind
  (("C-c C-r" . ivy-resume)
   (:map ivy-minibuffer-map
         ("C-w" . ivy-backward-kill-word)))
  :config
  (ivy-mode 1))

;; Ivy-enhanced Emacs commands
(use-package counsel
  :delight
  :bind ("C-c t" . counsel-load-theme)
  :config (counsel-mode 1))

;; Ivy-enhanced version of isearch
(use-package swiper
  :bind ("C-s" . swiper))

;; Integration with tramp-mode (remote file editing)
(use-package counsel-tramp
  :after tramp
  :bind ("C-c f" . counsel-tramp))


;;; Project tools

(use-package projectile
  :delight
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (setq projectile-require-project-root nil)
  (setq projectile-completion-system 'ivy)
  (setq projectile-project-search-path '("~/src/"))
  (require 'subr-x)  ; work around bug similar to https://github.com/alphapapa/org-protocol-capture-html/issues/7
  :config
  (projectile-mode 1))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode 1))

;; direnv integration
;; updates environment based on local .envrc
(use-package direnv
  :config (direnv-mode))

;; Find files with content matching regex
(use-package ripgrep)

;; Highlight TODO items in comments
(use-package hl-todo
  :config (global-hl-todo-mode 1))


;;; Tree view

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
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit))


;;; Code completion

(use-package company
  :delight
  :bind (:map company-active-map ("jk" . company-complete))
  :hook (after-init . global-company-mode))


;;; Code formatting

(use-package format-all
  :bind ("C-c C-f" . format-all-buffer))

(use-package reformatter
  :config
  (reformatter-define stylish-cabal
    :program "stylish-cabal"
    :lighter " SC"))


;;; Git

(use-package magit
  :demand
  :bind (("C-x g" . magit-status)
         (:map magit-file-mode-map
               ("C-c g" . magit-file-dispatch)))
  :config
  (global-magit-file-mode 1))

(use-package evil-magit
  :after (evil magit))

;; Gist export
(use-package gist)

;; Browse/edit remote files via ssh and ftp
(use-package browse-at-remote
  :bind ("C-c b" . browse-at-remote))


;;; Syntax checking

(use-package flycheck
  :init (setq ispell-program-name "aspell")
  :config (global-flycheck-mode t))


;;; Language Server Protocol support

(use-package lsp-mode
  :commands lsp
  :hook ((lsp-mode . lsp-lens-mode)
         (haskell-mode . lsp)
         (scala-mode . lsp))
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
  :commands lsp-treemacs-errors-list
  :config
  ;; Scala-specific
  (lsp-metals-treeview-enable t)
  (setq lsp-metals-treeview-show-when-views-received t))


;;; TeX

(use-package tex
  :config (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools")))


;;; Markdown

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))


;;; Haskell

(use-package haskell-mode
  :hook (haskell-mode . interactive-haskell-mode)
  :bind ((:map haskell-mode-map ("C-c C-h" . 'haskell-hoogle-lookup-from-local))
         (:map haskell-cabal-mode-map ("C-c C-f" . stylish-cabal-buffer)))
  :init (setq haskell-interactive-popup-errors nil))

(use-package flycheck-haskell
  :hook (haskell-mode . flycheck-haskell-setup))

(use-package lsp-haskell
  :config
  (setq lsp-haskell-process-path-hie "ghcide")
  (setq lsp-haskell-process-args-hie '())

  ;; HACK: won't always want hlint chained after lsp, but ok for now
  (flycheck-add-next-checker 'lsp '(warning . haskell-hlint))

  ;; Comment/uncomment this line to see interactions between lsp client/server.
  ;;(setq lsp-log-io t)
  )

;; Haskell code formatter
(use-package ormolu
  :hook (haskell-mode . ormolu-format-on-save-mode)
  :bind
  (:map haskell-mode-map
        ("C-c r" . ormolu-format-buffer)))

;; Fira Code ligatures
(use-package fira-code-mode
  :hook haskell-mode)


;;; Jupyter (REPL, org-babel integration)

(use-package jupyter
  :bind ("C-c j" . jupyter-run-repl)
  :general
  (:keymaps 'jupyter-repl-mode-map
            :states '(normal insert)
            "C-j" 'jupyter-repl-history-next-matching
            "C-k" 'jupyter-repl-history-previous-matching))


;;; Python

(use-package anaconda-mode
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)))

(use-package company-anaconda
  :after (company anaconda-mode)
  :config (add-to-list 'company-backends 'company-anaconda))

(use-package pyenv-mode
  :hook python-mode)


;;; Config files

(use-package json-mode
  :hook (json-mode . flycheck-mode))

(use-package yaml-mode)

(use-package nix-mode)


;;; REST tools

(use-package restclient)

(use-package company-restclient
  :after (company restclient))

(use-package ob-restclient
  :after (org-babel restclient))

(use-package logview)


;;; Snippets

(use-package yasnippet
  :config (yas-global-mode 1))

(use-package yasnippet-snippets)


;;; Org-mode

(use-package org
  :bind (("C-c c" . org-capture)
         ("C-c n b" . mcw:display-bookmarks-in-side-window)
         ("C-c n j" . mcw:org-notes-open-journal)
         ("C-c n k" . org-store-link)
         ("C-c n s" . mcw:save-and-sync-org-notes)
         (:map org-mode-map
               ("C-c C-c" .
                (lambda ()
                  (interactive)
                  (org-ctrl-c-ctrl-c)
                  (org-display-inline-images)))))
  :hook ((org-mode . visual-line-mode)
         (org-mode . turn-on-flyspell)
         (after-save . mcw:maybe-sync-org-notes))
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
  (defvar mcw:org-notes-references-directory
    (concat mcw:org-notes-directory (file-name-as-directory "references")))

  (defun mcw:maybe-sync-org-notes ()
    "Sync org notes if the current buffer is visiting an org file in \
the org-notes directory."
    (when (and (derived-mode-p 'org-mode)
               (string-prefix-p (expand-file-name mcw:org-notes-directory)
                                (buffer-file-name)))
      (mcw:sync-org-notes)))

  (require 'git-sync)
  (defun mcw:sync-org-notes ()
    "Sync org notes repo with upstream."
    (interactive)
    (let ((default-directory mcw:org-notes-directory))
      (git-sync)))

  (defun mcw:save-and-sync-org-notes ()
    "Save all org buffers and sync gtd repo."
    (interactive)
    (org-save-all-org-buffers)
    (mcw:sync-org-notes))

  (defun mcw:display-bookmarks-in-side-window ()
    "Display org-notes bookmarks file in a side window."
    (interactive)
    (select-window
     (display-buffer-in-side-window
      (find-file-noselect mcw:org-notes-bookmarks-file) nil)))

  (defun mcw:org-notes-open-journal ()
    "Open org-notes journal file."
    (interactive)
    (find-file mcw:org-notes-journal-file))

  (setq org-hide-emphasis-markers t)
  (setq org-startup-indented t)
  (setq org-tags-column 0) ; don't try to align tags
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
           (file+olp+datetree mcw:org-notes-journal-file)
           "* %?\nEntered on %U\n  %i\n  %a")))
  (setq org-stuck-projects
        '("LEVEL=2+PROJECT-TODO=DONE"
          ("NEXT")
          nil
          ""))
  (setq org-tags-sort-function #'string<)
  (setq org-confirm-babel-evaluate nil)
  (setq org-log-into-drawer t)

  :config
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
  (add-to-list 'org-file-apps-gnu '(t . "xdg-open %s")) ; use xdg-open as default (replaces mailcap)
  (add-to-list 'org-modules 'org-habit)

  ;; Display centered dots for list bullets
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

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
  (require 'ox-md)     ; enable Markdown export
  (require 'ox-beamer) ; enable Beamer presentation export
  (with-eval-after-load 'ox-latex
    (setq org-latex-listings 'minted)
    (setq org-latex-pdf-process '("latexmk -shell-escape -bibtex -f -pdf -output-directory=%o %f"))
    (add-to-list 'org-latex-packages-alist '("newfloat" "minted"))))

(use-package org-agenda
  :bind (("C-c a" . org-agenda)
         (:map org-agenda-mode-map
               ;; minimal set of evil movements in org-agenda
               ("j" . evil-next-line)
               ("k" . evil-previous-line)
               ("C-u" . evil-scroll-page-up)
               ("C-d" . evil-scroll-page-down)
               ("C-w h" . evil-window-left)
               ("C-w l" . evil-window-right)))
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

;; Vim-like bindings for org-mode
(use-package org-evil
  :hook (org-mode . org-evil-mode))

;; Minor mode for editing LaTeX inside of org documents
(use-package cdlatex
  :hook (org-mode . turn-on-org-cdlatex))

;; Unicode bullets for headlines in org-mode
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; Mix fixed- and variable-pitch fonts (e.g. in org-mode)
(use-package mixed-pitch
  :hook (text-mode . mixed-pitch-mode))

;; Fix for list indentation with variable pitch in org-mode
(use-package org-variable-pitch
  :init (setq org-variable-pitch-fixed-faces nil)
  :hook (org-mode . org-variable-pitch-minor-mode))

;; enable Github-flavored Markdown export
(use-package ox-gfm)

;; Note-taking
(use-package deft
  :after org
  :bind ("C-c n d" . deft)
  :custom
  (deft-auto-save-interval 0) ; disable auto-save
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory mcw:org-notes-notes-directory))

;; fast viewing and searching for PDF files
(use-package pdf-tools
  :mode  ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . (lambda () (linum-mode -1)))
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

(use-package org-roam
  :delight
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory mcw:org-notes-notes-directory)
  :bind ((:map org-roam-mode-map
               (("C-c n l" . org-roam)
                ("C-c n f" . org-roam-find-file)
                ("C-c n g" . org-roam-graph)))
         (:map org-mode-map
               (("C-c n i" . org-roam-insert))))
  :config
  (require 'org-roam-protocol))

(use-package company-org-roam
  :after company
  :config (add-to-list 'company-backends 'company-org-roam))

;; Screenshot and yank images (e.g. from the web) into Org documents
(use-package org-download
  :after org
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))

(use-package org-ref
  :init
  (setq reftex-default-bibliography
        (list (concat mcw:org-notes-references-directory "master.bib")))

  (setq org-ref-bibliography-notes
        (concat mcw:org-notes-references-directory "notes.org"))
  (setq org-ref-default-bibliography
        (list (concat mcw:org-notes-references-directory "master.bib")))
  (setq org-ref-pdf-directory
        (concat mcw:org-notes-references-directory
                (file-name-as-directory "bibtex-pdfs")))

  :config
  (setq org-ref-completion-library 'org-ref-ivy-cite))

(use-package ivy-bibtex
  :init
  (setq bibtex-completion-bibliography
        (concat mcw:org-notes-references-directory "master.bib"))
  (setq bibtex-completion-library-path
        (concat mcw:org-notes-references-directory "bibtex-pdfs"))
  (setq bibtex-completion-notes-path
        (concat mcw:org-notes-references-directory "helm-bibtex-notes")))

;; Integration between org-roam, ivy-bibtex, org-ref
(use-package org-roam-bibtex
  :delight
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind (:map org-mode-map (("C-c n a" . orb-note-actions))))

(use-package org-pomodoro
  :ensure t
  :commands (org-pomodoro))


;;; IRC client

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


;;; Scala

;; Highlighting, indentation and motion commands
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

;; Executing sbt commands
(use-package sbt-mode
  :commands (sbt-start sbt-command)
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

;; Posframe is a pop-up tool that must be manually installed for dap-mode
(use-package posframe)

;; Debug Adapter Protocol for running tests and debugging
(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))


;;; Agda

(use-package agda2-mode
  :mode ("\\.l?agda\\'" "\\.lagda.md\\'"))


;;; Misc

(use-package pinentry
  :config (pinentry-start))

;; Startup profiler
(use-package esup
  :commands (esup))

(use-package ess)

(provide 'init)
;;; init.el ends here

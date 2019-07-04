;;; package -- Summary
;;; Commentary:
;;; Code:
;; Path to additional elisp files
(add-to-list 'load-path "~/.emacs.d/elisp/")

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

(add-to-list 'default-frame-alist '(font . "FiraCode"))
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 80))

;; Show matching parens
(show-paren-mode 1)

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

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

;; Light theme (default)
(use-package leuven-theme
  :custom
  (leuven-scale-outline-headlines nil "")
  (leuven-scale-org-agenda-structure nil "")
  :config
  (load-theme 'leuven t))

;; Dark theme
(use-package zenburn-theme)

(use-package ivy
  :bind (("C-x b" . ivy-switch-buffer)
	 ("C-c C-r" . ivy-resume)
	 (:map ivy-minibuffer-map
	       (("C-j" . ivy-next-line)
		("C-k" . ivy-previous-line)
		("C-h" . ivy-backward-kill-word))))
  :config
  (ivy-mode 1))

(use-package counsel
  :bind (("C-s" . swiper)
	 ("M-x" . counsel-M-x)
	 ("C-x l" . counsel-locate)
	 ("C-x C-f" . counsel-find-file)
	 ("C-x C-r" . counsel-recentf)
	 ("C-c g" . counsel-git)
	 ("C-c j" . counsel-git-grep)
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
(require 'fira-code-mode)

;; highlighting TODO items in comments
(use-package hl-todo
  :config (global-hl-todo-mode 1))

;; Tools for working with git
(use-package magit
  :bind ("C-x g" . magit-status))

(use-package evil-magit
  :after magit)

;; browse at remote
(use-package browse-at-remote
  :bind ("C-c b" . browse-at-remote))

;; treemacs
(use-package treemacs
  :defer t
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
  :bind
  (:map global-map
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

;; Org (environment for outlining, todos, literate programming)
(setq mcw:org-gtd-directory (file-name-as-directory "~/org/gtd/"))
(setq mcw:org-gtd-agenda-file (concat mcw:org-gtd-directory "gtd.org"))

(use-package org
  :bind (("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :hook ((org-mode . turn-on-flyspell)
	 (org-capture . org-align-all-tags))
  :init
  (setq org-startup-indented t)
  (setq org-capture-templates
	'(("t" "Todo" entry
	   (file mcw:org-gtd-agenda-file)
	   "* TODO %? :Inbox:\n%U\n")))
  (setq org-stuck-projects
	'("+LEVEL=1/-DONE"              ;; Used to identify a project
	  ("TODO" "NEXT" "NEXTACTION")  ;; If subtree contains any of these states, project is not stuck
	  ("Inbox" "Reading" "Someday") ;; Do not consider projects with any of these tags stuck
	  ""))
  (setq org-confirm-babel-evaluate nil)
  :config
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (haskell . t)
     (jupyter . t)
     (R . t)
     (restclient . t)
     (scala . t)
     (shell . t))))

(use-package org-agenda
  :ensure nil
  :bind
  (:map org-agenda-mode-map
	;; minimal set of evil movements in org-agenda
	("j" . evil-next-line)
	("k" . evil-previous-line)
	("C-u" . evil-scroll-page-up)
	("C-d" . evil-scroll-page-down)
	("C-w h" . evil-window-left)
	("C-w l" . evil-window-right))
  :init
  (setq org-agenda-files (list mcw:org-gtd-agenda-file))
  (setq org-agenda-custom-commands
	'(("n" "Agenda and all TODOs"
	   ((agenda "")
	    (tags "Inbox")
	    (tags "Reading")
	    (alltodo "")))))
  (setq org-enforce-todo-dependencies t))

(defun mcw:save-and-sync-org ()
  (interactive)
  (org-save-all-org-buffers)
  (let ((default-directory mcw:org-gtd-directory))
    (shell-command "git-sync")))

(global-set-key (kbd "C-c o s") 'mcw:save-and-sync-org)

(use-package flycheck
  :config (global-flycheck-mode))

;; Language Server Protocol support
(use-package lsp-mode
  :commands lsp
  :config (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package company-lsp
  :commands company-lsp)

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

;; LaTeX
(use-package tex
  :ensure auctex)

;; Minor mode for editing LaTeX inside of org documents
(use-package cdlatex
  :hook (org-mode . turn-on-org-cdlatex))

(with-eval-after-load 'ox-latex
  (setq org-latex-listings 'minted)
  (setq org-latex-to-pdf-process '("latexmk -f -pdf %f"))
  (add-to-list 'org-latex-packages-alist '("newfloat" "minted")))

;; For exporting org documents as Beamer presentations
(require 'ox-beamer)

;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

;; Haskell
(use-package haskell-mode
  :hook (haskell-mode . fira-code-mode)
  :bind (:map haskell-mode-map ("C-c C-f" . 'mcw:haskell-mode-format-buffer-with-brittany))
  :init (setq haskell-process-type 'stack-ghci))

(use-package intero
  :config
  (flycheck-add-next-checker 'intero '(warning . haskell-hlint))
  (intero-global-mode 1))

(use-package lsp-haskell)

(defun mcw:haskell-mode-format-buffer-with-brittany ()
  (interactive)
  (save-buffer)
  (shell-command-to-string (format "brittany --write-mode inplace %s" buffer-file-name))
  (revert-buffer :ignore-auto :noconfirm))

;; Scala
(use-package scala-mode
  :after smartparens
  :hook ((scala-mode . smartparens-mode)
	 (scala-mode . fira-code-mode))
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package ensime
  :defer
  :bind (:map scala-mode-map
	 ("C-c C-e" . ensime)
	 ("C-c C-f" . mcw:scala-mode-format-buffer-with-sbt-scalafmt)
	 :map ensime-mode-map
	 ("C-c C-v g" . ensime-edit-definition-of-thing-at-point))
  :custom (ensime-startup-notification 'nil))

(defun mcw:scala-mode-format-buffer-with-sbt-scalafmt ()
  (interactive)
  (save-buffer)
  (ensime-sbt-run-command-in-project "scalafmtOnly" t)
  (revert-buffer :ignore-auto :noconfirm))

;; Jupyter (REPL, org-babel integration)
(use-package jupyter
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

(use-package blacken
  :bind (:map python-mode-map ("C-c C-f" . blacken-buffer)))

;; json
(use-package json-mode
  :hook (json-mode . flycheck-mode)
  :bind (:map json-mode-map ("C-c C-f" . json-pretty-print-buffer)))

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

;; Note-taking
(use-package deft
  :bind ("C-c d" . deft)
  :init
  (setq deft-extension "org"))

;; pinentry
(use-package pinentry
  :config (pinentry-start))

;; SQL
(use-package sql
  :hook (sql-interactive-mode . (lambda () (toggle-truncate-lines t)))
  :bind ("C-c s" . mcw:sql-connect)
  :commands sql-connect
  :init
  (defun mcw:sql-connect ()
    (interactive)
    (if (not (boundp 'sql-connections)) (mcw:load-sql-connections) nil)
    (call-interactively 'sql-connect))
  (defun mcw:load-sql-connections ()
    (interactive)
    (require 'sql-connections "~/.sql-connections.el.gpg"))
  )

(use-package sql-indent
  :hook sql-mode)

;; plantuml
(use-package plantuml-mode
  :config
  (setq org-plantuml-jar-path (expand-file-name "~/.local/libexec/plantuml.jar")))

(use-package flycheck-plantuml)

;; S3
(use-package s3ed
  :bind (("C-c r f" . s3ed-find-file)
	 ("C-c r s" . s3ed-save-file)))

;; custom file
(setq custom-file "~/.emacs-custom.el")

(provide 'init)
;;; init.el ends here

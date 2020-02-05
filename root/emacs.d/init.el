;;; package -- Summary
;;; Commentary:
;;; Code:

;;; Path to additional elisp files
(add-to-list 'load-path "~/.emacs.d/elisp/")

;;; Relocate custom file
(setq custom-file "~/.emacs-custom.el")

;;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(add-to-list 'default-frame-alist `(font . ,(if (string-equal system-type "darwin") "Fira Code" "FiraCode")))
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 80))

(setq split-height-threshold 100)

(setq-default indent-tabs-mode nil)

;;; Show matching parens
(show-paren-mode 1)

(defun increment-number-at-point ()
  "Increment integer at the cursor position."
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(global-set-key (kbd "C-c +") 'increment-number-at-point)

(require 'package)

;; Make impure packages archives unavailable
(setq package-archives nil)

(setq package-enable-at-startup nil)
(package-initialize)

(eval-when-compile
  (require 'use-package))

(use-package imenu
  :bind ("C-c i" . imenu))

;;; Helper for keybindings
(use-package general)

;;; Vim emulation
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

;;; Bind key combination to ESC
(use-package evil-escape
  :init
  (setq-default evil-escape-key-sequence "fd")
  :config
  (evil-escape-mode 1))

;;; Dark theme
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(use-package ivy
  :bind (("C-x b" . ivy-switch-buffer)
	 ("C-c C-r" . ivy-resume)
	 (:map ivy-minibuffer-map
	       (("C-j" . ivy-next-line)
		("C-k" . ivy-previous-line)
		("C-h" . ivy-backward-kill-word)))
         (:map ivy-switch-buffer-map
               (("C-j" . ivy-next-line)
                ("C-k" . ivy-previous-line))))
  :config
  (ivy-mode 1))

;;; Counsel
(use-package counsel
  :bind (("C-s" . swiper)
	 ("M-x" . counsel-M-x)
	 ("C-x l" . counsel-locate)
	 ("C-x C-f" . counsel-find-file)
	 ("C-x C-r" . counsel-recentf)
	 ("C-c t" . counsel-load-theme)))

;;; Integration with tramp-mode (remote file editing)
(use-package counsel-tramp
  :after tramp
  :bind ("C-c f" . counsel-tramp))

;;; Display key binding hints in minibuffer
(use-package which-key
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

;;; Project tools
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

;;; Find files with content matching regex
(use-package ripgrep)

;;; Completion
(use-package company
  :bind
  (:map company-active-map
	("C-j" . company-select-next)
	("C-k" . company-select-previous)
	("jk" . company-complete))
  :hook (after-init . global-company-mode)
  :config
  (company-mode 1))

;;; direnv integration
;; (updates environment based on local .envrc)
(use-package direnv
  :config (direnv-mode))

;;; Deal with parens in pairs
(use-package smartparens)

(use-package evil-smartparens
  :after smartparens
  :hook (smartparens-enabled . evil-smartparens-mode))

;;; Surround text objects with parens, brackets, quotes, etc.
(use-package evil-surround
  :config (global-evil-surround-mode 1))

;;; Relative line numbering
(use-package nlinum-relative
  :config (nlinum-relative-setup-evil)
  :hook (prog-mode . nlinum-relative-mode))

;;; Fira Code ligatures
(require 'fira-code-mode)

;;; highlighting TODO items in comments
(use-package hl-todo
  :config (global-hl-todo-mode 1))

;;; Tools for working with git
(use-package magit
  :bind ("C-x g" . magit-status))

(use-package evil-magit
  :after (evil magit))

;;; Browse/edit remote files via ssh and ftp
(use-package browse-at-remote
  :bind ("C-c b" . browse-at-remote))

;;; Tree view
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

;;; Syntax checking
(use-package flycheck
  :config (global-flycheck-mode))

;;; Language Server Protocol support
(use-package lsp-mode
  :commands lsp
  :config (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package company-lsp
  :commands company-lsp)

(use-package lsp-ivy
  :commands (lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol))

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

;;; Minor mode for editing LaTeX inside of org documents
(use-package cdlatex
  :hook (org-mode . turn-on-org-cdlatex))

(with-eval-after-load 'ox-latex
  (setq org-latex-listings 'minted)
  (setq org-latex-to-pdf-process '("latexmk -f -pdf %f"))
  (add-to-list 'org-latex-packages-alist '("newfloat" "minted")))

;;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

;;; Haskell
(use-package haskell-mode
  :hook (haskell-mode . fira-code-mode)
  :bind (:map haskell-mode-map
	 ("C-c C-h" . 'haskell-hoogle)
         ("C-c C-f" . 'mcw:haskell-mode-format-buffer-with-brittany))
  :init (setq haskell-process-type 'stack-ghci))

(use-package intero
  :config
  (flycheck-add-next-checker 'intero '(warning . haskell-hlint))
  (intero-global-mode 1))

(use-package lsp-haskell)

(defun mcw:haskell-mode-format-buffer-with-brittany ()
  (interactive)
  (save-buffer)
  (shell-command (format "stack exec brittany -- --write-mode inplace %s" buffer-file-name))
  (revert-buffer :ignore-auto :noconfirm))

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
  :after company anaconda-mode
  :config (add-to-list 'company-backends 'company-anaconda))

(use-package pyenv-mode
  :hook python-mode)

(use-package blacken
  :bind (:map python-mode-map ("C-c C-f" . blacken-buffer)))

;;; json
(use-package json-mode
  :hook (json-mode . flycheck-mode)
  :bind (:map json-mode-map ("C-c C-f" . json-pretty-print-buffer)))

;;; yaml
(use-package yaml-mode)

;;; Major mode for viewing log files
(use-package logview)

;;; Send HTTP requests
(use-package restclient)

(use-package company-restclient
  :after restclient)

(use-package ob-restclient
  :after org-babel restclient)

;;; Note-taking
(use-package deft
  :bind ("C-c d" . deft)
  :init
  (setq deft-extension "org"))

;;; pinentry
(use-package pinentry
  :config (pinentry-start))

;;; Gist export
(use-package gist
  :bind ("C-c g" . gist-region-or-buffer-private))

;;; Snippet tool
(use-package yasnippet
  :config (yas-global-mode 1))

(use-package yasnippet-snippets)

;;; Org (environment for outlining, todos, literate programming)
(setq mcw:org-notes-directory (file-name-as-directory "~/src/org-notes/"))
(setq mcw:org-notes-gtd-file (concat mcw:org-notes-directory "gtd.org"))
(setq mcw:org-notes-journal-file (concat mcw:org-notes-directory "journal.org"))

(use-package org
  :bind (("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :hook ((org-mode . turn-on-flyspell)
	 (org-capture . org-align-all-tags))
  :init
  (setq org-startup-indented t)
  (setq org-todo-keywords '((sequence "TODO" "NEXT" "BLOCKED" "REVIEW" "|" "DONE")))
  (setq org-tag-persistent-alist
        '((:startgroup) ("@home" . ?h) ("@work" . ?w)
          (:startgroup) ("PROJECT" . ?P) ("INBOX" . ?I) ("MAYBE" . ?M)))
  (setq org-capture-templates
	'(("t" "Todo" entry
	   (file mcw:org-notes-gtd-file)
	   "* TODO %? :INBOX:\n%U\n")
          ("j" "Journal" entry
           (file+datetree mcw:org-notes-journal-file)
           "* %?\nEntered on %U\n  %i\n  %a")))
  (setq org-stuck-projects
	'("LEVEL=1+PROJECT-INBOX-TODO=DONE"
          ("NEXT")
          nil
	  ""))
  (setq org-tags-sort-function #'string<)
  (setq org-confirm-babel-evaluate nil)
  :config
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2))
  (require 'ox-md)     ;; enable Markdown export
  (require 'ox-beamer) ;; enable Beamer presentation export

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (haskell . t)
     (jupyter . t)
     (R . t)
     (restclient . t)
     (shell . t))))

(use-package org-agenda
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
  (setq org-agenda-files (list mcw:org-notes-gtd-file mcw:org-notes-journal-file))
  (setq org-agenda-custom-commands
        '(("i" "Inbox" tags "INBOX")
          ("p" "Projects" tags "LEVEL=1+PROJECT")
          ("n" "Next tasks"  tags-todo "PROJECT+TODO=\"NEXT\"")))
  (setq org-enforce-todo-dependencies t))

;; enable Github-flavored Markdown export
(use-package ox-gfm)

;; fast viewing and searching for PDF files
(use-package pdf-tools
  :init (setq pdf-view-use-scaling t)
  :hook (pdf-view-mode . (lambda () (linum-mode -1)))
  :config (pdf-tools-install))

;; tools for notes and annotations linked to PDFs
;; (use-package interleave)
(use-package org-noter)

;; spaced repetition flash cards
;; TODO disabled until "Lisp nesting exceeds ‘max-lisp-eval-depth" error solved
;; (use-package org-drill)

(defun mcw:save-and-sync-org-notes ()
  "Save all org buffers and sync gtd repo."
  (interactive)
  (org-save-all-org-buffers)
  (let ((default-directory mcw:org-notes-directory))
    (shell-command "git-sync")))

(global-set-key (kbd "C-c o s") 'mcw:save-and-sync-org-notes)

;; Nix
(use-package nix-mode
  :bind (:map nix-mode-map ("C-c C-f" . mcw:nix-mode-format-buffer-with-nixfmt)))

(defun mcw:nix-mode-format-buffer-with-nixfmt ()
  (interactive)
  (save-buffer)
  (shell-command (format "nixfmt %s" buffer-file-name))
  (revert-buffer :ignore-auto :noconfirm))


(provide 'init)
;;; init.el ends here

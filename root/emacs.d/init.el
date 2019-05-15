;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

(add-to-list 'default-frame-alist '(font . "mononoki-13"))
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 80))

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

;; Helper for keybindings
(use-package general)

;; Vim mode
(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  :config
  (evil-mode 1)
  (general-define-key
   :states '(normal insert)
   :keymaps 'comint-mode-map
   "C-j" 'comint-next-input
   "C-k" 'comint-previous-input))

(use-package evil-escape
  :init
  (setq-default evil-escape-key-sequence "fd")
  :config
  (evil-escape-mode 1))

;; Themes
(use-package zenburn-theme)
(use-package leuven-theme
  :custom
  (leuven-scale-outline-headlines nil "")
  (leuven-scale-org-agenda-structure nil ""))

(load-theme 'leuven t)


(use-package ivy
  :config
  (ivy-mode 1)
  (general-define-key
   "C-x b" 'ivy-switch-buffer
   "C-c C-r" 'ivy-resume)
  (general-define-key
   :keymaps 'ivy-minibuffer-map
   "C-j" 'ivy-next-line
   "C-k" 'ivy-previous-line
   "C-h" 'ivy-backward-kill-word))

(use-package counsel
  :config
  (general-define-key
   "C-s" 'swiper
   "M-x" 'counsel-M-x
   "C-x C-f" 'counsel-find-file
   "C-x C-r" 'counsel-recentf
   "C-c g" 'counsel-git
   "C-c j" 'counsel-git-grep
   "C-x l" 'counsel-locate))

(use-package counsel-tramp
  :config
  (general-define-key "C-c f" 'counsel-tramp))

;; Which Key
(use-package which-key
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

;; Projectile
(use-package projectile
  :init
  (setq projectile-require-project-root nil)
  (setq projectile-completion-system 'ivy)
  (require 'subr-x)  ;; address bug similar to https://github.com/alphapapa/org-protocol-capture-html/issues/7
  :config
  (projectile-mode 1)
  (general-define-key
   :keymaps 'projectile-mode-map
   "C-c p" 'projectile-command-map))

(use-package counsel-projectile
  :config (counsel-projectile-mode 1))

(use-package ripgrep)

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; completion
(use-package company
  :config
  (company-mode 1)
  (general-define-key
   :keymaps 'company-active-map
   "C-j" 'company-select-next
   "C-k" 'company-select-previous
   "jk" 'company-complete)
  :hook (after-init . global-company-mode))

;; smartparens
(use-package smartparens)

(use-package evil-smartparens
  :hook (smartparens-enabled . evil-smartparens-mode))

;; surround
(use-package evil-surround :config (global-evil-surround-mode 1))

;; line numbering
(use-package nlinum-relative
  :config
  (nlinum-relative-setup-evil)
  :hook (prog-mode . nlinum-relative-mode))

;; highlighting TODO items in comments
(use-package hl-todo :config (hl-todo-mode 1))

;; Magit
(use-package magit
  :config
  (general-define-key "C-x g" 'magit-status))

(use-package evil-magit)

;; browse at remote
(use-package browse-at-remote
  :config
  (general-define-key "C-c b" 'browse-at-remote))

;; neotree
(use-package neotree)

;; org-mode
(setq org-startup-indented t)
(setq mcw:org-gtd-directory (file-name-as-directory "~/.gtd/"))
(setq mcw:org-gtd-agenda-file (concat mcw:org-gtd-directory "gtd.org"))
(setq org-agenda-files (list mcw:org-gtd-agenda-file))

(setq org-capture-templates
      '(("t" "Todo" entry
	 (file mcw:org-gtd-agenda-file)
	 "* TODO %? :Inbox:\n%U\n")))

(setq org-agenda-custom-commands
      '(("n" "Agenda and all TODOs"
	 ((agenda "")
	  (tags "Inbox")
	  (tags "Reading")
	  (alltodo "")))))

(add-hook 'org-mode-hook 'turn-on-flyspell)
(add-hook 'org-capture-mode-hook 'org-align-all-tags)

(general-define-key
 "C-c a" 'org-agenda
 "C-c c" 'org-capture)

(with-eval-after-load 'org-agenda
  (setq org-stuck-projects '("+LEVEL=1/-DONE" ("TODO" "NEXT" "NEXTACTION") nil ""))
  (general-define-key
   :keymaps 'org-agenda-mode-map
   ;; minimal set of evil movements in org-agenda
   "C-w h" 'evil-window-left
   "C-w l" 'evil-window-right
   "j" 'evil-next-line
   "k" 'evil-previous-line))

(defun mcw:save-and-sync-gtd ()
  (interactive)
  (org-save-all-org-buffers)
  (let ((default-directory mcw:org-gtd-directory))
    (shell-command "git-sync")))

(general-define-key "C-c t s" 'mcw:save-and-sync-gtd)

;; org-babel
(with-eval-after-load 'org
  (setq org-confirm-babel-evaluate nil)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (haskell . t)
     (jupyter . t)
     (R . t)
     (restclient . t)
     (scala . t))))

;; LaTeX
(use-package tex
  :ensure auctex)

(use-package cdlatex
  :hook (org-mode . turn-on-org-cdlatex))

;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

;; Haskell
(use-package intero
  :config
  (intero-global-mode 1)
  (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))

(with-eval-after-load 'haskell-mode
  (general-define-key
   :keymaps 'haskell-mode-map
   "C-c C-f" 'mcw:haskell-mode-format-buffer-with-brittany))

(defun mcw:haskell-mode-format-buffer-with-brittany ()
  (interactive)
  (save-buffer)
  (shell-command-to-string (format "brittany --write-mode inplace %s" buffer-file-name))
  (revert-buffer :ignore-auto :noconfirm))

(setq haskell-process-type 'stack-ghci)

;; Scala
(use-package ensime
  :hook (scala-mode . smartparens-mode)
  :custom (ensime-startup-notification 'nil))

(with-eval-after-load 'scala-mode
  (general-define-key
   :keymaps 'scala-mode-map
   "C-c C-e" 'ensime
   "C-c C-f" 'mcw:scala-mode-format-buffer-with-scalafmt))

(with-eval-after-load 'ensime-mode
  (general-define-key
   :keymaps 'ensime-mode-map
   "C-c C-v g" 'ensime-edit-definition-of-thing-at-point))

(defun mcw:scala-mode-format-buffer-with-scalafmt ()
  (interactive)
  (save-buffer)
  (shell-command-to-string (format "scalafmt %s" buffer-file-name))
  (revert-buffer :ignore-auto :noconfirm))

;; Jupyter
(use-package jupyter)

;; Python
(use-package anaconda-mode
  :hook ((python-mode . anaconda-mode)
	 (python-mode . anaconda-eldoc-mode)))

(use-package company-anaconda
  :config
  (eval-after-load "company"
    '(add-to-list 'company-backends 'company-anaconda)))

(use-package pyenv-mode
  :hook python-mode)

;; json
(use-package json-mode
  :config
  (general-define-key
   :keymaps 'json-mode-map
   "C-c C-f" 'json-pretty-print-buffer))

(add-hook 'json-mode-hook 'flycheck-mode)

;; yaml
(use-package yaml-mode)

;; logview
(use-package logview)

;; rest client
(use-package ob-restclient)

;; deft
(use-package deft
  :init
  (setq deft-extension "org")
  :config
  (general-define-key "C-c d" 'deft))

;; pinentry
(use-package pinentry :config (pinentry-start))

;; SQL
(defun mcw:sql-connect ()
  (interactive)
  (if (not (boundp 'sql-connections)) (mcw:load-sql-connections) nil)
  (call-interactively 'sql-connect))

(defun mcw:load-sql-connections ()
  (interactive)
  (require 'sql-connections "~/.sql-connections.el.gpg"))

(general-define-key "C-c s" 'mcw:sql-connect)

(add-hook 'sql-interactive-mode-hook
	  (lambda ()
	    (toggle-truncate-lines t)))

;; restclient
(use-package restclient)
(use-package company-restclient)

;; plantuml
(use-package plantuml-mode
  :config
  (setq org-plantuml-jar-path (expand-file-name "~/.local/libexec/plantuml.jar")))

(use-package flycheck-plantuml)

;; S3
(use-package s3ed
  :config
  (general-define-key
   "C-c r f" 's3ed-find-file
   "C-c r s" 's3ed-save-file))

;; custom file
(setq custom-file "~/.emacs-custom.el")

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
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Vim mode
(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  :config
  (evil-mode 1))

(use-package evil-escape
  :ensure t
  :init
  (setq-default evil-escape-key-sequence "fd")
  :config
  (evil-escape-mode 1))

(use-package zenburn-theme :ensure t)
(load-theme 'zenburn t)

;; Custom keybinding
(use-package general
  :ensure t)

(use-package ivy
  :ensure t
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
  :ensure t
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
  :ensure t
  :config
  (general-define-key "C-c f" 'counsel-tramp))

;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

;; Projectile
(use-package projectile
  :ensure t
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
  :ensure t
  :config (counsel-projectile-mode 1))

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; completion
(use-package company
  :ensure t
  :config
  (company-mode 1)
  (general-define-key
   :keymaps 'company-active-map
   "C-j" 'company-select-next
   "C-k" 'company-select-previous
   "jk" 'company-complete)
  :hook (after-init . global-company-mode))

;; surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; line numbering
(use-package nlinum-relative
  :ensure t
  :config
  (nlinum-relative-setup-evil)
  :hook (prog-mode . nlinum-relative-mode))

;; highlighting TODO items in comments
(use-package hl-todo
  :ensure t
  :config
  (hl-todo-mode 1))

;; Magit
(use-package magit
  :ensure t
  :config
  (general-define-key "C-x g" 'magit-status))

(use-package evil-magit :ensure t)

;; browse at remote
(use-package browse-at-remote
  :ensure t
  :config
  (general-define-key "C-c r" 'browse-at-remote)
  :pin melpa-stable)

;; neotree
(use-package neotree
  :ensure t
  :pin melpa-stable)

;; org-mode
(setq org-startup-indented t)
(setq org-agenda-files '("~/.gtd/gtd.org"))

(setq org-capture-templates
      '(("t" "Todo" entry
	 (file "~/.gtd/gtd.org")
	 "* TODO %? :inbox:\n%U\n")))

(add-hook 'org-capture-mode-hook 'org-align-all-tags)

(general-define-key
 "C-c a" 'org-agenda
 "C-c c" 'org-capture)

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map "j" 'evil-next-line)
  (define-key org-agenda-mode-map "k" 'evil-previous-line))

;; org-babel
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (plantuml . t))))

;; Haskell
(use-package intero
  :ensure t
  :config
  (intero-global-mode 1)
  (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))

(with-eval-after-load 'haskell-mode
  (general-define-key
   :keymaps 'haskell-mode-map
   "C-c C-f" 'haskell-mode-format-buffer-with-brittany))

(defun haskell-mode-format-buffer-with-brittany ()
  (interactive)
  (save-buffer)
  (shell-command-to-string (format "brittany --write-mode inplace %s" buffer-file-name))
  (revert-buffer :ignore-auto :noconfirm))

;; Scala
(use-package ensime
  :ensure t
  :pin melpa-stable
  :custom
  (ensime-startup-notification 'nil))

(with-eval-after-load 'scala-mode
  (general-define-key
   :keymaps 'scala-mode-map
   "C-c C-f" 'scala-mode-format-buffer-with-scalafmt))

(with-eval-after-load 'ensime-mode
  (general-define-key
   :keymaps 'ensime-mode-map
   "C-c C-v g" 'ensime-edit-definition-of-thing-at-point))

(defun scala-mode-format-buffer-with-scalafmt ()
  (interactive)
  (save-buffer)
  (shell-command-to-string (format "scalafmt %s" buffer-file-name))
  (revert-buffer :ignore-auto :noconfirm))

;; logview
(use-package logview
  :ensure t)

;; deft
(use-package deft
  :init (setq deft-extension "org")
  :ensure t)

;; pinentry
(use-package pinentry
  :ensure t
  :config
  (pinentry-start))

;; SQL
(defun mcw:load-sql-connections ()
  (interactive)
  (require 'sql-connections "~/.sql-connections.el.gpg"))

(general-define-key "C-c s" 'sql-connect)

(add-hook 'sql-interactive-mode-hook
	  (lambda ()
	    (toggle-truncate-lines t)))

;; plantuml
(use-package plantuml-mode
  :ensure t
  :config
  (setq org-plantuml-jar-path (expand-file-name "~/.local/libexec/plantuml.jar")))

(use-package flycheck-plantuml
  :ensure t)

;; custom file
(setq custom-file "~/.emacs-custom.el")

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

(add-to-list 'default-frame-alist '(font . "mononoki-14"))
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

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-h") 'ivy-backward-kill-word))

(use-package counsel :ensure t)

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)

(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)

(global-set-key (kbd "C-c C-r") 'ivy-resume)

(global-set-key (kbd "C-j") 'comint-next-input)
(global-set-key (kbd "C-k") 'comint-previous-input)

;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

;; Custom keybinding
(use-package general
  :ensure t
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "/"   '(counsel-rg :which-key "ripgrep")
   "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
   "SPC" '(counsel-M-x :which-key "M-x")
   ;; Files
   "ff"  '(counsel-find-file :which-key "find files")
   "fr"  '(counsel-recentf :which-key "recent files")
   ;; Buffers
   "bb"  '(ivy-switch-buffer :which-key "buffers list")
   ;; Window
   "wl"  '(windmove-right :which-key "move right")
   "wh"  '(windmove-left :which-key "move left")
   "wk"  '(windmove-up :which-key "move up")
   "wj"  '(windmove-down :which-key "move bottom")
   "w/"  '(split-window-right :which-key "split right")
   "w-"  '(split-window-below :which-key "split bottom")
   "wx"  '(delete-window :which-key "delete window")
   ;; Projects
   "pp"  '(projectile-switch-project :which-key "switch project")
   "pf"  '(projectile-find-file :which-key "find project file")
   "pg"  '(projectile-ripgrep :which-key "search project files")
   ;; Version control
   "gs"  '(magit-status :which-key "git status")
   ;; SQL
   "sc"  '(sql-connect :which-key "connect to database")
   ;; Others
   "at"  '(ansi-term :which-key "open terminal")
   "aoc" '(org-capture :which-key "org capture")
   "aoa" '(org-agenda :which-key "org agenda")))

;; Projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-require-project-root nil)
  (setq projectile-completion-system 'ivy)
  (require 'subr-x)  ;; address bug similar to https://github.com/alphapapa/org-protocol-capture-html/issues/7
  :config (projectile-mode 1))

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
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous)
  (define-key company-active-map (kbd "jk") 'company-complete)
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
  (add-hook 'prog-mode-hook 'nlinum-relative-mode))

;; Magit
(use-package magit :ensure t)
(use-package evil-magit :ensure t)

;; perspectives
(use-package persp-mode
  :ensure t
  :pin melpa-stable
  :config (persp-mode 1))

;; org-mode

(setq org-agenda-files
      '("~/.gtd/inbox.org"
	"~/.gtd/gtd.org"
	"~/.gtd/someday.org"))

(setq org-capture-templates
      '(("t" "Todo" entry
	 (file "~/.gtd/inbox.org")
	 "* TODO %?\n%U\n")))

(setq org-refile-targets
      '(("~/.gtd/gtd.org" :maxlevel . 2)
	("~/.gtd/someday.org" :level . 1)))

;; Haskell
(use-package intero
  :ensure t
  :config
  (intero-global-mode 1)
  (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))

(with-eval-after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c C-f") #'haskell-mode-format-buffer-with-brittany))

(defun haskell-mode-format-buffer-with-brittany ()
  (interactive)
  (shell-command-to-string (format "brittany --write-mode inplace %s" buffer-file-name))
  (revert-buffer :ignore-auto :noconfirm))

;; Scala
(use-package ensime
  :ensure t
  :pin melpa-stable
  :custom
  (ensime-startup-notification 'nil))

(with-eval-after-load 'scala-mode
  (define-key scala-mode-map (kbd "C-c C-f") #'scala-mode-format-buffer-with-scalafmt))

(defun scala-mode-format-buffer-with-scalafmt ()
  (interactive)
  (shell-command-to-string (format "scalafmt %s" buffer-file-name))
  (revert-buffer :ignore-auto :noconfirm))

;; logview
(use-package logview
  :ensure t)

;; deft
(use-package deft
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

(add-hook 'sql-interactive-mode-hook
	  (lambda ()
	    (toggle-truncate-lines t)))

;; custom file
(setq custom-file "~/.emacs-custom.el")
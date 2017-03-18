;; .spacemacs
;; Matt Wittmann

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '((auto-completion :variables
                      auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-complete-with-key-sequence "kj"
                      auto-completion-complete-with-key-sequence-delay 0.1)
     better-defaults
     c-c++
     csv
     docker
     emacs-lisp
     ess
     extra-langs
     git
     github
     haskell
     (html :variables
           css-indent-offset 2
           web-mode-markup-indent-offset 2
           web-mode-css-indent-offset 2
           web-mode-code-indent-offset 2
           web-mode-attr-indent-offset 2)
     (javascript :variables
                 flycheck-eslintrc "~/.eslintrc"
                 js-indent-level 2
                 js2-basic-offset 2
                 js2-strict-missing-semi-warning nil)
     latex
     markdown
     octave
     (org :variables
          org-startup-indented t
          org-babel-load-languages
          '((emacs-lisp . t)
            (haskell . t)
            (ipython . t)
            (js . t)
            (julia . t)
            (latex . t)
            (octave . t)
            (python . t)
            (R . t)
            (sql . t))
          org-confirm-babel-evaluate nil
          org-enable-github-support t
          org-latex-listings 'minted
          org-latex-minted-options
          '(("fontsize" "\\footnotesize")
            ("frame" "lines"))
          org-latex-to-pdf-process
          '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
     osx
     python
     react
     restclient
     scala
     scheme
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     shell-scripts
     spell-checking
     sql
     syntax-checking
     version-control
     yaml)
   dotspacemacs-additional-packages
   '(cdlatex
     ob-ipython)
   dotspacemacs-excluded-packages '()
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '(recents projects)
   dotspacemacs-startup-recent-list-size 5
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(solarized-dark
                         solarized-light
                         spacemacs-dark
                         spacemacs-light
                         zenburn
                         leuven)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-command-key ":"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-use-ido nil
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-micro-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 100
   dotspacemacs-inactive-transparency 80
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers 'relative
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'changed
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set before
packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;;
  ;; global emacs behavior
  ;;
  (setq
   vc-follow-symlinks t    ;; auto follow symlinks
   persp-auto-save-opt 0)

  ;;
  ;; projectile
  ;;
  (setq projectile-globally-ignored-file-suffixes '("pyc" "swp"))


  ;;
  ;; julia
  ;;
  (setq inferior-julia-program-name "/usr/local/bin/julia")

  ;;
  ;; maxima
  ;;
  (add-to-list 'load-path "/usr/local/Cellar/maxima/5.38.1/share/maxima/5.38.1/emacs")
  (autoload 'maxima-mode "maxima" "Maxima mode" t)
  (autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
  (autoload 'maxima "maxima" "Maxima interaction" t)
  (autoload 'imath-mode "imath" "Imath mode for math formula input" t)
  (setq imaxima-use-maxima-mode-flag t)
  (add-to-list 'auto-mode-alist '("\\.ma[cx]" . maxima-mode))

  ;;
  ;; org mode
  ;;
  (eval-after-load 'org
    (lambda ()
      (setq mcw/org-directory "~/org")
      (defun mcw/org-prefix-file (file)
        (concat (file-name-as-directory mcw/org-directory) file))
      (setq
       org-default-notes-file (mcw/org-prefix-file "notes.org")
       org-agenda-files (mapcar 'mcw/org-prefix-file '("gtd.org"))
       org-capture-templates
       '(("t" "Todo" entry (file+headline (mcw/org-prefix-file "gtd.org") "Tasks")
          "* TODO %?\nDEADLINE: %t\n%i\n%a")
         ("r" "Read" entry (file+headline (mcw/org-prefix-file "gtd.org") "Read")
          "* TODO %?\nDEADLINE: %t\n%i\n%a")
         ("n" "Notes" entry (file+datetree (mcw/org-prefix-file "notes.org"))
          "* %?\n  %i\n  %a")
         ("j" "Journal" entry (file+datetree (mcw/org-prefix-file "journal.org"))
          "* %?\nEntered on %U\n")))

      ;; custom org shortcuts
      (defun mcw/open-notes-file ()
        (interactive) (find-file org-default-notes-file))
      (defun mcw/open-gtd-file ()
        (interactive) (find-file (mcw/org-prefix-file "gtd.org")))
      (spacemacs/set-leader-keys "aon" 'mcw/open-notes-file)
      (spacemacs/set-leader-keys "aog" 'mcw/open-gtd-file)

      ;; org-export
      (require 'ox-beamer)

      ;; org-babel
      (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
      (add-hook 'org-mode-hook 'turn-on-org-cdlatex)))

  ;;
  ;; Database connections
  ;; https://truongtx.me/2014/08/23/setup-emacs-as-an-sql-database-client
  ;;
  (load "~/.sql-connections")
  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (toggle-truncate-lines t)))
  (add-hook 'sql-mode-hook
            (lambda ()
              (abbrev-mode t)
              (modify-syntax-entry ?_ "w" sql-mode-syntax-table)
              (when (file-exists-p "~/.sql-abbreviations")
                (load "~/.sql-abbreviations"))))
  (defun my-sql-connect (product connection)
    ;; load the password
    (require 'sql-my-password "~/.sql-my-password.el.gpg")
    (let* ((sql-product product)
           (password (cadr (assoc connection sql-my-password)))
           (sql-connection-alist (cons (list 'password password)
                                       sql-connection-alist)))
      ;; Postgres doesn't allow providing password on command line,
      ;; handle that case.  This should go inside sql-postgres.
      (when (and (eq sql-product 'postgres) password)
        (setenv "PGPASSWORD" password))
      (sql-connect connection)
      (when (and (eq sql-product 'postgres) password)
        (setenv "PGPASSWORD" nil)))))
(custom-set-variables
 '(epg-gpg-program "gpg1"))

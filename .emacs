;; -*- mode: lisp -*-

(require 'package)
(defvar gnu '("gnu" . "http://elpa.gnu.org/packages/"))
(defvar melpa '("melpa" . "https://melpa.org/packages/"))
(defvar melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))
(defvar ublt '("ublt" . "https://elpa.ubolonton.org/packages/"))
(defvar org-elpa '("org" . "http://orgmode.org/elpa/"))

(setq package-archives nil)
(add-to-list 'package-archives melpa-stable t)
(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives gnu t)
(add-to-list 'package-archives ublt t)
(add-to-list 'package-archives org-elpa t)
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(defun packages-install (&rest packages)
  (message "running packages-install")
  (mapc (lambda (package)
          (let ((name (car package))
                (repo (cdr package)))
            (when (not (package-installed-p name))
              (let ((package-archives (list repo)))
                (package-initialize)
                (package-install name)))))
        packages)
  (package-initialize)
  (delete-other-windows))
(defun init--install-packages ()
  (message "Lets install some packages")
  (packages-install
   (cons 'use-package melpa)))
(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; Disable start-up message
(setq inhibit-startup-message t)
(setq initial-major-mode 'org-mode)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(setq ring-bell-function 'ignore)
(setq auto-window-vscroll nil)
(setq byte-compile-warnings '(cl-functions))
(blink-cursor-mode 1)
(setq cursor-in-non-selected-windows t)
(setq show-paren-delay 0)
(show-paren-mode)
(transient-mark-mode 1)

(setq dired-listing-switches "-laGh1v --group-directories-first")

;; Make scheme less colorful
;; (setq inhibit-compacting-font-caches t)

;; Use highlight on curent line
;; (when window-system (global-hl-line-mode))

;; (global-visual-line-mode t)
(setq jit-lock-defer-time nil)
;; (setq fast-but-imprecise-scrolling t)
;; (setq font-lock-maximum-decoration 3)

;; Disable backups and auto-saves
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Change yes-no to y-n
(fset 'yes-or-no-p 'y-or-n-p)

;; Treat camelCase as sepecrate words
;; (global-subword-mode 1)
(setq frame-title-format '("Emacs"))
(setq column-number-mode t)


;; (setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; (add-hook 'prog-mode-hook 'visual-line-mode)

(global-set-key (kbd "C-S-l") 'display-line-numbers-mode)
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-S-f") 'rg)
(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "C-<tab>") 'mode-line-other-buffer)

;; ;; Never use tabs, use spaces instead
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default js-indent-level 4)
(setq-default css-indent-offset 4)
(setq-default c-basic-offset 4)

;; (autoload 'View-scroll-half-page-forward "view")
;; (autoload 'View-scroll-half-page-backward "view")
;; (global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
;; (global-set-key (kbd "M-v") 'View-scroll-half-page-backward)

(set-face-attribute 'default nil
                    :family "SF Mono"
                    :height 140
                    :weight 'normal
                    :width 'normal)

;; (use-package smartparens
;;   :ensure t
;;   :config
;;   (smartparens-global-mode))

(use-package prettier
  :ensure t
  :config
  (setenv "NODE_PATH" "/home/hoang/node_modules"))

(add-hook 'nxml-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq-default nxml-child-indent 4)
            (setq-default nxml-attribute-indent 4)))

;; ;; View and Windows
(defun ian/split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun ian/split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'ian/split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") 'ian/split-and-follow-vertically)


(defun kill-current-line (&optional n)
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (let ((kill-whole-line t))
      (kill-line n))))
(defun kill-line-or-region ()
  "Kill region if active only or kill line normally."
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-region)
    (call-interactively 'kill-line)))

(global-set-key (kbd "C-k") 'kill-line-or-region)

(define-key ctl-x-map (kbd "C-b") '(lambda (&optional arg)
                                     (interactive "P")
                                     (setq arg (not arg))
                                     (buffer-menu arg)))

;; from http://mbork.pl/2014-04-04_Fast_buffer_switching_and_friends
(defun bh:switch-bury-or-kill-buffer (&optional aggr)
  "With no argument, switch (but unlike C-x b, without the need
to confirm).  With C-u, bury current buffer.  With double C-u,
kill it (unless it's modified)."
  (interactive "P")
  (cond
   ((eq aggr nil) (progn
                    (cl-dolist (buf '("*Buffer List*" "*Ibuffer*" "*Bookmark List* " "*vc-change-log*" "*Locate*" "*grep*" "*compilation*" ))
                      (when (get-buffer buf)
                        (bury-buffer buf)))
                    (switch-to-buffer (other-buffer))))
   ((equal aggr '(4)) (bury-buffer))
   ((equal aggr '(16)) (kill-buffer-if-not-modified (current-buffer)))))

(global-set-key (kbd "C-`") 'bh:switch-bury-or-kill-buffer)

;; ;; from http://mbork.pl/2015-04-25_Some_Dired_goodies
(put 'dired-find-alternate-file 'disabled nil) ; visiting a file from dired closes the dired buffer

;; (add-hook 'prog-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

(use-package beacon
  :ensure t
  :config
  (setq beacon-size 15)
  (setq beacon-blink-when-window-scrolls nil)
  (setq beacon-blink-when-window-changes t)
  (beacon-mode 1))

(use-package org
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'org-indent-mode)
  )

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode))


(use-package dired+
  :load-path "3rd"
  :init
  (setq diredp-hide-details-initially-flag nil)
  (setq diredp-hide-details-propagate-flag nil)
  :config
  (diredp-toggle-find-file-reuse-dir 1))

;; Quickly edit .emacs
(defun config-visit ()
  (interactive)
  (find-file "~/.emacs"))
(global-set-key (kbd "C-c e") 'config-visit)

(use-package csv-mode
  :ensure t)

;; ;; which key - bring up some help
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (which-key-mode t)
  (setq which-key-idle-delay 0.3)
  (setq which-key-idle-secondary-delay 0.3))

;; ;; Ace windows
(use-package ace-window
  :ensure t
  :init
  (progn
    (setq aw-scope 'global) ;; was frame
    (ace-window-display-mode)
    (setq aw-background nil)
    (global-set-key (kbd "C-x O") 'other-frame)
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))
    )
  :config
  (global-set-key (kbd "M-o") 'ace-window))

(windmove-default-keybindings)

(defun find-file-ace-window ()
  "Use ace window to select a window for opening a file from dired."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (> (length (aw-window-list)) 1)
        (aw-select "" (lambda (window)
                        (aw-switch-to-window window)
                        (find-file file)))
      (find-file-other-window file))))

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map "o" 'find-file-ace-window)))

(use-package neotree
  :ensure t
  :bind (("C-c f t" . neotree-toggle)
         ([f8] . neotree-toggle))
  :config (setq neo-window-width 32
                neo-create-file-auto-open t
                neo-banner-message nil
                neo-show-updir-line nil
                neo-mode-line-type 'neotree
                neo-smart-open t
                neo-dont-be-alone t
                neo-persist-show nil
                neo-show-hidden-files t
                neo-auto-indent-point t)
  (define-key neotree-mode-map (kbd "i") #'neotree-enter-horizontal-split)
  (define-key neotree-mode-map (kbd "I") #'neotree-enter-vertical-split))

;; (use-package treemacs
;;   :ensure t
;;   :defer t
;;   :init
;;   (with-eval-after-load 'winum
;;     (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
;;   :config
;;   (progn
;;     (setq treemacs-follow-after-init          t
;;           treemacs-width                      35
;;           treemacs-indentation                2
;;           treemacs-git-integration            t
;;           treemacs-collapse-dirs              3
;;           treemacs-silent-refresh             nil
;;           treemacs-change-root-without-asking nil
;;           treemacs-sorting                    'alphabetic-desc
;;           treemacs-show-hidden-files          t
;;           treemacs-never-persist              nil
;;           treemacs-is-never-other-window      nil
;;           treemacs-goto-tag-strategy          'refetch-index)

;;     (treemacs-follow-mode t)
;;     (treemacs-filewatch-mode t))
;;   :bind
;;   (:map global-map
;;         ([f8]        . treemacs-toggle)
;;         ("M-0"       . treemacs-select-window)
;;         ("C-c 1"     . treemacs-delete-other-windows)))

;; (use-package treemacs-projectile
;;   :defer t
;;   :ensure t
;;   :config
;;   (setq treemacs-header-function #'treemacs-projectile-create-header))

;; ;; To nyan or not nyan
;; (use-package nyan-mode
;;   :ensure t
;;   :config
;;   (nyan-mode)
;;   (nyan-start-animation))

;; (use-package fill-column-indicator
;;   :ensure t
;;   :config
;;   (setq-default fci-rule-column 80)
;;   (add-hook 'python-mode-hook 'fci-mode))

(use-package doom-modeline
  :ensure t
  :hook
  (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 25)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-window-width-limit fill-column)
  (setq doom-modeline-project-detection 'project)
  (setq doom-modeline-buffer-file-name-style 'auto)
  (setq find-file-visit-truename t)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-indent-info nil)
  (setq doom-modeline-checker-simple-format nil)
  (setq doom-modeline-vcs-max-length 12)
  (setq doom-modeline-env-version t)
  (setq doom-modeline-env-enable-python t)
  (setq doom-modeline-minor-modes t)
  )

(use-package minions
  :ensure t
  :config
  (minions-mode 1))


(setq-default python-indent-guess-indent-offset-verbose nil)
;; (setq-default py-python-command "python3")
;; (setq-default python-shell-interpreter "python3")

;; (use-package flycheck
;;   :ensure t
;;   :config
;;   (add-hook 'after-init-hook #'global-flycheck-mode)
;;   (setq flycheck-check-syntax-automatically '(mode-enabled save))
;;   (add-hook 'python-mode-hook
;;             (lambda ()
;;               (setq flycheck-checker 'python-flake8
;;                     flycheck-checker-error-threshold 400))))

;; (use-package evil
;;   :ensure t
;;   :config
;;   (evil-mode 1))

(use-package ag
  :ensure t)

(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings))

(use-package ripgrep
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets :ensure t)
  (setq yas-snippet-dirs (append yas-snippet-dirs '("~/Dropbox/snippets/")))
  (yas-global-mode 1))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-global-modes '(not org-mode markdown-mode))
  (setq company-minimum-prefix-length 3)
  (global-set-key (kbd "C-c y") 'company-yasnippet)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package js2-mode
  :ensure t
  :config
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (add-hook 'js2-mode-hook
            (defun my-js2-mode-setup ()
              (flycheck-mode t)
              (when (executable-find "eslint")
                (flycheck-select-checker 'javascript-eslint)))))

(use-package company-jedi
  :ensure t
  :config
  (add-hook 'python-mode-hook (defun python-mode-hook ()
                                (add-to-list 'company-backends 'company-jedi))))

(use-package py-isort
  :ensure t
  )

;; (use-package pyenv-mode
;;   :ensure t
;;   :init
;;   (add-to-list 'exec-path "~/.pyenv/shims")
;;   (setenv "WORKON_HOME" "~/.pyenv/versions/")
;;   :bind
;;   ("C-x p e" . pyenv-activate-current-project)
;;   :config
;;   (pyenv-mode)
;;   (defun pyenv-activate-current-project ()
;;   "Automatically activates pyenv version if .python-version file exists."
;;   (interactive)
;;   (f-traverse-upwards
;;    (lambda (path)
;;      (message path)
;;      (let ((pyenv-version-path (f-expand ".python-version" path)))
;;        (if (f-exists? pyenv-version-path)
;;             (let ((pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8))))
;;               (pyenv-mode-set pyenv-current-version)
;;               (message (concat "Setting virtualenv to " pyenv-current-version))))))))
;;   )

(use-package pyenv-mode-auto
  :ensure t
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  (setenv "VIRTUALENVWRAPPER_HOOK_DIR" "~/.pyenv/versions/")
  :config
  )

;; (use-package elpy
;;   :ensure t
;;   :defer 0.5
;;   :init
;;   (advice-add 'python-mode :before 'elpy-enable)
;;   :config
;;   (elpy-enable)
;;   (setq eldoc-idle-delay 0.3)
;;   (add-hook 'elpy-mode-hook (lambda ()
;;                               (highlight-indentation-mode -1)
;;                               (flycheck-mode)
;;                               (setq python-check-command "flake8")))
;;   (when (require 'flycheck nil t)
;;     (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;     (add-hook 'elpy-mode-hook 'flycheck-mode))
;;   (define-key elpy-refactor-map (kbd "f")
;;     (cons (format "%sormat code"
;;                   (propertize "f" 'face 'bold))
;;           'elpy-black-fix-code))
;;   :bind
;;   (([f9] . elpy-black-fix-code)))

(use-package blacken
  :ensure t)

(use-package python-black
  :ensure t
  :after python)

(remove-hook 'find-file-hooks 'vc-refresh-state)

(use-package magit
  :ensure t
  :config
  (add-hook 'magit-process-find-password-functions
            'magit-process-password-auth-source)
  :bind (("C-x g" . magit-status)))


(use-package all-the-icons
  :ensure t
  :defer 0.5)

(use-package ivy
  :delight
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window)
         ("M-H"   . ivy-resume)
         :map ivy-minibuffer-map
         ("<tab>" . ivy-alt-done)
         ("C-i" . ivy-partial-or-done)
         ("S-SPC" . nil)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-switch-buffer-kill))
  :custom
  (ivy-case-fold-search-default t)
  (ivy-count-format "(%d/%d)")
  (ivy-re-builders-alist '((t . ivy--regex-plus)))
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package counsel
  :after ivy
  :delight
  :bind (("C-x C-d" . counsel-dired-jump)
         ("C-x C-h" . counsel-minibuffer-history)
         ("C-x C-l" . counsel-find-library)
         ("C-x C-r" . counsel-recentf)
         ("C-r" . counsel-minibuffer-history)
         ("C-x C-u" . counsel-unicode-char)
         ("C-x C-v" . counsel-set-variable))
  :config (counsel-mode)
  :custom (counsel-rg-base-command "rg -S -M 150 --no-heading --line-number --color never %s"))

(use-package ivy-pass
  :after ivy
  :commands ivy-pass)

(use-package ivy-rich
  :defer 0.1
  :preface
  (defun ivy-rich-branch-candidate (candidate)
    "Displays the branch candidate of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (format "%s%s"
                (propertize
                 (replace-regexp-in-string abbreviated-home-dir "~/"
                                           (file-name-directory
                                            (directory-file-name candidate)))
                 'face 'font-lock-doc-face)
                (propertize
                 (file-name-nondirectory
                  (directory-file-name candidate))
                 'face 'success)))))
  ;; (defun ivy-rich-compiling (candidate)
  ;;   "Displays compiling buffers of the candidate for ivy-rich."
  ;;   (let* ((candidate (expand-file-name candidate ivy--directory)))
  ;;     (if (or (not (file-exists-p candidate)) (file-remote-p candidate)
  ;;             (not (magit-git-repo-p candidate)))
  ;;         ""
  ;;       (if (my/projectile-compilation-buffers candidate)
  ;;           "compiling"
  ;;         ""))))
  (defun ivy-rich-file-group (candidate)
    "Displays the file group of the candidate for ivy-rich"
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (let* ((group-id (file-attribute-group-id (file-attributes candidate)))
               (group-function (if (fboundp #'group-name) #'group-name #'identity))
               (group-name (funcall group-function group-id)))
          (format "%s" group-name)))))
  (defun ivy-rich-file-modes (candidate)
    "Displays the file mode of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (format "%s" (file-attribute-modes (file-attributes candidate))))))
  (defun ivy-rich-file-size (candidate)
    "Displays the file size of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (let ((size (file-attribute-size (file-attributes candidate))))
          (cond
           ((> size 1000000) (format "%.1fM " (/ size 1000000.0)))
           ((> size 1000) (format "%.1fk " (/ size 1000.0)))
           (t (format "%d " size)))))))
  (defun ivy-rich-file-user (candidate)
    "Displays the file user of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (let* ((user-id (file-attribute-user-id (file-attributes candidate)))
               (user-name (user-login-name user-id)))
          (format "%s" user-name)))))
  (defun ivy-rich-switch-buffer-icon (candidate)
    "Returns an icon for the candidate out of `all-the-icons'."
    (with-current-buffer
        (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode :height 0.9)))
        (if (symbolp icon)
            (all-the-icons-icon-for-mode 'fundamental-mode :height 0.9)
          icon))))
  :config
  (plist-put ivy-rich-display-transformers-list
             'counsel-find-file
             '(:columns
               ((ivy-rich-candidate               (:width 73))
                (ivy-rich-file-user               (:width 8 :face font-lock-doc-face))
                (ivy-rich-file-group              (:width 4 :face font-lock-doc-face))
                (ivy-rich-file-modes              (:width 11 :face font-lock-doc-face))
                (ivy-rich-file-size               (:width 7 :face font-lock-doc-face))
                (ivy-rich-file-last-modified-time (:width 30 :face font-lock-doc-face)))))
  ;; (plist-put ivy-rich-display-transformers-list
  ;;            'counsel-projectile-switch-project
  ;;            '(:columns
  ;;              ((ivy-rich-branch-candidate        (:width 80))
  ;;               (ivy-rich-compiling))))
  (plist-put ivy-rich-display-transformers-list
             'ivy-switch-buffer
             '(:columns
               ((ivy-rich-switch-buffer-icon       (:width 2))
                (ivy-rich-candidate                (:width 40))
                (ivy-rich-switch-buffer-size       (:width 7))
                (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                (ivy-rich-switch-buffer-major-mode (:width 20 :face warning)))
               :predicate (lambda (cand) (get-buffer cand))))
  (ivy-rich-mode 1))

(use-package all-the-icons-ivy
  :after (all-the-icons ivy)
  :custom (all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window))
  :config
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-dired-jump)
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-find-library)
  (all-the-icons-ivy-setup))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper-isearch)
         ("M-." . swiper-isearch-thing-at-point)
         :map swiper-map ("M-%" . swiper-query-replace))
  :config
  (setq ivy-display-style 'fancy)
  (setq swiper-use-visual-line nil)
  (setq swiper-use-visual-line-p (lambda (a) nil)))

(defun bjm-swiper-recenter (&rest args)
  "recenter display after swiper"
  (recenter))
(advice-add 'swiper :after #'bjm-swiper-recenter)

(use-package all-the-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; (use-package tree-sitter
;;   :ensure t
;;   :config
;;   (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (use-package tree-sitter-langs
;;   :ensure t)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold nil
        doom-themes-enable-italic nil)
  (load-theme 'doom-palenight t)
  (doom-themes-org-config)
  ;; (doom-themes-visual-bell-config)
  ;; (doom-themes-neotree-config)
  (doom-themes-treemacs-config)
  )

(use-package projectile
  :ensure t
  :defer 0.5
  :preface
  (defun my/projectile-compilation-buffers (&optional project)
    (let* ((project-root (or project (projectile-project-root)))
           (buffer-list (mapcar #'process-buffer compilation-in-progress))
           (all-buffers (cl-remove-if-not
                         (lambda (buffer)
                           (projectile-project-buffer-p buffer project-root))
                         buffer-list)))
      (if projectile-buffers-filter-function
          (funcall projectile-buffers-filter-function all-buffers)
        all-buffers)))
  :custom
  (projectile-enable-caching t)
  (projectile-completion-system 'ivy)
  (projectile-mode-line '(:eval (projectile-project-name)))
  :config
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-completion-system 'ivy)
  (add-hook 'prog-mode-hook 'projectile-mode)
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it)))

;; (use-package counsel-projectile
;;   :ensure t
;;   :after (counsel projectile)
;;   :config
;;   (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;;   (counsel-projectile-mode 1))

(use-package avy
  :ensure t
  :bind ("M-'" . avy-goto-char-2))

(use-package bm
  :ensure t
  :bind (("C-c =" . bm-toggle)
         ("C-c [" . bm-previous)
         ("C-c ]" . bm-next)))

;; (use-package forge
;;   :ensure t
;;   :defer t
;;   :after magit
;;   :config
;;   (add-to-list 'forge-alist '("git.private.domain.com" "git.private.domain.com/api/v4" "git.private.domain.com" forge-gitlab-repository))
;;   )

(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-q") 'er/expand-region))

;; ;; Web - Mode

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

  (defun my-web-mode-hook ()
    (setq web-mode-enable-auto-closing t)
    (setq web-mode-enable-auto-quoting t)
    (setq web-mode-css-indent-offset 4)
    (setq web-mode-code-indent-offset 4)
    (setq web-mode-markup-indent-offset 4)
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-enable-current-column-highlight nil))

  (add-hook 'web-mode-hook  'my-web-mode-hook)
  (add-hook 'web-mode-hook 'whitespace-turn-off)
  (eval-after-load "web-mode" '(setq web-mode-enable-auto-expanding t)))

;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil)) ad-do-it) ad-do-it))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;; ;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)
;; (add-hook 'typescript-mode-hook #'setup-tide-mode)

;; (use-package dired-subtree
;;   :ensure t
;;   :after dired
;;   :config
;;   (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
;;   (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

(use-package dired-collapse
  :ensure t
  :after dired
  :config
  (setq dired-collapse-mode t))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package ibuffer-projectile
  :ensure t
  :after ibuffer
  :preface
  (defun my/ibuffer-projectile ()
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  :hook (ibuffer . my/ibuffer-projectile))

;; ;; ;; Kill Ring
;; (setq kill-ring-max 100)

;; ;; (use-package popup-kill-ring
;; ;;   :ensure t
;; ;;   :bind ("M-y" . popup-kill-ring))

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] #'easy-kill)
  (global-set-key [remap mark-sexp] #'easy-mark))

(use-package origami
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'origami-mode)
  ;; (define-key origami-mode-map (kbd "C-c f a") 'origami-toggle-all-nodes)
  ;; http://stackoverflow.com/questions/916797/emacs-global-set-key-to-c-tab
  (define-key origami-mode-map (kbd "C-;") 'origami-recursively-toggle-node))

;; ;; Indent buffer
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
(global-set-key (kbd "C-c n") 'iwb)

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  :bind (("C-x /" . undo-tree-visualize)))

(use-package posframe
  :ensure t)

(use-package s
  :ensure t)

;; (use-package hydra
;;   :ensure t)

(use-package carbon-now-sh
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'lisp-mode-hook
            (lambda()
              (rainbow-delimiters-mode)
              ))
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; ;; REST Support
(use-package restclient
  :ensure t
  :config
  (setq-default restclient-inhibit-cookies t)
  (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))
  )

(use-package less-css-mode
  :ensure t)

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'nxml-mode-hook 'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
  )

;; (use-package dockerfile-mode
;;   :ensure t)

(use-package f
  :ensure t)

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  )

(define-key mc/keymap (kbd "<return>") nil)

(defun move-line-or-region-down (&optional beg end)
  "Move the current line or active region down."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list nil nil)))
  (if beg
      (move-region-vertically beg end 1)
    (move-line-vertically 1)))
(defun move-line-or-region-up (&optional beg end)
  "Move the current line or active region down."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list nil nil)))
  (if beg
      (move-region-vertically beg end -1)
    (move-line-vertically -1)))
(global-set-key (kbd "M-<up>") 'move-line-or-region-up)
(global-set-key (kbd "M-<down>") 'move-line-or-region-down)

(defun move-line-vertically (dir)
  (let* ((beg (point-at-bol))
         (end (point-at-bol 2))
         (col (current-column))
         (region (delete-and-extract-region beg end)))
    (forward-line dir)
    (save-excursion
      (insert region))
    (goto-char (+ (point) col))))

(defun move-region-vertically (beg end dir)
  (let* ((point-before-mark (< (point) (mark)))
         (beg (save-excursion
                (goto-char beg)
                (point-at-bol)))
         (end (save-excursion
                (goto-char end)
                (if (bolp)
                    (point)
                  (point-at-bol 2))))
         (region (delete-and-extract-region beg end)))
    (goto-char beg)
    (forward-line dir)
    (save-excursion
      (insert region))
    (if point-before-mark
        (set-mark (+ (point)
                     (length region)))
      (set-mark (point))
      (goto-char (+ (point)
                    (length region))))
    (setq deactivate-mark nil)))


;; ;; -----------------------------------------------------------------
;; ;; Shift the selected region right if distance is positive, left if
;; ;; negative

(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      (setq deactivate-mark nil))))
(defun shift-right ()
  (interactive)
  (shift-region tab-width))
(defun shift-left ()
  (interactive)
  (shift-region (- tab-width)))
(global-set-key (kbd "C-S-<right>") 'shift-right)
(global-set-key (kbd "C-S-<left>") 'shift-left)

(defun open-and-indent-line-below ()
  "Open a line below the current one, move there, and indent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
(defun open-and-indent-line-above ()
  "Open a line above the current one, move there, and indent."
  (interactive)
  (move-beginning-of-line 1)
  (save-excursion
    (insert "\n"))
  (indent-according-to-mode))
(global-set-key [C-return] 'open-and-indent-line-below)
(global-set-key [C-S-return] 'open-and-indent-line-above)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'whitespace-cleanup)
(setq mode-require-final-newline t)

;; (setq explicit-shell-file-name "/bin/bash")
(setq explicit-shell-file-name "/bin/zsh")

(use-package vterm
  :ensure t)

;; ;; Resize windows
(global-set-key (kbd "C-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-<down>") 'shrink-window)
(global-set-key (kbd "C-s-<up>") 'enlarge-window)

(use-package page-break-lines
  :ensure t)

;; (use-package dashboard
;;   :ensure t
;;   :config
;;   (dashboard-setup-startup-hook)

;;   (turn-on-page-break-lines-mode)

;;   ;; (setq dashboard-banner-logo-title "Hey there sexy!")
;;   (setq dashboard-startup-banner 3)
;;   (setq dashboard-center-content t)
;;   (setq dashboard-show-shortcuts nil)
;;   (setq dashboard-set-heading-icons t)
;;   (setq dashboard-set-file-icons t)
;;   (setq dashboard-set-init-info t)
;;   (setq dashboard-items '((recents  . 5)
;;                           (bookmarks . 5)
;;                           (projects . 5)
;;                           (agenda . 5)
;;                           (registers . 5))))

(use-package rust-mode
  :ensure t
  :config
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil)))
  (setq rust-format-on-save t)
  )

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))

(defun my-shell ()
  (interactive)
  (let ((default-directory "/ssh:user@host:"))
    (shell)))

(set-face-italic-p 'italic nil)
(set-face-bold-p 'bold nil)

(customize-set-variable 'tramp-verbose 6 "Enable remote command traces")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window ivy-switch-buffer))
 '(package-selected-packages
   '(ripgrep pyenv-mode pyenv ibuffer-projectile docker apheleia beacon python-black blacken company-jedi mic-paren tree-sitter tree-sitter-langs evil vterm dracula-theme origami zen-mode prettier py-isort ivy-rich ivy-filthy-rich quelpa-use-package quelpa dired+ dashboard expand-region fill-column-indicator yasnippet-snippets xclip which-key web-mode use-package undo-tree try tide smartparens rust-mode rg restclient rainbow-delimiters pyimpsort posframe org-bullets nord-theme neotree multiple-cursors minions markdown-mode magit js2-mode hungry-delete hl-todo emmet-mode elpy easy-kill doom-themes doom-modeline dired-collapse diminish csv-mode counsel-projectile carbon-now-sh bm all-the-icons-ivy all-the-icons-dired ag ace-window))
 '(tramp-verbose 6))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))

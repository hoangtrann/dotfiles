;;; -*- mode: lisp -*-

(require 'package)

;; Setting up the packages host

(defvar gnu '("gnu" . "http://elpa.gnu.org/packages/"))
(defvar melpa '("melpa" . "https://melpa.org/packages/"))
(defvar melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))
(defvar org-elpa '("org" . "http://orgmode.org/elpa/"))

(setq package-archives nil)
(add-to-list 'package-archives melpa-stable t)
(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives gnu t)
(add-to-list 'package-archives org-elpa t)
;; (package-initialize)

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
   (cons 'use-package melpa)
   ))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; Interface Tweaks

;; Disable start-up message
(setq inhibit-startup-message t)
(setq initial-major-mode 'org-mode)

;; Disable menu bar, tool bar, scroll bar
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(setq ring-bell-function 'ignore)
(setq auto-window-vscroll nil)
(setq inhibit-compacting-font-caches t)
(setq byte-compile-warnings '(cl-functions))

;; (setq-default indicate-empty-lines t)
;; (progn
;;   (define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
;;   (setcdr (assq 'empty-line fringe-indicator-alist) 'tilde))

;; Cursor
(blink-cursor-mode 0)
(setq cursor-in-non-selected-windows t)

;; Enable Transient mark mode
(transient-mark-mode 1)

;; Use highlight on curent line
;; (when window-system (add-hook 'prog-mode-hook 'hl-line-mode))
;; (add-hook 'prog-mode-hook 'hl-line-mode)
;; (add-hook 'prog-mode-hook 'turn-on-visual-line-mode)
;; (add-hook 'prog-mode-hook 'visual-line-mode)

;; (global-visual-line-mode t)
(global-hl-line-mode +1)
;; (setq jit-lock-defer-time 0)
;; (setq fast-but-imprecise-scrolling t)

;; Disable backups and auto-saves
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Change yes-no to y-n
(fset 'yes-or-no-p 'y-or-n-p)

;; Treat camelCase as sepecrate words
;; (global-subword-mode 1)
(setq frame-title-format '("Emacs"))
(setq column-number-mode t)

(setq dired-listing-switches "-aBhl  --group-directories-first")

;; (use-package fill-column-indicator
;;   :ensure t
;;   :config
;;   (setq fci-rule-column 88)
;;   (add-hook 'python-mode-hook #'fci-mode)
;;   (add-hook 'prog-mode-hook #'fci-mode))

(show-paren-mode)
;; (setq org-startup-truncated nil)
;; (setq-default display-line-numbers-type 'visual
;;               display-line-numbers-current-absolute t
;;               display-line-numbers-width 2
;;               display-line-numbers-widen t)

(global-display-line-numbers-mode)
;; (setq display-line-numbers-type "relative")

(global-set-key (kbd "C-S-l") 'display-line-numbers-mode)
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-q") 'kill-this-buffer)
(global-set-key (kbd "C-S-f") 'rg)
(global-set-key (kbd "C-S-k") 'kill-whole-line)

;; ;; Never use tabs, use spaces instead
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default js-indent-level 4)
(setq-default css-indent-offset 4)
(setq-default c-basic-offset 4)

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

;; (autoload 'View-scroll-half-page-forward "view")
;; (autoload 'View-scroll-half-page-backward "view")

;; (global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
;; (global-set-key (kbd "M-v") 'View-scroll-half-page-backward)

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
;; (put 'dired-find-alternate-file 'disabled nil) ; visiting a file from dired closes the dired buffer

;; Quickly edit .emacs
(defun config-visit ()
  (interactive)
  (find-file "~/.emacs"))
(global-set-key (kbd "C-c e") 'config-visit)

(use-package csv-mode
  :ensure t)

;; ;; posframe
(use-package posframe
  :ensure t)

;; ;; which key - bring up some help
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (which-key-mode t)
  (setq which-key-idle-delay 0.5)
  (setq which-key-idle-secondary-delay 0.3))

;; ;; Ace windows
(use-package ace-window
  :ensure t
  :init
  (progn
    (setq aw-scope 'global) ;; was frame
    (ace-window-display-mode)
    (global-set-key (kbd "C-x O") 'other-frame)
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0)))))
    )
  :config
  (global-set-key (kbd "M-o") 'ace-window))

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
  :config
  (global-set-key [f8] 'neotree-toggle))

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
;;         ([f8]        . treemacs)
;;         ([f9]        . treemacs-projectile-toggle)
;;         ("M-0"       . treemacs-select-window)
;;         ("C-c 1"     . treemacs-delete-other-windows)
;;         ))

;; (use-package treemacs-projectile
;;   :defer t
;;   :ensure t
;;   :config
;;   (setq treemacs-header-function #'treemacs-projectile-create-header)
;;   )

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-palenight t)
  (doom-themes-org-config)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config))

;; ;; To nyan or not nyan
;; (use-package nyan-mode
;;   :ensure t
;;   :config
;;   (nyan-mode)
;;   (nyan-start-animation))

(use-package doom-modeline
  :ensure t
  :hook
  (after-init . doom-modeline-mode)
  :config
  ;; (setq doom-modeline-height 25)
  ;; (setq doom-modeline-bar-width 3)
  (setq doom-modeline-window-width-limit fill-column)
  (setq doom-modeline-project-detection 'project)
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  (setq doom-modeline-icon (display-graphic-p))
  (setq find-file-visit-truename t)
  (setq doom-modeline-major-mode-icon t)
  ;; (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-indent-info t)
  (setq doom-modeline-checker-simple-format t)
  (setq doom-modeline-vcs-max-length 12)
  (setq doom-modeline-env-version t)
  (setq doom-modeline-env-enable-python t)
  (setq doom-modeline-minor-modes nil)
  )

(use-package minions
  :ensure t
  :config
  (minions-mode 1))

;; ;; List of recently used families:
;; ;; DejaVu Sans Mono
;; ;; Hack
;; ;; Monaco
;; ;; Fantasque Sans Mono
;; ;; (set-face-attribute 'default nil
;; ;;                     :family "Monaco"
;; ;;                     :height 105)

(set-face-attribute 'default nil :font "Mononoki Nerd Font-15")
(set-frame-font "Mononoki Nerd Font-15" nil t)

;; (setq-default python-indent-guess-indent-offset-verbose nil)
(setq-default py-python-command "python3")
(setq-default python-shell-interpreter "python3")

(use-package flycheck
  :ensure t
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; ;; Diminish modes
;; (use-package diminish
;;   :ensure t)

;; (use-package moody
;;   :ensure t
;;   :config
;;   (setq x-underline-at-descent-line t)
;;   (moody-replace-mode-line-buffer-identification)
;;   (moody-replace-vc-mode))

;; (use-package ag
;;   :ensure t)

(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings))

(use-package yasnippet
  :ensure t
  :diminish yas
  :config
  (use-package yasnippet-snippets
    :ensure t)
  ;; (setq yas-snippet-dirs (append yas-snippet-dirs
  ;;                                '("~/Dropbox/snippets/")))
  (yas-global-mode 1))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.3)
  (setq company-global-modes '(not org-mode markdown-mode))
  (setq company-minimum-prefix-length 3)
  (add-hook 'after-init-hook 'global-company-mode)
  )

(use-package js2-mode
  :ensure t
  :config
  ;; (add-hook 'js-mode-hook 'js2-minor-mode)
  (add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (add-hook 'js2-mode-hook
          (defun my-js2-mode-setup ()
            (flycheck-mode t)
            (when (executable-find "eslint")
              (flycheck-select-checker 'javascript-eslint))))
  )

;; ;; use web-mode for .jsx files
;; (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))


;; (use-package company-tern
;;   :ensure t
;;   :config
;;   (add-to-list 'company-backends 'company-tern)
;;   (add-hook 'js2-mode-hook (lambda ()
;;                              (tern-mode)
;;                              (company-mode))))

(add-hook 'python-mode-hook
          (lambda ()
            (setq flycheck-checker 'python-flake8
                  flycheck-checker-error-threshold 400)))

(use-package pyimpsort
  :ensure t)

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (elpy-enable)
  (setq eldoc-idle-delay 1)
  (setq elpy-rpc-python-command "python3")
  (add-hook 'elpy-mode-hook (lambda ()
                              (highlight-indentation-mode -1)
                              (flycheck-mode)
                              (setq python-check-command "flake8")))
  (remove-hook 'elpy-modules 'elpy-module-flymake))

;; ;; Swiper/Ivy/Counsel
(use-package counsel
  :ensure t
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind
  ("C-x b" . ivy-switch-buffer)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy))

(use-package swiper
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-display-style 'fancy)
  (setq swiper-action-recenter t)
  (global-set-key (kbd "C-s") 'swiper-isearch)
  (global-set-key (kbd "C-r") 'swiper-isearch)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c f") 'counsel-recentf)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c r") 'counsel-rg)
  (global-set-key (kbd "C-c a") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    )
  )

(setq counsel-grep-base-command
      "rg -i -M 120 --no-heading --line-number --color never %s %s")

;; (use-package ivy-hydra
;;   :ensure t)

(use-package all-the-icons
  :ensure t
  :defer 0.5)

(use-package all-the-icons-ivy
  :ensure t
  :after (all-the-icons ivy)
  :custom (all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window ivy-switch-buffer))
  :config
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-dired-jump)
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-find-library)
  (all-the-icons-ivy-setup))


(use-package all-the-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package projectile
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'projectile-mode)
  (projectile-mode +1)
  (defadvice projectile-project-root (around ignore-remote first activate)
  (unless (file-remote-p default-directory) ad-do-it)))

(use-package counsel-projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (counsel-projectile-mode))

;; Modify action of counsel-projectile to open project in dired
(defun my/counsel-projectile-switch-project-action-dired (project)
  "Open ‘dired’ at the root of the project."
  (let ((projectile-switch-project-action
         (lambda ()
           (projectile-dired))))
    (counsel-projectile-switch-project-by-name project)))

(counsel-projectile-modify-action
 'counsel-projectile-switch-project-action
 '((add ("." my/counsel-projectile-switch-project-action-dired
         "open ‘dired’ at the root of the project")
        1)))

;; ;; Modify action of counsel-projectile to open project in magit
;; ;; (counsel-projectile-modify-action
;; ;;  'counsel-projectile-switch-project-action
;; ;;  '((move counsel-projectile-switch-project-action-vc 1)
;; ;;    (setkey counsel-projectile-switch-project-action-vc "o")
;; ;;    (setkey counsel-projectile-switch-project-action " ")))

(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-char-2))

(use-package bm
  :ensure t
  :bind (("C-c =" . bm-toggle)
         ("C-c [" . bm-previous)
         ("C-c ]" . bm-next)))

;; ;; Magit - Magic
(use-package magit
  :ensure t
  :config
  (add-hook 'magit-process-find-password-functions
            'magit-process-password-auth-source)
  :bind (("C-x g" . magit-status)))

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

;; (setq save-interprogram-paste-before-kill t)

;; (defun narrow-or-widen-dwim (p)
;;   "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
;; Intelligently means: region, org-src-block, org-subtree, or defun,
;; whichever applies first.
;; Narrowing to org-src-block actually calls `org-edit-src-code'.

;; With prefix P, don't widen, just narrow even if buffer is already
;; narrowed."
;;   (interactive "P")
;;   (declare (interactive-only))
;;   (cond ((and (buffer-narrowed-p) (not p)) (widen))
;;         ((region-active-p)
;;          (narrow-to-region (region-beginning) (region-end)))
;;         ((derived-mode-p 'org-mode)
;;          ;; `org-edit-src-code' is not a real narrowing command.
;;          ;; Remove this first conditional if you don't want it.
;;          (cond ((ignore-errors (org-edit-src-code))
;;                 (delete-other-windows))
;;                ((org-at-block-p)
;;                 (org-narrow-to-block))
;;                (t (org-narrow-to-subtree))))
;;         (t (narrow-to-defun))))

;; ;; (define-key endless/toggle-map "n" #'narrow-or-widen-dwim)
;; ;; This line actually replaces Emacs' entire narrowing keymap, that's
;; ;; how much I like this command. Only copy it if that's what you want.
;; (define-key ctl-x-map "n" #'narrow-or-widen-dwim)

;; ;; Web - Mode
(use-package web-mode
  :ensure t
)
;; (use-package web-mode
;;   :ensure t
;;   :config

  ;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.xml?\\'" . web-mode))

  ;; (defun my-web-mode-hook ()
  ;;   (setq web-mode-enable-auto-closing t)
  ;;   (setq web-mode-enable-auto-quoting t)
  ;;   (setq web-mode-css-indent-offset 4)
  ;;   (setq web-mode-code-indent-offset 4)
  ;;   (setq web-mode-markup-indent-offset 4)
  ;;   (setq web-mode-enable-current-element-highlight t)
  ;;   (setq web-mode-enable-current-column-highlight nil))

  ;; (add-hook 'web-mode-hook  'my-web-mode-hook)
  ;; (add-hook 'web-mode-hook 'whitespace-turn-off)
  ;; (eval-after-load "web-mode"
  ;;   '(setq web-mode-enable-auto-expanding t))
  ;; )

;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
;; (defadvice web-mode-highlight-part (around tweak-jsx activate)
;;   (if (equal web-mode-content-type "jsx")
;;       (let ((web-mode-enable-part-face nil))
;;         ad-do-it)
;;     ad-do-it))

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
  (company-mode +1)
  ;; (setq-default typescript-indent-level 2)
  ;; (setq tide-format-options '(:indentSize 2 :tabSize 2 :insertSpaceAfterOpeningAndBeforeClosingTemplateStringBraces nil :placeOpenBraceOnNewLineForFunctions nil :placeOpenBraceOnNewLineForControlBlocks nil))
  )

;; (setq company-tooltip-align-annotations t)

;; ;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; (setq dired-dwim-target t)
;; (use-package dired-narrow
;;   :ensure t
;;   :config
;;   (bind-key "C-c C-n" #'dired-narrow)
;;   (bind-key "C-c C-f" #'dired-narrow-fuzzy)
;;   (bind-key "C-x C-N" #'dired-narrow-regexp)
;;   )

;; (use-package dired-subtree
;;   :ensure t
;;   :after dired
;;   :config
;;   (bind-key "<tab>" #'dired-subtree-toggle dired-mode-map)
;;   (bind-key "<backtab>" #'dired-subtree-cycle dired-mode-map))

;; ;; (use-package dumb-jump
;; ;;   :bind (("M-g o" . dumb-jump-go-other-window)
;; ;;          ("M-g j" . dumb-jump-go)
;; ;;          ("M-g x" . dumb-jump-go-prefer-external)
;; ;;          ("M-g z" . dumb-jump-go-prefer-external-other-window))
;; ;;   :config
;; ;;   ;; (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
;; ;;   :init
;; ;;   (dumb-jump-mode)
;; ;;   :ensure
;; ;;   )

(use-package dired-collapse
  :ensure t
  :after dired
  :config
  (setq dired-collapse-mode t))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("org" (name . "^.*org$"))
               ("magit" (mode . magit-mode))
               ("IRC" (or (mode . circe-channel-mode) (mode . circe-server-mode)))
               ("web" (or (mode . web-mode) (mode . js2-mode)))
               ("shell" (or (mode . eshell-mode) (mode . shell-mode)))
               ("mu4e" (or

                        (mode . mu4e-compose-mode)
                        (name . "\*mu4e\*")
                        ))
               ("programming" (or
                               (mode . clojure-mode)
                               (mode . clojurescript-mode)
                               (mode . python-mode)
                               (mode . c++-mode)))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))
               ))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))

;; don't show these
;; (add-to-list 'ibuffer-never-show-predicates "zowie")
;; Don't show filter groups if there are no buffers in that group
(setq ibuffer-show-empty-filter-groups nil)
;; Don't ask for confirmation to delete marked buffers
(setq ibuffer-expert t)

;; ;; ;; Kill Ring
;; (setq kill-ring-max 100)

;; ;; (use-package popup-kill-ring
;; ;;   :ensure t
;; ;;   :bind ("M-y" . popup-kill-ring))

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] #'easy-kill)
  (global-set-key [remap mark-sexp] #'easy-mark)
  )

(use-package hideshow
  :ensure t
  :bind (("C-M->" . my-toggle-hideshow-all)
         ("C-M-<" . hs-hide-level)
         ("C-;" . hs-toggle-hiding)
         ;; ([C-tab] . hs-toggle-hiding)
         )
  :config
  ;; Hide the comments too when you do a 'hs-hide-all'
  (setq hs-hide-comments nil)
  ;; Set whether isearch opens folded comments, code, or both
  ;; where x is code, comments, t (both), or nil (neither)
  (setq hs-isearch-open t)
  ;; Add more here

  (setq hs-set-up-overlay
        (defun my-display-code-line-counts (ov)
          (when (eq 'code (overlay-get ov 'hs))
            (overlay-put ov 'display
                         (propertize
                          (format " ... <%d>"
                                  (count-lines (overlay-start ov)
                                               (overlay-end ov)))
                          'face 'font-lock-type-face)))))

  (defvar my-hs-hide nil "Current state of hideshow for toggling all.")
       ;;;###autoload
  (defun my-toggle-hideshow-all () "Toggle hideshow all."
         (interactive)
         (setq my-hs-hide (not my-hs-hide))
         (if my-hs-hide
             (hs-hide-all)
           (hs-show-all)))

  (add-hook 'prog-mode-hook #'hs-minor-mode)
  )

(use-package sgml-mode
  :ensure t)

(add-to-list 'hs-special-modes-alist
             (list 'nxml-mode
                   "<!--\\|<[^/>]*[^/]>"
                   "-->\\|</[^/>]*[^/]>"
                   "<!--"
                   'nxml-forward-element
                   nil))

;; Fix HTML folding
(dolist (mode '(sgml-mode
                html-mode
                html-erb-mode))
  (add-to-list 'hs-special-modes-alist
               (list mode
                     "<!--\\|<[^/>]*[^/]>"
                     "-->\\|</[^/>]*[^/]>"
                     "<!--"
                     'sgml-skip-tag-forward
                     nil)))

;; (defun ian/newline-and-push-brace ()
;;   "`newline-and-indent', but bracket aware."
;;   (interactive)
;;   (insert "\n")
;;   (when (looking-at "}")
;;     (insert "\n")
;;     (indent-according-to-mode)
;;     (forward-line -1))
;;   (indent-according-to-mode))

;; (global-set-key (kbd "RET") 'ian/newline-and-push-brace)

;; ;; NXML mode
;; (setq x-select-enable-clipboard t)
;; (add-hook 'nxml-mode-hook 'hs-minor-mode)
;; (add-hook 'nxml-mode-hook (lambda ()
;;                             (global-set-key [C-tab] 'completion-at-point)))
;; (setq rng-nxml-auto-validate-flag nil)

;; ;; SGML Mode
;; ;; (defalias 'xml-mode 'sgml-mode
;; ;;   "Use `sgml-mode' instead of nXML's `xml-mode'.")
;; ;; (setq-default sgml-basic-offset 4)

;; ;; Indent buffer
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(global-set-key (kbd "C-c n") 'iwb)

;; (use-package hl-todo
;;   :ensure t
;;   :config
;;   (global-hl-todo-mode))

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

;; (use-package s
;;   :ensure t)

;; (use-package hydra
;;   :ensure t)

(use-package carbon-now-sh
  :ensure t)

;; ;; (use-package highlight-indent-guides
;; ;;   :ensure t
;; ;;   :config
;; ;;   (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;; ;;   (setq highlight-indent-guides-method 'character))

;; ;; (use-package highlight-parentheses
;; ;;   :ensure t
;; ;;   :diminish highlight-parentheses-mode
;; ;;   :config
;; ;;   (add-hook 'emacs-lisp-mode-hook
;; ;;             (lambda()
;; ;;               (highlight-parentheses-mode)
;; ;;               )))

;; ;; (global-highlight-parentheses-mode)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'lisp-mode-hook
            (lambda()
              (rainbow-delimiters-mode)
              ))
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode)
  ;; (add-hook 'web-mode-hook #'turn-on-smartparens-mode t)
  )

;; ;; (add-hook 'before-save-hook #'gofmt-before-save)

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

(use-package org
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (add-hook 'org-mode-hook (lambda () (setq org-bullets-mode 1)))
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'org-indent-mode)
  )

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode))

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

;; ;; (setq explicit-shell-file-name "/bin/bash")
(setq explicit-shell-file-name "/bin/zsh")

;; ;; Resize windows
(global-set-key (kbd "C-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-<down>") 'shrink-window)
(global-set-key (kbd "C-s-<up>") 'enlarge-window)

;; (use-package dashboard
;;   :ensure t
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (setq dashboard-banner-logo-title "Hey there sexy!")
;;   (setq dashboard-startup-banner 'logo)
;;   (setq dashboard-center-content t)
;;   (setq dashboard-show-shortcuts nil)
;;   (setq dashboard-set-heading-icons t)
;;   (setq dashboard-set-file-icons t)
;;   (setq dashboard-set-init-info t)
;;   (setq dashboard-items '((recents  . 5)
;;                           (projects . 10)
;;                           (bookmarks . 5)))
;;   )

(use-package rust-mode
  :ensure t
  :config
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil)))
  (setq rust-format-on-save t)
  )

(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))

;; ;; ################################################

;; ;; Display time
;; (setq display-time-24hr-format t)
;; (setq display-time-format "%H:%M - %d %B %Y")
;; (setq display-time-format "%H:%M")
;; (display-time-mode 1)

(customize-set-variable 'tramp-verbose 6 "Enable remote command traces")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-ivy-buffer-commands (quote (ivy-switch-buffer-other-window ivy-switch-buffer)))
 '(package-selected-packages
   (quote
    (expand-region fill-column-indicator yasnippet-snippets xclip which-key web-mode use-package undo-tree try tide smartparens rust-mode rg restclient rainbow-delimiters pyimpsort posframe org-bullets nord-theme neotree multiple-cursors minions markdown-mode magit js2-mode hungry-delete hl-todo emmet-mode elpy easy-kill doom-themes doom-modeline dired-collapse diminish csv-mode counsel-projectile carbon-now-sh bm beacon all-the-icons-ivy all-the-icons-dired ag ace-window)))
 '(tramp-verbose 6))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))

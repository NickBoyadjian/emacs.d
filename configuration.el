(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq visible-bell 1
      ring-bell-function 'ignore)

;; Line numbers
(set-fringe-mode '(0 . nil))
(setq display-line-numbers-width-start 1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(when (display-graphic-p)
;; No title. See init.el for initial value.
(setq-default frame-title-format nil)
;; Hide the cursor in inactive windows.
(setq cursor-in-non-selected-windows nil)
(setq-default cursor-type 'bar)
;; Avoid native dialogs.
(setq use-dialog-box nil))
;; Make the title bar look good
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(global-eldoc-mode -1)
;; Set the font
(set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 140)

(setq warning-minimum-level :error)

(setopt default-process-coding-system '(utf-8-unix . utf-8-unix)
      locale-coding-system 'utf-8)

(set-charset-priority 'unicode)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25
        doom-modeline-buffer-file-name-style 'file-name
        doom-modeline-lsp-icon t))

(use-package hide-mode-line
  :config
  (add-hook 'treemacs-mode-hook #'hide-mode-line-mode)
  (add-hook 'vterm-mode-hook #'hide-mode-line-mode)
  (add-hook 'magit-mode-hook #'hide-mode-line-mode))

(when (display-graphic-p)
  ;; No title. See init.el for initial value.
  (setq-default frame-title-format nil)
  ;; Hide the cursor in inactive windows.
  (setq cursor-in-non-selected-windows nil)
  ;; Avoid native dialogs.
  (setq use-dialog-box nil))

;; Set font face height. Value is 1/10pt.
(set-face-attribute 'default nil
		    :height 140)

;; Don't use continuation character.
(setq-default fringe-indicator-alist (delq (assq 'continuation fringe-indicator-alist) fringe-indicator-alist))

(use-package frame
  :ensure nil
  :defer
  :init
  ;; Mispressing C-z or C-x C-z invokes `suspend-frame' (disable).
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z"))
  :config
  ;; Enable expanding frame to end of screen.
  (setq frame-resize-pixelwise t)
  ;; Remove thin border. Visible since Monterey.
  (set-frame-parameter nil 'internal-border-width 0)
  (set-frame-position (selected-frame) 15 53))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(defun nb/tab-name-format (tab i)
    "Return the tab name with different font colors for active and inactive tabs."
    (set-face-attribute 'tab-bar nil :background "#282a36")
    (let* ((current-tab (eq (car tab) 'current-tab))
           (name (alist-get 'name tab))
           (separator " ")
           ;; Define face for active and inactive tabs
           (active-name-face '(:foreground "#C792EA" :background "#282a36" :height 1))
           (inactive-name-face '(:foreground "#FFFFFF" :background "#282a36" :height 1))
           (separator-face '(:foreground "#FFFFFF")))
      (concat
       ;; Apply different faces based on whether the tab is active
       (propertize name 'face (if current-tab active-name-face inactive-name-face))
       (propertize separator 'face separator-face))))


(setq tab-bar-tab-name-format-function #'nb/tab-name-format
      tab-bar-tab-hints t
      tab-bar-show t
      tab-bar-position 'top
      tab-bar-close-button-show nil
      tab-bar-new-button-show nil
      tab-bar-auto-width nil)

(use-package dashboard
  :custom
  (dashboard-projects-backend 'project-el)
  (dashboard-items '((recents  . 5)
                     (projects . 5)
                     (bookmarks . 5)
                     (agenda . 5)))
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 3
        ;; dashboard-icon-type 'all-the-icons
        dashboard-items '((recents   . 5)
                          (projects  . 5)
                          (agenda    . 5))))

(use-package treesit
      :ensure nil
      :mode (("\\.tsx\\'" . tsx-ts-mode)
             ("\\.js\\'"  . typescript-ts-mode)
             ("\\.mjs\\'" . typescript-ts-mode)
             ("\\.mts\\'" . typescript-ts-mode)
             ("\\.cjs\\'" . typescript-ts-mode)
             ("\\.ts\\'"  . typescript-ts-mode)
             ("\\.jsx\\'" . tsx-ts-mode)
             ("\\.json\\'" .  json-ts-mode)
             ("\\.Dockerfile\\'" . dockerfile-ts-mode)
             ("\\.prisma\\'" . prisma-ts-mode)
             ;; More modes defined here...
             )
      :preface
      (defun os/setup-install-grammars ()
        "Install Tree-sitter grammars if they are absent."
        (interactive)
        (dolist (grammar
                 '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
                   (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
                   (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
                   (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                   (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
                   (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
                   (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
          (add-to-list 'treesit-language-source-alist grammar)
          ;; Only install `grammar' if we don't already have it
          ;; installed. However, if you want to *update* a grammar then
          ;; this obviously prevents that from happening.
          (unless (treesit-language-available-p (car grammar))
            (treesit-install-language-grammar (car grammar)))))

      ;; Optional, but recommended. Tree-sitter enabled major modes are
      ;; distinct from their ordinary counterparts.
      ;;
      ;; You can remap major modes with `major-mode-remap-alist'. Note
      ;; that this does *not* extend to hooks! Make sure you migrate them
      ;; also
      (dolist (mapping
               '((css-mode . css-ts-mode)
                 (typescript-mode . typescript-ts-mode)
                 (js-mode . typescript-ts-mode)
                 (js2-mode . typescript-ts-mode)
                 (css-mode . css-ts-mode)
                 (json-mode . json-ts-mode)
                 (js-json-mode . json-ts-mode)))
        (add-to-list 'major-mode-remap-alist mapping))
      :config
      (os/setup-install-grammars))

(use-package eglot
  :ensure nil
  :bind
  (:map eglot-mode-map
  ("C-c e a" . eglot-code-actions)
  ("C-c e f" . eglot-format)
  ("C-c e r" . eglot-rename)
  ("C-c e R" . eglot-reconnect)
  ("C-c e o" . eglot-code-action-organize-imports)
  ("C-c e D" . eglot-find-declaration)
  ("C-c e i" . eglot-find-implementation)
  ("C-c e d" . eglot-find-typeDefinition)
  ("C-c e h" . eldoc))
  :custom
  (eglot-autoshutdown t)
  :config
  ;; Make eldoc only display one liner in echo area
  (setq eldoc-echo-area-use-multiline-p nil)

  ;; Javascript
  (add-hook 'js2-mode-hook 'eglot-ensure)
  (add-to-list 'eglot-server-programs '((js2-mode) "typescript-language-server" "--stdio"))
  ;; Elixir
  (add-hook 'elixir-mode-hook 'eglot-ensure)
  (add-to-list 'eglot-server-programs '(elixir-mode "~/build/elixir-ls-v0.24.1/language_server.sh")))

(use-package project
  :ensure nil
  :custom ((project-compilation-buffer-name-function
            'project-prefixed-buffer-name))

  :init
  (defun nb/--project-open-file (filename)
    "Open or create the FILENAME file in the current project."
    (unless (project-current)
      (error "File/buffer doesn't make part of an project"))
    (when-let* ((project (project-current))
                (default-directory (expand-file-name (project-root project))))
      (find-file filename)))

  (defun nb/project-api-file ()
    "Open or create the _api.org file in the current project."
    (interactive)
    (nb/--project-open-file "_api.org"))

  :bind (:map project-prefix-map
              ("o a" . nb/project-api-file)
              ("S" . nb/project-save-project-buffers))
  :config

  (defun nb/vterm-in-project ()
    "Invoke `vterm' in the project's root.
Switch to the project specific term buffer if it already exists."
    (interactive)
    (unless (project-current)
      (error "File/buffer doesn't make part of an project"))
    (when-let* ((project (project-current))
                (default-directory (expand-file-name (project-root project)))
                (buffer-name (project-prefixed-buffer-name "vterm")))
      (unless (buffer-live-p (get-buffer buffer-name))
        (unless (require 'vterm nil 'noerror)
          (error "Package 'vterm' is not available"))
        (when (fboundp 'vterm)
          (vterm buffer-name)))
      (pop-to-buffer-same-window buffer-name)))

  (fset 'project-shell 'nb/vterm-in-project))

(use-package ibuffer-projectile
  :config
  (add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-projectile-set-filter-groups)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic)))))

(use-package tabspaces
  :hook (after-init . tabspaces-mode) ;; use this only if you want the minor-mode loaded at startup.
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :bind (
         ("C-x p p" . tabspaces-open-or-create-project-and-workspace)
         ("H-<tab>  o" . tabspaces-open-or-create-project-and-workspace)
         ("H-<tab> TAB" . tabspaces-switch-or-create-workspace)
         ("H-<tab> k" . tabspaces-kill-buffers-close-workspace))
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  ;; (tabspaces-initialize-project-with-todo t)
  ;; (tabspaces-todo-file-name "project-todo.org")

  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-auto-restore t)

  :config
  ;; Filter Buffers for Consult-Buffer
  (with-eval-after-load 'consult
    ;; hide full buffer list (still available with "b" prefix)
    (consult-customize consult--source-buffer :hidden t :default nil)
    ;; set consult-workspace buffer list
    (defvar consult--source-workspace
      (list :name     "Workspace Buffers"
            :narrow   ?w
            :history  'buffer-name-history
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda () (consult--buffer-query
                                  :predicate #'tabspaces--local-buffer-p
                                  :sort 'visibility
                                  :as #'buffer-name)))

      "Set workspace buffer list for consult-buffer.")
    (add-to-list 'consult-buffer-sources 'consult--source-workspace)))

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode))

(use-package prettier
  :config
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (sp-with-modes '(elixir-mode)
    (sp-local-pair "do" "end"
                   :when '(("SPC" "RET"))
                   :post-handlers '(("||\n[i]" "RET"))))
  :bind
  (:map smartparens-mode-map
        ("C-)" . sp-forward-slurp-sexp)
        ("C-(" . sp-forward-barf-sexp)
        ("C-{" . sp-backward-slurp-sexp)
        ("C-}" . sp-backward-barf-sexp))
  :hook   (prog-mode . smartparens-mode))

(defconst NB/IS-MACOS (eq system-type 'darwin))

(when NB/IS-MACOS
  (setopt mac-command-modifier 'meta
	  mac-option-modifier 'hyper))

(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "C-;") 'avy-goto-char))

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package mwim
  :bind ("C-a" . mwim-beginning)
        ("C-e" . mwim-end))

(use-package surround
  :ensure t
  :bind-keymap ("C-c s" . surround-keymap))

(use-package org
  :ensure nil
  :custom
    (org-confirm-babel-evaluate nil))

(use-package org-modern
:ensure t
:init
;; Add frame borders and window dividers
;;
;; WJH 2023-12-05: These are necessary in order to be able to see the
;; indicators for source blocks.  On the other hand, I do not want
;; them as large as in the examples (40 pixels!), so I am using 4
;; instead
(modify-all-frames-parameters
 '((right-divider-width . 4)
   (internal-border-width . 4)))
;; Make things blend in
(dolist (face '(window-divider
		window-divider-first-pixel
		window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
:config
(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t
 org-startup-folded t

 ;; Org styling
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis "…"
 org-adapt-indentation t

 ;; Agenda styling
 org-agenda-tags-column 0
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "◀── now ─────────────────────────────────────────────────")

(global-org-modern-mode)
)

(setq
   org-directory "~/.org/"
   org-startup-folded t)

(setq org-default-notes-file (concat org-directory "notes.org"))

(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-count 20)
  (setq vertico-cycle t))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(setf epa-pinentry-mode 'loopback)

(use-package consult
  :bind  (;; Related to the control commands.
          ("C-c h" . consult-history)
          ("C-c m" . consult-mode-command)
          ("C-c b" . consult-bookmark)
          ("C-c k" . consult-kmacro)
          ;; Navigation
          ("C-x M-:" . consult-complex-command)
          ("C-x b". consult-buffer)
          ("C-x 4 b". consult-buffer-other-window)
          ("C-x 5 b". consult-buffer-other-frame)
          ;; Goto map
          ("M-g e" . consult-compile-error)
          ("M-g g" . consult-goto-line)
          ("M-g M-g" . consult-goto-line)
          ("M-g o" . consult-outline)
          ("M-g m" . consult-mark)
          ("M-g k" . consult-global-mark)
          ("M-g i" . consult-imenu)
          ("M-g I" . consult-imenu-multi)
          ("M-g !" . consult-flymake)

          ("M-s f" . consult-find)
          ("M-s L" . consult-locate)
          ("M-s g" . consult-git-grep)
          ("M-s G" . consult-grep)
          ("M-s r" . consult-ripgrep)
          ("M-s l" . consult-line)
          ("M-s k" . consult-keep-lines)
          ("M-s u" . consult-focus-lines))
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  (consult-narrow-key "<")
  (consult-project-root-function #'projectile-project-root)
  ;; Provides consistent display for both `consult-register' and the register
  ;; preview when editing registers.
  (register-preview-delay 0)
  (register-preview-function #'consult-register-preview))

(use-package marginalia
  :init
  (marginalia-mode 1)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)
              ("M-A" . marginalia-cycle)))

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package elixir-mode
  :ensure t
  :init
  (defun nb/enter-pipe ()
    (interactive)
    (let ((oldpos (point)))
      (end-of-line)
      (newline-and-indent)
      (insert "|> ")))
  :bind (("<C-return>" . nb/enter-pipe)))

  (use-package exunit
    :config
    ;; fix broken dark test link
    (custom-set-faces
     '(ansi-color-black ((t (:background "MediumPurple2" :foreground "MediumPurple2")))))
    :hook
    (elixir-ts-mode . exunit-mode)
    (elixir-mode . exunit-mode))

(use-package compile-credo
    :ensure (podium :type git :url "git@github.com:vinikira/compile-credo.git" :branch "master"))

(setq-default js-indent-level 2)

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config)

(use-package rjsx-mode)

(use-package prettier-js
  :ensure t)

(use-package jest-test-mode
  :ensure t
  :commands jest-test-mode
  :hook (typescript-mode js-mode typescript-tsx-mode))

(use-package gleam-ts-mode
  :mode (rx ".gleam" eos))

(defun +elpaca-unload-seq (e)
  (and (featurep 'seq) (unload-feature 'seq t))
  (elpaca--continue-build e))

;; You could embed this code directly in the reicpe, I just abstracted it into a function.
(defun +elpaca-seq-build-steps ()
  (append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
                       elpaca--pre-built-steps elpaca-build-steps))
          (list '+elpaca-unload-seq 'elpaca--activate-package)))

(use-package seq :ensure `(seq :build ,(+elpaca-seq-build-steps)))

(use-package transient)

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (custom-set-variables
   '(git-gutter:modified-sign "|") ;; two space
   '(git-gutter:added-sign "+")    ;; multiple character is OK
   '(git-gutter:deleted-sign "-")
   '(git-gutter:unchanged "  "))

  (set-face-foreground 'git-gutter:modified "orange")
  (set-face-foreground 'git-gutter:added "green")
  (set-face-foreground 'git-gutter:deleted "red"))

(use-package blamer
  :ensure t
  :bind (("s-i" . blamer-show-commit-info)
         ("C-c i" . blamer-show-posframe-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 140
                    :italic t))))

(use-package which-key
  :ensure t
  :defer 10
  :diminish which-key-mode
  :config
  (which-key-mode 1))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)
    (treemacs-follow-mode t)
    (treemacs-project-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-all-the-icons
  :after (treemacs projectile)
  :ensure t
  :config
  (treemacs-load-theme 'all-the-icons))

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(use-package vterm
  :config
  (setq vterm-max-scrollback 2000))

(setq help-window-select t)

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          ;;vterm-mode
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)

  (defun nb/popup-vterm ()
    "Open vterm as a popup."
    (interactive)
    (popper-toggle)
    (nb/vterm-in-project)))

(use-package switch-window
  :bind ("C-x o" . switch-window)
  :config
  (setq switch-window-shortcut-style 'qwerty))

(keymap-global-set "C-x C-b" 'ibuffer)

(use-package corfu
  :init
  (global-corfu-mode))

(use-package emacs
  :ensure nil
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package verb
  :after org
  :config

  (defun verb-graphql (rs)
  "Transform verb RS to GraphQL request."
  (let* ((before-body (oref rs body))
         (splited-body (split-string before-body "\n\n"))
         (query (nth 0 splited-body))
         (variables (nth 1 splited-body))
         (json-object-type 'alist)
         (parsed-variables (if variables (json-parse-string variables) '()))
         (new-body (json-encode `((query . ,query) (variables . ,parsed-variables)))))
    (oset rs body new-body)
    rs))

  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  (add-to-list 'org-babel-load-languages '(verb . t)))

(use-package ob-async
  :after ob)

(use-package podium
  :ensure (podium :type git :url "git@gitlab-ssh.podium.com:vinicius.simoes/podium.el.git" :branch "master")
  :custom
  (podium-gitlab-oncall-projects
   '("engineering/account-structure/vader"
     "engineering/account-structure/anakin"
     "engineering/account-structure/anakin_client"))
  (podium-gitlab-defaultcodepath "~/podium/"))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

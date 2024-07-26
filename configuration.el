(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq visible-bell 1)
(setq ring-bell-function 'ignore)
(global-display-line-numbers-mode)
(when (display-graphic-p)
;; No title. See init.el for initial value.
(setq-default frame-title-format nil)
;; Hide the cursor in inactive windows.
(setq cursor-in-non-selected-windows nil)
;; Avoid native dialogs.
(setq use-dialog-box nil))
;; Make the title bar look good
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(setopt default-process-coding-system '(utf-8-unix . utf-8-unix)
      locale-coding-system 'utf-8)

(set-charset-priority 'unicode)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

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

(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t)
  (ar/load-material-org-tweaks)
  :init
  (defun ar/load-material-org-present-tweaks ()
    (with-eval-after-load 'frame
      (set-cursor-color "#2BA3FF"))

    (with-eval-after-load 'faces
      (set-face-attribute 'org-level-1 nil :foreground "#ff69b4" :background 'unspecified :box nil)
      (set-face-attribute 'org-level-2 nil :inherit 'lisp-extra-font-lock-quoted :foreground 'unspecified :background 'unspecified :box nil)
      (set-face-attribute 'org-block nil :background "grey11" :box nil)))

  (defun ar/drop-material-org-present-tweaks ()
    (with-eval-after-load 'frame
      (set-cursor-color "royal blue"))

    (with-eval-after-load 'faces
      (set-face-attribute 'org-level-1 nil :foreground 'unspecified :background 'unspecified :box nil)
      (set-face-attribute 'org-level-2 nil :inherit nil :foreground 'unspecified :background 'unspecified :box nil)
      (set-face-attribute 'org-block nil :background 'unspecified :box nil)))

  (defun ar/load-material-org-tweaks ()
    (with-eval-after-load 'frame
      (set-cursor-color "orange"))

    (with-eval-after-load 'faces
      (set-face-attribute 'header-line nil :background "#212121" :foreground "dark grey")
      (set-face-attribute 'internal-border nil :background "#212121")
      ;; From https://gist.github.com/huytd/6b785bdaeb595401d69adc7797e5c22c#file-customized-org-mode-theme-el
      (set-face-attribute 'default nil :stipple nil :background "#212121" :foreground "#eeffff" :inverse-video nil
			  ;; :family "Menlo" ;; or Meslo if unavailable: https://github.com/andreberg/Meslo-Font
			  ;; :family "Hack" ;; brew tap homebrew/cask-fonts && brew cask install font-hack
			  :family "JetBrains Mono" ;; brew tap homebrew/cask-fonts && brew install --cask font-jetbrains-mono
			  ;; :family "mononoki" ;; https://madmalik.github.io/mononoki/ or sudo apt-get install fonts-mononoki
			  :box nil :strike-through nil :overline nil :underline nil :slant 'normal :weight 'normal
			  :width 'normal :foundry "nil")
      ;; Enable rendering SF symbols on macOS.
      (when (memq system-type '(darwin))
	(set-fontset-font t nil "SF Pro Display" nil 'append))

      ;; Emoji's: welcome back to Emacs
      ;; https://github.crookster.org/emacs27-from-homebrew-on-macos-with-emoji/
      (when (>= emacs-major-version 27)
	(set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

      ;; Hardcode region theme color.
      (set-face-attribute 'region nil :background "#3f464c" :foreground "#eeeeec" :underline nil)
      (set-face-attribute 'mode-line nil :background "#191919" :box nil)

      ;; Styling moody https://github.com/tarsius/moody
      (let ((line (face-attribute 'mode-line :underline)))
	(set-face-attribute 'mode-line nil :overline   line)
	(set-face-attribute 'mode-line-inactive nil :overline   line)
	(set-face-attribute 'mode-line-inactive nil :underline  line)
	(set-face-attribute 'mode-line nil :box nil)
	(set-face-attribute 'mode-line-inactive nil :box nil)
	(set-face-attribute 'mode-line-inactive nil :background "#212121" :foreground "#5B6268")))

    (with-eval-after-load 'font-lock
      ;; brew install font-iosevka-aile
      ;; (set-face-attribute 'font-lock-comment-face nil :font "Iosevka Aile")
      (set-face-attribute 'font-lock-comment-face nil :font "JetBrains Mono")
      (set-face-attribute 'font-lock-constant-face nil :foreground "#C792EA")
      (set-face-attribute 'font-lock-keyword-face nil :foreground "#2BA3FF" :slant 'italic)
      (set-face-attribute 'font-lock-preprocessor-face nil :inherit 'bold :foreground "#2BA3FF" :slant 'italic :weight 'normal)
      (set-face-attribute 'font-lock-string-face nil :foreground "#C3E88D")
      (set-face-attribute 'font-lock-type-face nil :foreground "#FFCB6B")
      (set-face-attribute 'font-lock-variable-name-face nil :foreground "#FF5370"))

    (with-eval-after-load 'em-prompt
      (set-face-attribute 'eshell-prompt nil :foreground "#eeffff"))

    (with-eval-after-load 'company
      (set-face-attribute 'company-preview-search nil :foreground "sandy brown" :background 'unspecified)
      (set-face-attribute 'company-preview-common nil :inherit 'default :foreground 'unspecified :background "#212121"))

    (with-eval-after-load 'company-box
      (set-face-attribute 'company-box-candidate  nil :inherit 'default :foreground "#eeffff" :background "#212121" :box nil)
      (set-face-attribute 'company-box-background nil :inherit 'default :background "#212121" :box nil)
      (set-face-attribute 'company-box-annotation nil :inherit 'company-tooltip-annotation :background "#212121" :foreground "dim gray")
      (set-face-attribute 'company-box-selection nil :inherit 'company-tooltip-selection :foreground "sandy brown"))

    (with-eval-after-load 'popup
      (set-face-attribute 'popup-menu-face nil
			  :foreground (face-foreground 'default)
			  :background (face-background 'default))
      (set-face-attribute 'popup-menu-selection-face nil
			  :foreground "sandy brown"
			  :background "dim gray"))

    (with-eval-after-load 'paren
      (set-face-attribute 'show-paren-match nil
			  :background 'unspecified
			  :foreground "#FA009A"))

    (with-eval-after-load 'org-indent
      (set-face-attribute 'org-indent nil :background "#212121"))

    (with-eval-after-load 'org-faces
      (set-face-attribute 'org-hide nil :foreground "#212121" :background "#212121" :strike-through nil)
      (set-face-attribute 'org-done nil :foreground "#b9ccb2" :strike-through nil)
      (set-face-attribute 'org-agenda-date-today nil :foreground "#Fb1d84")
      (set-face-attribute 'org-agenda-done nil :foreground "#b9ccb2" :strike-through nil)
      (set-face-attribute 'org-table nil :background 'unspecified)
      (set-face-attribute 'org-code nil :background 'unspecified)
      (set-face-attribute 'org-level-1 nil :background 'unspecified :box nil)
      (set-face-attribute 'org-level-2 nil :background 'unspecified :box nil)
      (set-face-attribute 'org-level-3 nil :background 'unspecified :box nil)
      (set-face-attribute 'org-level-4 nil :background 'unspecified :box nil)
      (set-face-attribute 'org-level-5 nil :background 'unspecified :box nil)
      (set-face-attribute 'org-level-6 nil :background 'unspecified :box nil)
      (set-face-attribute 'org-level-7 nil :background 'unspecified :box nil)
      (set-face-attribute 'org-level-8 nil :background 'unspecified :box nil)
      (set-face-attribute 'org-block-begin-line nil :background 'unspecified :box nil)
      (set-face-attribute 'org-block-end-line nil :background 'unspecified :box nil)
      (set-face-attribute 'org-block nil :background 'unspecified :box nil))

    (with-eval-after-load 'mu4e-vars
      (set-face-attribute 'mu4e-header-highlight-face nil :inherit 'default :foreground "sandy brown" :weight 'bold :background 'unspecified)
      (set-face-attribute 'mu4e-unread-face nil :inherit 'default :weight 'bold :foreground "#2BA3FF" :underline nil))

    (with-eval-after-load 'comint
      (set-face-attribute 'comint-highlight-input nil
			  :inherit 'default
			  :foreground "sandy brown"
			  :weight 'normal
			  :background 'unspecified))

    ;; No color for fringe, blends with the rest of the window.
    (with-eval-after-load 'fringe
      (set-face-attribute 'fringe nil
			  :foreground (face-foreground 'default)
			  :background (face-background 'default)))

    ;; No color for sp-pair-overlay-face.
    (with-eval-after-load 'smartparens
      (set-face-attribute 'sp-pair-overlay-face nil
			  :foreground (face-foreground 'default)
			  :background (face-background 'default)))

    ;; Remove background so it doesn't look selected with region.
    ;; Make the foreground the same as `diredfl-flag-mark' (ie. orange).
    (with-eval-after-load 'diredfl
      (set-face-attribute 'diredfl-flag-mark-line nil
			  :foreground "orange"
			  :background 'unspecified))

    (with-eval-after-load 'dired-subtree
      (set-face-attribute 'dired-subtree-depth-1-face nil
			  :background 'unspecified)
      (set-face-attribute 'dired-subtree-depth-2-face nil
			  :background 'unspecified)
      (set-face-attribute 'dired-subtree-depth-3-face nil
			  :background 'unspecified)
      (set-face-attribute 'dired-subtree-depth-4-face nil
			  :background 'unspecified)
      (set-face-attribute 'dired-subtree-depth-5-face nil
			  :background 'unspecified)
      (set-face-attribute 'dired-subtree-depth-6-face nil
			  :background 'unspecified))

    ;; Trying out line underline (instead of wave).
    (mapatoms (lambda (atom)
		(let ((underline nil))
		  (when (and (facep atom)
			     (setq underline
				   (face-attribute atom
						   :underline))
			     (eq (plist-get underline :style) 'wave))
		    (plist-put underline :style 'line)
		    (set-face-attribute atom nil
					:underline underline)))))))

;; (use-package lsp-mode
    ;;   :ensure t
    ;;   :config
    ;;   (setq lsp-keymap-prefix "C-c l")
    ;;   (setq lsp-modeline-code-actions-segments '(count icon name))

    ;;   :init
    ;;   '(lsp-mode))

    ;; (use-package lsp-ui :commands lsp-ui-mode)
    ;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(with-eval-after-load 'eglot
  (setf (alist-get 'elixir-mode eglot-server-programs)
        (if (and (fboundp 'w32-shell-dos-semantics)
                 (w32-shell-dos-semantics))
            '("language_server.bat")
          (eglot-alternatives
           '("language_server.sh" "start_lexical.sh")))))

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode))

(defconst NB/IS-MACOS (eq system-type 'darwin))

(when NB/IS-MACOS
  (setopt mac-command-modifier 'meta
	  mac-option-modifier 'hyper))

(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "C-;") 'avy-goto-char))

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
        completion-category-overrides '((file (styles partial-completion))))

(setf epa-pinentry-mode 'loopback)

;; Example configuration for Consult
(use-package consult
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"
))

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package elixir-mode
  :ensure t
  :custom
  (lsp-elixir-server-command '("~/build/lexical/_build/dev/package/lexical/bin/start_lexical.sh")))

(use-package exunit
  :config
  ;; fix broken dark test link
  (custom-set-faces
   '(ansi-color-black ((t (:background "MediumPurple2" :foreground "MediumPurple2")))))
  :hook
  (elixir-ts-mode . exunit-mode)
  (elixir-mode . exunit-mode))

(defun nick/enter-pipe ()
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)
    (insert "|> ")))

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

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

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
  (dashboard-setup-startup-hook))

(use-package vterm)

(use-package switch-window
  :bind ("C-x o" . switch-window))

(use-package corfu
      ;; Optional customizations
      ;; :custom
      ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
      ;; (corfu-auto t)                 ;; Enable auto completion
      ;; (corfu-separator ?\s)          ;; Orderless field separator
      ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
      ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
      ;; (corfu-preview-current nil)    ;; Disable current candidate preview
      ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
      ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
      ;; (corfu-scroll-margin 5)        ;; Use scroll margin

      ;; Enable Corfu only for certain modes.
      ;; :hook ((prog-mode . corfu-mode)
      ;;        (shell-mode . corfu-mode)
      ;;        (eshell-mode . corfu-mode))

      ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
      ;; be used globally (M-/).  See also the customization variable
      ;; `global-corfu-modes' to exclude certain modes.
      :init
      (global-corfu-mode)

      :config
      (setq lsp-completion-provider :none)
(defun corfu-lsp-setup ()
  (setq-local completion-styles '(orderless)
	      completion-category-defaults nil))
(add-hook 'lsp-mode-hook #'corfu-lsp-setup)
)

(use-package general
  :ensure t
  :config

  (defvar-keymap prefix-buffer-map
    :doc "Prefix key map for buffers."
    "i" 'ibuffer
    "d"  'kill-current-buffer
    "b"  'consult-project-buffer
    "B"  'consult-buffer
    "p"  'Previous-buffer)

  (defvar-keymap prefix-window-map
    :doc "Prefix key map for windows."
    "c" 'split-window-right
    "n" 'split-window-below
    "d" 'delete-window
    "o" 'ace-window)

  (defvar-keymap prefix-project-map
    :doc "Prefix key map for projects."
    "p" 'project-switch-project
    "f" 'project-find-file
    "s" 'conult-grep)

  (defvar-keymap prefix-search-map
    :doc "Prefix key map for searching."
    "p" 'consult-grep
    "b" 'consult-line)

  (defvar-keymap prefix-open-map
    :doc "Prefix key map for opening things."
    "t" 'vterm
    "p" 'treemacs)

  (defvar-keymap prefix-map
    :doc "My prefix key map."
    "b" prefix-buffer-map
    "w" prefix-window-map
    "p" prefix-project-map
    "o" prefix-open-map
    "s" prefix-search-map)

  (which-key-add-keymap-based-replacements prefix-map
    "b" `("Buffer" . ,prefix-buffer-map)
    "w" `("Window" . ,prefix-window-map)
    "p" `("Project" . ,prefix-project-map)
    "o" `("Open" . ,prefix-open-map)
    "s" `("Search" . ,prefix-search-map))

  (keymap-set global-map "C-." prefix-map)
  )

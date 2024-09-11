;; Configure elpaca
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
		                          :ref nil :depth 1
		                          :files (:defaults "elpaca-test.el" (:exclude "extensions"))
		                          :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
	      (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
		             ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
						                                     ,@(when-let ((depth (plist-get order :depth)))
						                                         (list (format "--depth=%d" depth) "--no-single-branch"))
						                                     ,(plist-get order :repo) ,repo))))
		             ((zerop (call-process "git" nil buffer t "checkout"
				                               (or (plist-get order :ref) "--"))))
		             (emacs (concat invocation-directory invocation-name))
		             ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
				                               "--eval" "(byte-recompile-directory \".\" 0 'force)")))
		             ((require 'elpaca))
		             ((elpaca-generate-autoloads "elpaca" repo)))
	          (progn (message "%s" (buffer-string)) (kill-buffer buffer))
	        (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Elpaca use-package
;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

(org-babel-load-file "~/projects/nick/emacs.d/configuration.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "47e6f8c23eaea064b89ed1361b5824ee4f9562a8c4a30774ee9ee69f9b9d4f69" "1d89fcf0105dd8778e007239c481643cc5a695f2a029c9f30bd62c9d5df6418d" "5ec088e25ddfcfe37b6ae7712c9cb37fd283ea5df7ac609d007cafa27dab6c64" "d43860349c9f7a5b96a090ecf5f698ff23a8eb49cd1e5c8a83bb2068f24ea563" "0f220ea77c6355c411508e71225680ecb3e308b4858ef6c8326089d9ea94b86f" "ebbd4bbb0f017cb09f7a3b1363b83dfde0c5f4970cda2705419457366cd2de91" "a1c18db2838b593fba371cb2623abd8f7644a7811ac53c6530eebdf8b9a25a8d" "f13fa8e962b2ad938955650c449a8447769fc617f5d914552bff6b2ea7fec0bd" "f149d9986497e8877e0bd1981d1bef8c8a6d35be7d82cba193ad7e46f0989f6a" "90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "4825b816a58680d1da5665f8776234d4aefce7908594bea75ec9d7e3dc429753" "80214de566132bf2c844b9dee3ec0599f65c5a1f2d6ff21a2c8309e6e70f9242" default))
 '(git-gutter:added-sign "+")
 '(git-gutter:deleted-sign "-")
 '(git-gutter:modified-sign "|")
 '(git-gutter:unchanged "  ")
 '(org-agenda-files '("~/.org/notes.org"))
 '(package-selected-packages '(eglot elixir-mode which-key magit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-black ((t (:background "MediumPurple2" :foreground "MediumPurple2"))))
 '(tab-bar ((t (:background "#191919" :foreground "#fff" :height 1.1 :padding 1))))
 '(tab-bar-tab ((t (:background "#191919" :foreground "#fff" :box (:line-width 3 :color "#191919" :style flat-button)))))
 '(tab-bar-tab-inactive ((t (:background "#191919" :foreground "#aaa")))))

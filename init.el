;; Configure elpaca
(defvar elpaca-installer-version 0.8)
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

(org-babel-load-file "~/.emacs.d/configuration.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec" "937401a2e532f2c8c881b6b3f20d9d4b6b9405bccf72ea6289c9d3f4507eb1ab" "712dda0818312c175a60d94ba676b404fc815f8c7e6c080c9b4061596c60a1db" "a75aff58f0d5bbf230e5d1a02169ac2fbf45c930f816f3a21563304d5140d245" "2e7dc2838b7941ab9cabaa3b6793286e5134f583c04bde2fba2f4e20f2617cf7" "013728cb445c73763d13e39c0e3fd52c06eefe3fbd173a766bfd29c6d040f100" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "df6dfd55673f40364b1970440f0b0cb8ba7149282cf415b81aaad2d98b0f0290" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" "4990532659bb6a285fee01ede3dfa1b1bdf302c5c3c8de9fad9b6bc63a9252f7" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33" default))
 '(git-gutter:added-sign "+")
 '(git-gutter:deleted-sign "-")
 '(git-gutter:modified-sign "|")
 '(git-gutter:unchanged "  ")
 '(org-agenda-files '("~/.org/notes.org"))
 '(package-selected-packages '(ultra-scroll))
 '(package-vc-selected-packages
   '((ultra-scroll :vc-backend Git :url "https://github.com/jdtsmith/ultra-scroll"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-black ((t (:background "MediumPurple2" :foreground "MediumPurple2")))))

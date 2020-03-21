(use-package posframe)

(use-package company
  :config
  (global-company-mode t))

(use-package swiper
  :init
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume))

(use-package counsel
  :bind
  (("C-c f" . counsel-recentf))
  :init
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
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))


(use-package youdao-dictionary
  :ensure t
  :init
  ;; (defun my-posframe-arghandler (buffer-or-name arg-name value)
  ;;   (let ((info '(:internal-border-width 1 :background-color "#222")))
  ;;     (or (plist-get info arg-name) value)))

  ;; (setq posframe-arghandler #'my-pyosframe-arghandler)
  :functions (posframe-show
	      posframe-hide)
  :commands (youdao-dictionary-mode
	     youdao-dictionary--region-or-word
	     youdao-dictionary--format-result)
  :bind (("C-c y" . my-youdao-search-at-point)
	 ("C-c Y" . youdao-dictionary-search-at-point))
  :config
  ;; Cache documents
  (setq url-automatic-caching t)

  ;; Enable Chinese word segmentation support (支持中文分词)
  (setq youdao-dictionary-use-chinese-word-segmentation t)

  (with-eval-after-load 'posframe
    (defun youdao-dictionary-search-at-point-posframe ()
      "Search word at point and display result with posframe."
      (interactive)
      (let ((word (youdao-dictionary--region-or-word)))
	(if word
	    (progn
	      (posframe-show "youdao-buffer"
			     :foreground-color "green"
			     :background-color "#222"
			     :string (youdao-dictionary--format-result word)
			     :position (point))
	      (unwind-protect
		  (push (read-event) unread-command-events)
		(posframe-hide "youdao-buffer")))
	  (message "Nothing to look up")))))

  (defun my-youdao-search-at-point ()
    (interactive)
    (if (display-graphic-p)
	(if (fboundp 'youdao-dictionary-search-at-point-posframe)
	    (youdao-dictionary-search-at-point-posframe)
	  (youdao-dictionary-search-at-point-tooltip))
      (youdao-dictionary-search-at-point))))

(use-package projectile)
(use-package counsel-projectile)
(use-package magit)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package general
  :ensure t
  :config
  (general-create-definer spc-leader-def
    :states '(normal insert visual emacs)
    :prefix "<SPC>"
    :non-normal-prefix "C-,")
  (spc-leader-def
    "SPC" 'counsel-M-x
    "yv" '(youdao-dictionary-play-voice-at-point :wk "pronounce")
    "yy" 'my-youdao-search-at-point
    "fr" 'counsel-recentf
    "bb" 'counsel-switch-buffer
    "bl" 'evil-switch-to-windows-last-buffer
    "ff" 'find-file
    "fs" 'swiper
    "bk" 'kill-current-buffer
    "gs" 'magit-status
    "gd" 'magit-dispatch
    "ar" 'anzu-replace-at-cursor-thing
    "aa" 'anzu-query-replace-at-cursor
    "pf" 'counsel-projectile-find-file
    "pp" 'counsel-projectile-switch-project
    "pt" 'treemacs
    )
  (global-set-key (kbd "s-/") 'comment-line))


(provide 'init-pkgs2)

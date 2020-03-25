(use-package smart-hungry-delete
  :ensure t
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
		 ("C-d" . smart-hungry-delete-forward-char))
  :defer nil ;; dont defer so we can add our functions to hooks 
  :config (smart-hungry-delete-add-default-hooks)
  )

;;;###autoload
(defun my-string-inflection-cycle-auto ()
  "switching by major-mode"
  (interactive)
  (cond
   ;; for emacs-lisp-mode
   ((eq major-mode 'emacs-lisp-mode)
    (string-inflection-all-cycle))
   ;; for python
   ((eq major-mode 'python-mode)
    (string-inflection-python-style-cycle))
   ;; for java
   ((eq major-mode 'java-mode)
    (string-inflection-java-style-cycle))
   (t
    ;; default
    (string-inflection-ruby-style-cycle))))

(use-package drag-stuff
  :hook
  (java-mode . drag-stuff-mode)
  :config
  (drag-stuff-define-keys))


(use-package string-inflection
  :config
  ;; C-q C-u is the key bindings similar to Vz Editor.
  (global-unset-key (kbd "C-q"))
  (global-set-key (kbd "C-q C-u") 'my-string-inflection-cycle-auto))

(use-package ivy)

(use-package projectile)

(use-package ivy-xref
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package helm)
(use-package better-defaults)

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package posframe)

(use-package shell-pop
  :init
  (csetq shell-pop-default-directory "/Users/peterwu/.emacs.d")
  (csetq shell-pop-shell-type (quote ("multi-term" "*multi-term*" (lambda nil (multi-term)))))
  (csetq shell-pop-term-shell "/usr/local/bin/zsh")
  (csetq shell-pop-window-size 30)
  (csetq shell-pop-full-span t)
  (csetq shell-pop-window-position "bottom")
  (csetq shell-pop-autocd-to-working-dir t)
  (csetq shell-pop-restore-window-configuration t)
  (csetq shell-pop-cleanup-buffer-at-process-exit t)
  :hook (shell-pop-in-after . (lambda () (linum-mode -1))))

(use-package emojify)

(use-package dumb-jump
  :config
  (setq dumb-jump-default-project "~/Workspace")
  (setq dumb-jump-selector 'ivy))

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
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  :config
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable)
  )

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

(use-package projectile
  :init
  (csetq projectile-project-search-path '("~/Workspace/")))

(use-package counsel-projectile)
(use-package evil-magit :after (evil magit))
(use-package anzu)
(use-package expand-region)

(use-package fuz)

;; (use-package ivy-fuz
;;   :ensure t
;;   :demand t
;;   :after ivy
;;   :custom
;;   (ivy-sort-matches-functions-alist '((t . ivy-fuz-sort-fn)))
;;   (ivy-re-builders-alist '((t . ivy-fuz-regex-fuzzy)))
;;   :config
;;   (add-to-list 'ivy-highlight-functions-alist '(ivy-fuz-regex-fuzzy . ivy-fuz-highlight-fn)))

(use-package which-key
  :config
  (which-key-mode t))

(use-package better-shell
  :bind (("C-'" . better-shell-shell)))

(use-package eyebrowse)
(use-package multi-term)

(use-package yasnippet
  :hook
  ((prog-mode . yas-minor-mode))
  :config
  (yas-reload-all))

(use-package yasnippet-snippets)

(use-package ivy-yasnippet
  :after yasnippet)

(use-package ivy-explorer
  :config
  (ivy-explorer-mode t))

(use-package iedit
  :bind
  ("s-d" . iedit-mode))

(use-package popwin)

(use-package ivy-rich
  :config
  (ivy-rich-mode 1))


(provide 'init-pkgs)

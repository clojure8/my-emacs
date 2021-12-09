;; -*- coding: utf-8; lexical-binding: t; -*-


;;; Emacs 28 native compile
(when (and (>= emacs-major-version 28)
	   (fboundp 'native-comp-available-p)
	   (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors nil)
  (setq package-native-compile t)
  (add-to-list 'native-comp-eln-load-path
	       (expand-file-name "eln-cache" user-emacs-directory)))

;;; flymake cannot find load-path solution
;; [refs] https://emacs-china.org/t/flymake/8323/19
(setq elisp-flymake-byte-compile-load-path
      (append elisp-flymake-byte-compile-load-path load-path))

(defalias 'yes-or-no-p 'y-or-n-p)

;;; system coding
;; although others may add many other settings here,
;; but I think the next line is enough
(prefer-coding-system 'utf-8)

;;; emacs settings
(setq auto-save-default nil	   ; disable auto save
      auto-window-vscroll nil
      delete-by-moving-to-trash t  ; disable delete directly
      fast-but-imprecise-scrolling t
      frame-title-format "%b"
      help-window-select t
      inhibit-startup-screen t	   ; disable the startup screen splash
      inhibit-default-init t
      ;; initial-scratch-message nil
      inhibit-compacting-font-caches t
      make-backup-files nil             ; disable backup file
      ;; Mouse wheel scroll behavior
      ;; mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      next-line-add-newlines nil
      read-process-output-max (* 64 1024)
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position t
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      visible-bell nil)

;;; macOS special settings
;; <macOS> Command -> Meta, Option -> Super
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
	mac-option-modifier 'super
	ns-use-native-fullscreen t))

;;; Windows special settings
(when (boundp 'w32-get-true-file-attributes)
  (setq w32-get-true-file-attributes nil
	w32-pipe-read-delay 0
	w32-pipe-buffer-size (* 64 1024)))

;; solve the Chinese paste issue
(unless (memq system-type '(cygwin windows-nt ms-dos))
  selection-coding-system 'utf-8)

;; 像素滚动
(add-hook 'after-init-hook #'pixel-scroll-precision-mode)

(use-package evil
  :hook (after-init . evil-mode)
  :init
  (csetq evil-want-keybinding nil)
  )

(use-package evil-collection
  :after evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (setq evil-collection-mode-list '(dired magit org))
  (evil-collection-init))

(use-package general
  :config
  (general-define-key
   "M-p" 'counsel-switch-buffer
   "M-z" 'evil-undo
   "M-x" 'counsel-M-x
   "s-x" 'counsel-M-x
   "M-v" 'evil-paste-after
   "s-/" 'comment-line
   "M-/" 'comment-line
   "M-f" 'swiper
   "C-S-p" 'counsel-switch-buffer
   "M-s" 'save-buffer
   "s-;" 'yas-expand
   "C-x p" '+treemacs/toggle
   "C-x k" 'kill-current-buffer
   "C-c C-c" 'er/expand-region)
  (general-create-definer g-leader-def
	:states '(normal insert visual emacs)
	:prefix "g"
	:non-normal-prefix "C-;")
  (g-leader-def
	"l" 'centaur-tabs-forward-tab
	"h" 'centaur-tabs-backward-tab
	"k" 'centaur-tabs-forward-group
	"j" 'centaur-tabs-backward-group)
  (general-create-definer spc-leader-def
	:states '(normal insert visual emacs)
	:prefix "<SPC>"
	:non-normal-prefix "C-,")
  (spc-leader-def
	"SPC" 'counsel-M-x
	"br" 'revert-buffer
	"yv" '(youdao-dictionary-play-voice-at-point :wk "pronounce")
	"yy" 'my-youdao-search-at-point
	"ys" 'ivy-yasnippet
	"oc" 'org-capture
	"fr" 'counsel-recentf
	"bb" 'counsel-switch-buffer
	"bl" 'evil-switch-to-windows-last-buffer
	"ff" 'counsel-find-file
	"fs" 'swiper
	"bk" 'kill-current-buffer
	"gs" 'magit-status
	"gd" 'magit-dispatch
	"ar" 'anzu-replace-at-cursor-thing
	"aa" 'anzu-query-replace-at-cursor
	"pf" 'counsel-projectile-find-file
	"fp" 'counsel-projectile-find-file
	"pp" 'counsel-projectile-switch-project
	"pa" 'projectile-add-known-project
	"pr" 'counsel-projectile-rg
	"op" '+treemacs/toggle
	"jj" 'dumb-jump-go
	"jb" 'dumb-jump-back
	"cr" 'counsel-rg
	))

(use-package savehist)

(use-package undo-tree
  :hook (after-init . global-undo-tree-mode))

(use-package flycheck
  :hook (after-init . global-flycheck-mode))



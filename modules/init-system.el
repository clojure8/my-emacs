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
(csetq tab-width 4)
(setq auto-save-default nil	   ; disable auto save
      tab-width 4
      auto-window-vscroll nil
      ns-use-proxy-icon nil  ; hide titlebar icon
      delete-by-moving-to-trash t  ; disable delete directly
      fast-but-imprecise-scrolling t
      frame-title-format nil
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

(defvar temporary-file-directory "~/.emacs.d/.local/tmp")
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;==================================================
;; utf8
;;==================================================
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(csetq default-buffer-file-coding-system 'utf-8)

;; 像素滚动
(add-hook 'after-init-hook #'pixel-scroll-precision-mode)


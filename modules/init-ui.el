;; -*- coding: utf-8; lexical-binding: t; -*-

;;; code:

;;==================================================
;; utf8
;;==================================================
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(csetq default-buffer-file-coding-system 'utf-8)

;;===================================================
;; UI
;;===================================================
;; 隐藏titlebar显示文件名
(csetq ns-use-proxy-icon nil)
(csetq frame-title-format nil)

;;==================================================
;; 设置默认的目录
;;==================================================
(csetq default-directory "~/.emacs.d/")
;; 去除counsel默认开头
;(setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) "")

;;==================================================
;; 关闭告警图标
;;==================================================
(defun fLash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))


(add-hook 'org-mode-hook (lambda ()
						   (csetq org-table '((t (:foreground "#6c71c4" :family "Ubuntu Mono"))))))

;;==================================================
;; 中文字体设置
;;==================================================
(defun set-font (english chinese english-size chinese-size)
  (set-face-attribute 'default nil :font
					  (format   "%s:pixelsize=%d"  english english-size))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
	(set-fontset-font (frame-parameter nil 'font) charset
					  (font-spec :family chinese :size chinese-size))))
(add-hook 'after-init-hook
		  (lambda ()
			(set-font "JetBrains Mono" "STkaiti" 12 14)
			;; (set-font "Source Code Pro" "STkaiti" 13 15)
			(csetq line-spacing 0.108)
			;; (set-font "monospace" "STkaiti" 13 13)
			;; (set-font "Hack" "STkaiti" 12 14)
			(setq visible-bell nil
           ring-bell-function 'flash-mode-line)
			))

;;==================================================
;; 零散的一些设置
;;==================================================
(csetq tab-width 4)
(setq auto-save-default nil) ; stop creating #autosave# files
(setq inhibit-splash-screen t)
(add-hook 'after-init-hook 'global-hl-line-mode)

;;==================================================
;; 显示行号
;;==================================================
(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode))

;;==================================================
;; mac titilebar透明设置
;;==================================================
(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; {light, dark}
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-hook 'after-load-theme-hook
            (lambda ()
              (let ((bg (frame-parameter nil 'background-mode)))
                (set-frame-parameter nil 'ns-appearance bg)
                (setcdr (assq 'ns-appearance default-frame-alist) bg)))))

;;==================================================
;; theme
;;==================================================

;; (use-package eclipse-theme
;;   :config
;;   (doom-themes-treemacs-config)
;;   (load-theme 'eclipse t))

;; (use-package leuven-theme
;;   :config
;;   (load-theme 'leuven t))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  ;; (load-theme 'doom-fairy-floss t)
  ;;(load-theme 'doom-acario-dark t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") 
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;==================================================
;; modeline
;;==================================================
;; (use-package mood-line
;;   :straight
;;   (mood-line :host github :repo "clojure8/mood-line"))

;; (use-package mini-modeline
;;   :after mood-line
;;   :hook (after-init . mini-modeline-mode)
;;   :config
;;   ;; Setup flycheck hooks
;;   (add-hook 'flycheck-status-changed-functions #'mood-line--update-flycheck-segment)
;;   (add-hook 'flycheck-mode-hook #'mood-line--update-flycheck-segment)

;;   ;; Setup VC hooks
;;   (add-hook 'find-file-hook #'mood-line--update-vc-segment)
;;   (add-hook 'after-save-hook #'mood-line--update-vc-segment)
;;   (advice-add #'vc-refresh-state :after #'mood-line--update-vc-segment)

;;   (setq mini-modeline-face-attr '(:background "ffffff"))
;;   (face-spec-set 'mini-modeline-mode-line
;;                  '((((background light))
;;                     :background "#aa0000" :height 0.2 :box nil)
;;                    (t
;;                     :background "#aa0000" :height 0.2 :box nil)))
;;   (defun my/mood-line-segment-position ()
;; 	"Displays the current cursor position in the mode-line."
;; 	(concat "%l:%c"
;; 			(when mood-line-show-cursor-point (propertize (format ":%d" (point)) 'face 'mood-line-unimportant))
;; 			(propertize " %p  " 'face 'mood-line-unimportant)))
;;   (defun my/mood-line-segment-major-mode ()
;; 	"Displays the current major mode in the mode-line."
;; 	(concatconcat (format-mode-line (car mode-name) 'mood-line-major-mode) "  "))

;;   (setq mood-line-show-encoding-information t
;; 		mood-line-show-eol-style t)

;;   (setq mini-modeline-l-format '(" "
;;                                  (:eval (mood-line-segment-modified))
;;                                  ;; (:eval (mood-line-segment-buffer-name))
;;                                  (:eval "")
;;                                  (:eval (buffer-name))
;;                                  (:eval "   ")
;;                                  (:eval (mood-line-segment-anzu))
;;                                  (:eval (mood-line-segment-multiple-cursors))
;;                                  (:eval (my/mood-line-segment-position))))
;;   (setq mini-modeline-r-format
;;         '((:eval (mood-line-segment-eol))
;;           (:eval (mood-line-segment-encoding))
;;           (:eval (mood-line-segment-vc))
;;           (:eval (mood-line-segment-major-mode))
;;           (:eval (mood-line-segment-misc-info))
;;           (:eval (mood-line-segment-flycheck))
;;           (:eval (mood-line-segment-flymake))
;;           (:eval (mood-line-segment-process))
;;           " ")))

(use-package doom-modeline
  :init
  (setq doom-modeline-height 1)
  (set-face-attribute 'mode-line nil :family "JetBrains Mono" :height 116)
  :hook (after-init . doom-modeline-mode))

;;==================================================
;; others
;;==================================================

;; (use-package beacon
;;   :config
;;   (beacon-mode 1))

(use-package aggressive-indent
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

(use-package all-the-icons)

(use-package all-the-icons-ivy
  :after all-the-icons
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup)
  :config
  (setq all-the-icons-ivy-file-commands
        '(counsel-find-file counsel-file-jump counsel-recentf counsel-projectile-find-file counsel-projectile-find-dir)))

(use-package hydra)

;;==================================================
;; another tabbar
;;==================================================
(use-package centaur-tabs
  :demand
  ;; :custom-face
  ;; (centaur-tabs-selected ((t (:background "#ffffff" :weight bold :foreground "black"))))
  ;; (centaur-tabs-unselected ((t (:background "#e5e5e5" :foreground "#666666"))))
  :hook (after-init . centaur-tabs-mode)
  :init
  (setq centaur-tabs-height 20)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-set-close-button t)
  (setq x-underline-at-descent-line t))

(add-hook 'after-init-hook (lambda () (setq initial-frame-alist '((width . 142) (height . 53) (top . 20) (left . 144)))))



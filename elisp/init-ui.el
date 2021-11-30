;;==================================================
;; 初始化scratch设置 
;;==================================================
(defun xah-new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

It returns the buffer (for elisp programing).

URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (text-mode)
    (setq buffer-offer-save t)
    $buf
    ))
;; (setq initial-major-mode (quote text-mode))
(setq initial-buffer-choice 'xah-new-empty-buffer)

;;==================================================
;; utf8
;;==================================================
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;;===================================================
;; UI
;;===================================================
;; 隐藏titlebar显示文件名
(csetq ns-use-proxy-icon nil)
;; (csetq frame-title-format nil)
;;==================================================
;; 设置默认的目录
;;==================================================
(csetq default-directory "~")
;; 去除counsel默认开头
;(setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) "")

;;==================================================
;; 关闭告警图标
;;==================================================
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))
(setq visible-bell nil
      ring-bell-function 'flash-mode-line)


(csetq org-table '((t (:foreground "#6c71c4" :family "Ubuntu Mono"))))
;==================================================
;; 中文字体设置
;;==================================================
(defun set-font (english chinese english-size chinese-size)
  (set-face-attribute 'default nil :font
                      (format   "%s:pixelsize=%d"  english english-size))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family chinese :size chinese-size))))
;; (set-font "Source Code Pro" "STkaiti" 13 15)
(csetq line-spacing 0.108)
;; (set-font "monospace" "STkaiti" 13 13)
;; (set-font "Hack" "STkaiti" 12 14)
(set-font "JetBrains Mono" "STkaiti" 12 14)

;;==================================================
;; 零散的一些设置
;;==================================================
(csetq tab-width 4)
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-splash-screen t)
(global-hl-line-mode t)
;;改鼠标为光标
;; (setq-default cursor-type 'bar)
;; (add-hook 'prog-hook 'prettify-symbols-mode)

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

(use-package leuven-theme
  :config
  (load-theme 'leuven t))

; (use-package doom-themes
;   :config
;   ;; Global settings (defaults)
;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;   (load-theme 'doom-one t)
;   ;; (load-theme 'doom-fairy-floss t)
;   ;;(load-theme 'doom-acario-dark t)
;   ;; Enable flashing mode-line on errors
;   (doom-themes-visual-bell-config)
;   ;; Enable custom neotree theme (all-the-icons must be installed!)
;   (doom-themes-neotree-config)
;   ;; or for treemacs users
;   (setq doom-themes-treemacs-theme "doom-colors") 
;   (doom-themes-treemacs-config)
;   ;; Corrects (and improves) org-mode's native fontification.
;   (doom-themes-org-config))



;;==================================================
;; modeline
;;==================================================
(use-package mood-line
  :straight
  (mood-line :host github :repo "clojure8/mood-line")
  :hook
  (after-init . mood-line-mode))


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
;; (use-package centaur-tabs
;;   :demand
;;   :custom-face
;;   (centaur-tabs-selected ((t (:background "#ffffff" :weight bold :foreground "black"))))
;;   (centaur-tabs-unselected ((t (:background "#e5e5e5" :foreground "#666666"))))
;;   :init
;;   (setq centaur-tabs-height 22)
;;   (setq centaur-tabs-set-icons t)
;;   (setq centaur-tabs-set-close-button nil)
;;   (setq x-underline-at-descent-line t)
;;   :config
;;   ;; (centaur-tabs-headline-match)
;;   (centaur-tabs-mode t))


(provide 'init-ui)

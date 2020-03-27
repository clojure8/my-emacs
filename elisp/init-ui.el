;;===================================================
;; UI
;;===================================================
;; 隐藏titlebar显示文件名
(csetq ns-use-proxy-icon nil)
;; (csetq frame-title-format nil)

(setq visible-bell nil
      ring-bell-function 'flash-mode-line)
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

;;中英文字体设置
(defun set-font (english chinese english-size chinese-size)
  (set-face-attribute 'default nil :font
                      (format   "%s:pixelsize=%d"  english english-size))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family chinese :size chinese-size))))
;; (set-font "Source Code Pro" "STkaiti" 13 15)
(csetq line-spacing 0.308)
;; (set-font "monospace" "STkaiti" 13 13)
(set-font "Hack" "STkaiti" 12 14)

(csetq tab-width 4)
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-splash-screen t)
;;(setq-default mode-line-format nil)
(global-hl-line-mode t)

;;改鼠标为光标
(setq-default cursor-type 'bar)
(add-hook 'prog-hook 'prettify-symbols-mode)

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

(use-package eclipse-theme
  :config
  (load-theme 'eclipse t))

;; (use-package doom-themes
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-one t)
;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Enable custom neotree theme (all-the-icons must be installed!)
;;   (doom-themes-neotree-config)
;;   ;; or for treemacs users
;;   (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
;;   (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

;; (use-package spacemacs-theme
;;   :defer t
;;   :init
;;   (load-theme 'spacemacs-dark t)
;;   ;; (load-theme 'spacemacs-light t)
;;   )

;; (use-package monokai-theme
;;   :defer t
;;   :init
;;   (load-theme 'monokai t))

;;==================================================
;; 显示行号
;;==================================================
(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode))

;;==================================================
;; modeline
;;==================================================

(use-package spaceline
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1))

;; (require 'awesome-tray)
;; (csetq awesome-tray-mode-line-active-color  "#3366ff")
;; (csetq awesome-tray-active-modules
;;        '("location" "parent-dir" "mode-name" "awesome-tab" ))
;; (awesome-tray-mode 1)


;;==================================================
;; others
;;==================================================

(use-package beacon
  :config
  (beacon-mode 1))

(use-package aggressive-indent
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

(use-package all-the-icons
  :defer 0.5)

(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup)
  :config
  (setq all-the-icons-ivy-file-commands
        '(counsel-find-file counsel-file-jump counsel-recentf counsel-projectile-find-file counsel-projectile-find-dir))
  )

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
;;   (setq centaur-tabs-height 23)
;;   (setq centaur-tabs-set-icons t)
;;   (setq centaur-tabs-set-close-button nil)
;;   (setq x-underline-at-descent-line t)
;;   :config
;;   ;; (centaur-tabs-headline-match)
;;   (centaur-tabs-mode t))

(provide 'init-ui)

;;========================================================================================
;; UI
;;========================================================================================
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

;; (when (memq window-system '(mac ns))
;;   (add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; {light, dark}
;;   (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;;   (add-hook 'after-load-theme-hook
;;             (lambda ()
;; 	          (let ((bg (frame-parameter nil 'background-mode)))
;;                 (set-frame-parameter nil 'ns-appearance bg)
;;                 (setcdr (assq 'ns-appearance default-frame-alist) bg)))))


;; (use-package eclipse-theme
;;   :config
;;   (load-theme 'eclipse t))

(use-package spacemacs-theme
  :defer t
  :init
  (load-theme 'spacemacs-dark t)
  ;; (load-theme 'spacemacs-light t)
  )

;; (use-package monokai-theme
;;   :defer t
;;   :init
;;   (load-theme 'monokai t))

;;显示行号
(global-linum-mode t)

(use-package powerline-evil
  :after (evil evil-collection)
  :config
  (setq powerline-display-buffer-size nil)
  (setq powerline-display-mule-info nil)
  (setq powerline-display-hud nil)
  (when (display-graphic-p)
    (powerline-default-theme)
    (remove-hook 'focus-out-hook 'powerline-unset-selected-window)))

;; (use-package telephone-line
;;   :init
;;   (setq telephone-line-primary-left-separator 'telephone-line-gradient
;;         telephone-line-secondary-left-separator 'telephone-line-nil
;;         telephone-line-primary-right-separator 'telephone-line-gradient
;;         telephone-line-secondary-right-separator 'telephone-line-nil)
;;   (setq ;;telephone-line-height 24
;;    telephone-line-evil-use-short-tag t)
;;   :config
;;   (telephone-line-mode 1))

;; (use-package    feebleline
;;   :ensure       t
;;   :config       (setq feebleline-msg-functions
;;                       '((feebleline-line-number         :post "" :fmt "%5s")
;;                         (feebleline-column-number       :pre ":" :fmt "%-2s")
;;                         (feebleline-file-directory      :face feebleline-dir-face :post "")
;;                         (feebleline-file-or-buffer-name :face font-lock-keyword-face :post "")
;;                         (feebleline-file-modified-star  :face font-lock-warning-face :post "")
;;                         (feebleline-git-branch          :face feebleline-git-face :pre " : ")
;;                         (feebleline-project-name        :align right)))
;;   (feebleline-mode 1))

;; (use-package mini-modeline
;;   :config
;;   (mini-modeline-mode 1))

;; (require 'awesome-tray)
;; (csetq awesome-tray-mode-line-active-color  "#3366ff")
;; (csetq awesome-tray-active-modules
;;        '("location" "parent-dir" "mode-name" "awesome-tab" ))
;; (awesome-tray-mode 1)

;; (toggle-frame-maximized)
(use-package beacon
  :config
  (beacon-mode 1))

(use-package aggressive-indent
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

(use-package all-the-icons
  :defer 0.5)

;; (use-package all-the-icons-ivy
;;   :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup)
;;   :config
;;   (setq all-the-icons-ivy-file-commands
;;         '(counsel-find-file counsel-file-jump counsel-recentf counsel-projectile-find-file counsel-projectile-find-dir))
;;   )

;; (require 'awesome-tab)
;; (setq awesome-tab-height 100)
;; (setq awesome-tab-icon-height 0.8)
;; (awesome-tab-mode t)


(provide 'init-ui)

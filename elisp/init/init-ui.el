;;========================================================================================
;; UI
;;========================================================================================
;;(if (display-graphic-p)
;;    (progn
;;      (setq initial-frame-alist
;;            '(
;;              (width . 106) ; chars
;;              (height . 47) ; lines
;;              (left . 50)
;;              (top . 50)))
;;      (setq default-frame-alist
;;            '(
;;              (width . 106)
;;              (height . 47)
;;              (left . 50)
;;              (top . 50)))))

;;中英文等宽设置
;;(defun set-font (english chinese english-size chinese-size)
;;  (set-face-attribute 'default nil :font
;;                      (format   "%s:pixelsize=%d"  english english-size))
;;  (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;    (set-fontset-font (frame-parameter nil 'font) charset
;;                      (font-spec :family chinese :size chinese-size))))
;;
;;(set-font "Source Code Pro" "STkaiti" 13 16)
;;(custom-set-faces
;; ;; custom-set-faces was added by Custom.
;; ;; If you edit it by hand, you could mess it up, so be careful.
;; ;; Your init file should contain only one such instance.
;; ;; If there is more than one, they won't work right.
;; '(org-table ((t (:foreground "#6c71c4" :family "Ubuntu Mono")))))

(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
;;去滚动条
(scroll-bar-mode -1)
;;去工具栏
(tool-bar-mode -1)
;;去开始画面
(setq inhibit-splash-screen t)
;;去下面的状态栏
;;(setq-default mode-line-format nil)
;;改鼠标为光标
(setq-default cursor-type 'bar)
(add-hook 'prog-hook 'prettify-symbols-mode)

(defun mac-x-dark-theme ()
(when sys/mac-x-p
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-hook 'after-load-theme-hook
            (lambda ()
	      (let ((bg (frame-parameter nil 'background-mode)))
                (set-frame-parameter nil 'ns-appearance bg)
                (setcdr (assq 'ns-appearance default-frame-alist) bg))))))


;;Titlebar
(defun doom-init ()
  ;;安装doom主题样式
  (load-theme 'doom-one)
  (toggle-frame-maximized)
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-themes
  :config
  (doom-init))


;;显示行号
(if (fboundp 'display-line-numbers-mode)
    (use-package display-line-numbers
      :ensure nil
      :hook (prog-mode . display-line-numbers-mode))
  (use-package linum-off
    :demand
    :defines linum-format
    :hook (after-init . global-linum-mode)
    :init (setq linum-format "%4d ")
    :config
    ;; Highlight current line number
    (use-package hlinum
      :defines linum-highlight-in-all-buffersp
      :custom-face (linum-highlight-face ((t (:inherit default :background nil :foreground nil))))
      :hook (global-linum-mode . hlinum-activate)
      :init (setq linum-highlight-in-all-buffersp t))))


(provide 'init-ui)

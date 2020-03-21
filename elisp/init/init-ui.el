;;========================================================================================
;; UI
;;========================================================================================
(setq visible-bell nil
      ring-bell-function 'flash-mode-line)
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

;;中英文等宽设置
(defun set-font (english chinese english-size chinese-size)
  (set-face-attribute 'default nil :font
                      (format   "%s:pixelsize=%d"  english english-size))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family chinese :size chinese-size))))
;;(set-font "Source Code Pro" "STkaiti" 13 15)
;; (let ((font "Monofur"))
;;   (set-font font font 14 14))
(csetq org-table '((t (:foreground "#6c71c4" :family "Ubuntu Mono"))))

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

;; (when (memq window-system '(mac ns))
;;   (add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; {light, dark}
;;   (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;;   (add-hook 'after-load-theme-hook
;;             (lambda ()
;; 	      (let ((bg (frame-parameter nil 'background-mode)))
;;                 (set-frame-parameter nil 'ns-appearance bg)
;;                 (setcdr (assq 'ns-appearance default-frame-alist) bg)))))


(use-package eclipse-theme
  :config
  (load-theme 'eclipse t))

;;显示行号
(global-linum-mode 1)

(use-package powerline
  :after (eclipse-theme)
  :config
  (setq powerline-display-buffer-size nil)
  (setq powerline-display-mule-info nil)
  (setq powerline-display-hud nil)
  (when (display-graphic-p)
    (powerline-default-theme)
    (remove-hook 'focus-out-hook 'powerline-unset-selected-window)))

(toggle-frame-maximized)

(provide 'init-ui)

;; -*- coding: utf-8; lexical-binding: t; -*-

;;; code:


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

(add-hook 'org-mode-hook
	  (lambda () (csetq org-table '((t (:foreground "#6c71c4" :family "Ubuntu Mono"))))))

;;==================================================
;; 显示行号
;;==================================================
(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode))

(add-hook 'after-init-hook 'global-hl-line-mode)

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



;; init frame postition
(add-hook 'after-init-hook (lambda () (setq initial-frame-alist '((width . 142) (height . 53) (top . 20) (left . 144)))))



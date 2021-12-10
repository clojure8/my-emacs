;; -*- coding: utf-8; lexical-binding: t; -*-

;;; code:

(use-package mood-line
  :straight
  (mood-line :host github :repo "clojure8/mood-line"))

(use-package mini-modeline
  :after mood-line
  :hook (after-init . mini-modeline-mode)
  :init
  ;; (setq mini-modeline-right-padding 3)
  :config
  ;; Setup flycheck hooks
  (add-hook 'flycheck-status-changed-functions #'mood-line--update-flycheck-segment)
  (add-hook 'flycheck-mode-hook #'mood-line--update-flycheck-segment)

  ;; Setup VC hooks
  (add-hook 'find-file-hook #'mood-line--update-vc-segment)
  (add-hook 'after-save-hook #'mood-line--update-vc-segment)
  (advice-add #'vc-refresh-state :after #'mood-line--update-vc-segment)

  (setq mini-modeline-face-attr 'default)

  (face-spec-set 'mini-modeline-mode-line
                 '((((background light))
                    :background "#6699cc" :height 0.2 :box nil)
                   (t
                    :background "#aa0000" :height 0.2 :box nil)))
  
  (defun my/mood-line-segment-position ()
    "Displays the current cursor position in the mode-line."
    (concat "%l:%c"
			(when mood-line-show-cursor-point (propertize (format ":%d" (point)) 'face 'mood-line-unimportant))
			(propertize " %p  " 'face 'mood-line-unimportant)))
  (defun my/mood-line-segment-major-mode ()
    "Displays the current major mode in the mode-line."
    (concatconcat (format-mode-line (car mode-name) 'mood-line-major-mode) "  "))

  (setq mood-line-show-encoding-information t
		mood-line-show-eol-style t)

  (setq mini-modeline-l-format '(" "
                                 (:eval (mood-line-segment-modified))
                                 ;; (:eval (mood-line-segment-buffer-name))
                                 (:eval " ")
                                 (:eval (buffer-name))
                                 (:eval "  ")
                                 (:eval (mood-line-segment-anzu))
                                 (:eval (mood-line-segment-multiple-cursors))
                                 (:eval (my/mood-line-segment-position))
								 ;; (:eval (list (nyan-create)))
								 ))
  (setq mini-modeline-r-format
        '((:eval (mood-line-segment-eol))
          (:eval (mood-line-segment-encoding))
          (:eval (mood-line-segment-vc))
          (:eval (mood-line-segment-major-mode))
          (:eval (mood-line-segment-misc-info))
          (:eval (mood-line-segment-flycheck))
          (:eval (mood-line-segment-flymake))
          (:eval (mood-line-segment-process))
          " ")))


(use-package nyan-mode)

;; (use-package doom-modeline
;;   :init
;;   (setq doom-modeline-height 1)
;;   (set-face-attribute 'mode-line nil :family "JetBrains Mono" :height 116)
;;   :hook (after-init . doom-modeline-mode))




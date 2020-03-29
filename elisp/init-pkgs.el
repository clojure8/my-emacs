
(use-package figlet)
(use-package ace-window)
(use-package ace-link)
(use-package multiple-cursors)



(use-package avy)

(use-package smart-hungry-delete
  ;; 绑定backspace会导致counsel-find-file下产生readonly bug!!!
  ;; :bind (("<backspace>" . smart-hungry-delete-backward-char)
  ;;   	 ("C-d" . smart-hungry-delete-forward-char))
  :defer nil ;; dont defer so we can add our functions to hooks 
  :config (smart-hungry-delete-add-default-hooks))


;;;###autoload
(defun my-string-inflection-cycle-auto ()
  "switching by major-mode"
  (interactive)
  (cond
   ;; for emacs-lisp-mode
   ((eq major-mode 'emacs-lisp-mode)
    (string-inflection-all-cycle))
   ;; for python
   ((eq major-mode 'python-mode)
    (string-inflection-python-style-cycle))
   ;; for java
   ((eq major-mode 'java-mode)
    (string-inflection-java-style-cycle))
   (t
    ;; default
    (string-inflection-ruby-style-cycle))))

(use-package drag-stuff
  :hook
  (java-mode . drag-stuff-mode)
  :config
  (drag-stuff-define-keys))

;;==================================================
;; 变量命名变形
;;==================================================
(use-package string-inflection
  :config
  ;; C-q C-u is the key bindings similar to Vz Editor.
  (global-unset-key (kbd "C-q"))
  (global-set-key (kbd "C-q C-u") 'my-string-inflection-cycle-auto))

(use-package better-defaults)

(use-package editorconfig
  :hook
  (prog-mode . editorconfig-mode))

(use-package posframe)
(use-package emojify)

(use-package dumb-jump
  :config
  (setq dumb-jump-default-project "~/Workspace")
  (setq dumb-jump-selector 'ivy))

(use-package youdao-dictionary
  :ensure t
  :init
  ;; (defun my-posframe-arghandler (buffer-or-name arg-name value)
  ;;   (let ((info '(:internal-border-width 1 :background-color "#222")))
  ;;     (or (plist-get info arg-name) value)))

  ;; (setq posframe-arghandler #'my-pyosframe-arghandler)
  :functions (posframe-show
	          posframe-hide)
  :commands (youdao-dictionary-mode
	         youdao-dictionary--region-or-word
	         youdao-dictionary--format-result)
  :bind (("C-c y" . my-youdao-search-at-point)
	     ("C-c Y" . youdao-dictionary-search-at-point))
  :config
  ;; Cache documents
  (setq url-automatic-caching t)

  ;; Enable Chinese word segmentation support (支持中文分词)
  (setq youdao-dictionary-use-chinese-word-segmentation t)

  (with-eval-after-load 'posframe
    (defun youdao-dictionary-search-at-point-posframe ()
      "Search word at point and display result with posframe."
      (interactive)
      (let ((word (youdao-dictionary--region-or-word)))
	    (if word
	        (progn
	          (posframe-show "youdao-buffer"
			                 :foreground-color "green"
			                 :background-color "#222"
			                 :string (youdao-dictionary--format-result word)
			                 :position (point))
	          (unwind-protect
		          (push (read-event) unread-command-events)
		        (posframe-hide "youdao-buffer")))
	      (message "Nothing to look up")))))

  (defun my-youdao-search-at-point ()
    (interactive)
    (if (display-graphic-p)
	    (if (fboundp 'youdao-dictionary-search-at-point-posframe)
	        (youdao-dictionary-search-at-point-posframe)
	      (youdao-dictionary-search-at-point-tooltip))
      (youdao-dictionary-search-at-point))))

(use-package projectile
  :init
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-enable-caching t)
  ;; Projectile to be usable in every directory 
  (setq projectile-require-project-root nil)) 

(use-package counsel-projectile)
(use-package evil-magit :after (evil magit))
(use-package anzu)
(use-package expand-region)

(use-package which-key
  :config
  (which-key-mode t))

(use-package eyebrowse)
(use-package multi-term)

;;==================================================
;; yasnippet
;;==================================================
(use-package yasnippet
  :hook
  ((prog-mode . yas-minor-mode))
  :config
  (yas-reload-all))
(use-package ivy-yasnippet
  :after (ivy yasnippet))
(use-package yasnippet-snippets)

(use-package auto-yasnippet
  :bind
  ("M-o" . aya-open-line))


;;==================================================
;; 其他提高编辑速度的东西
;;==================================================
(use-package iedit
  :bind
  ("s-d" . iedit-mode))

(use-package popwin)

(use-package helpful
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-at-point)
  :config
  (setq helpful-max-buffers 5)
  ;; don't pop new window
  (setq helpful-switch-buffer-function
        (lambda (buf) (if-let ((window (display-buffer-reuse-mode-window buf '((mode . helpful-mode)))))
                          ;; ensure the helpful window is selected for `helpful-update'.
                          (select-window window)
                        ;; line above returns nil if no available window is found
                        (pop-to-buffer buf))))
  (defvar my-helpful-history () "History of helpful, a list of buffers.")
  (advice-add #'helpful-update :around #'my-helpful/helpful-update)
  (advice-add #'helpful--buffer :around (lambda (oldfunc &rest _)
                                          (let ((buf (apply oldfunc _)))
                                            (push buf my-helpful-history)
                                            buf))))
;;;###autoload
(defun my-helpful/helpful-update (oldfunc)
  "Insert back/forward buttons."
  (funcall oldfunc)
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (insert-text-button "Back"
                        'action (lambda (&rest _)
                                  (interactive)
                                  (my-helpful/switch-to-buffer (current-buffer) 1)))
    (insert " / ")
    (insert-text-button "Forward"
                        'action (lambda (&rest _)
                                  (interactive)
                                  (my-helpful/switch-to-buffer (current-buffer)  -1)))
    (insert "\n\n")))

;;;###autoload
(defun my-helpful/switch-to-buffer (buffer &optional offset)
  "Jump to last SYMBOL in helpful history, offset by OFFSET."
  (interactive)
  (require 'seq)
  (require 'cl-lib)
  (setq my-helpful-history (seq-remove (lambda (buf) (not (buffer-live-p buf))) my-helpful-history))
  (cl-labels ((find-index (elt lst)
                          (let ((idx 0)
                                (len (length lst)))
                            (while (and (not (eq elt (nth idx lst)))
                                        (not (eq idx len)))
                              (setq idx (1+ idx)))
                            (if (eq idx len)
                                nil
                              idx))))
    (let ((idx (+ (or offset 0) (find-index buffer my-helpful-history))))
      (if (or (>= idx (length my-helpful-history))
              (< idx 0))
          (message "No further history.")
        (switch-to-buffer (nth idx my-helpful-history))))))

(use-package gitignore-templates)

(use-package lua-mode)

;;==================================================
;; 连接浏览器的textarea
;;==================================================
(use-package atomic-chrome
  :config
  ;;这玩意太影响启动速度了，用的时候再开启
  ;; (atomic-chrome-start-server)
  )

(provide 'init-pkgs)

;;;###autoload
(defun insert-serial-version-uuid()
  (interactive)
  (insert "private static final long serialVersionUID = 1L;"))

;; ;;;###autoload
;; (defun default-code-style-hook()
;;   (setq c-basic-offset 2
;;         c-label-offset 0
;;         tab-width 4
;;         indent-tabs-mode nil
;;         ;; compile-command "mvn -q -o -f ~/src/content-engine/engine/engine-core/pom.xml test -DtrimStackTrace=false"
;;         require-final-newline nil))
;; (add-hook 'java-mode-hook 'default-code-style-hook)

(use-package hydra)

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "s-l")
  :hook ((java-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq lsp-inhibit-message t
        lsp-eldoc-render-all nil
        lsp-enable-file-watchers nil
        lsp-highlight-symbol-at-point nil)
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024 10)) ;; 10mb
  (setq lsp-idle-delay 0.500)
  )

(use-package lsp-java
  :init
  (setq lsp-java-server-install-dir "~/.emacs.d/eclipse.jdt.ls/")
  (setq lsp-java-vmargs
        (list
         "-noverify"
         "-Xmx1G"
         "-XX:+UseG1GC"
         "-XX:+UseStringDeduplication"
         "-javaagent:/Users/peterwu/.m2/repository/org/projectlombok/lombok/1.18.8/lombok-1.18.8.jar"
         )
        ;; Don't organise imports on save
        lsp-java-save-action-organize-imports nil
        lsp-java-java-path "/usr/bin/java"))

;; optional
(use-package lsp-ui
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :config
  (setq lsp-prefer-flymake nil
        lsp-ui-sideline-enable nil
        lsp-use-native-json t
        lsp-ui-sideline-ignore-duplicate t
        ;; lsp-ui-doc-use-webkit t
        lsp-ui-doc-delay 5.0
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-symbol nil))

;;(use-package company-lsp :commands company-lsp)

;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :bind
  ("<f5>" . dap-debug)
  ("<f7>" . dap-step-in)
  ("<M-f7>" . dap-step-out)
  ("<f8>" . dap-next)
  ("<f9>" . dap-continue)
  :hook
  ((java-mode . dap-mode)
   (java-mode . dap-ui-mode))
  :config
  (require 'dap-java)
  (dap-register-debug-template
   "Java Run"
   (list :type "java"
         :request "launch"
         :args ""
         :noDebug t
         :cwd nil
         :host "localhost"
         :request "launch"
         :modulePaths []
         :classPaths nil
         :name "JavaRun"
         :projectName nil
         :mainClass nil)))

(use-package flycheck
  :config
  (require 'lsp-ui-flycheck)
  (lsp-ui-flycheck-enable nil)
  (flycheck-mode))

;;==================================================
;; java自定义的一些东西
;;==================================================
(require 's)
(require 'f)
(defun my/find-java-project-root (&optional dir)
  (when (and (buffer-file-name)
             (not (and dir (f-same? "~" dir))))
    (let ((dir (or dir (f-dirname (buffer-file-name)))))
      (if (or (f-exists? (f-expand ".git" dir))
              (f-exists? (f-expand "pom.xml" dir))
              (f-exists? (f-expand "build.gradle" dir)))
          dir
        (my/find-java-project-root (f-parent dir))))))

(defun my/new-java-class (class-name)
  (interactive "sEnter a class name: ")
  (let* ((class-full-path-list (s-split "\\." class-name))
         (current-dir (f-dirname (buffer-file-name)))
         (save-to-current-dir? (= 1 (length class-full-path-list)))
         (project-root (or (my/find-java-project-root) current-dir))
         (src-default-dir "src/main/java")
         (src-dir (f-join project-root (if save-to-current-dir?  current-dir src-default-dir)))
         (package-dir (s-join "/" (butlast class-full-path-list)))  
         (java-class-name (s-capitalized-words (car (last class-full-path-list))))
         (java-package-name (if save-to-current-dir?
                                (s-join "." (f-split (f-relative current-dir (f-join project-root src-default-dir))))
                              (s-join "." (butlast class-full-path-list))))
         (java-class-file-content (concat "package " java-package-name ";\n\n\n"))
         (java-class-file-path (f-join src-dir package-dir (concat java-class-name ".java"))))
    (make-directory (f-join src-dir package-dir) t)
    (f-write-text java-class-file-content 'utf-8 java-class-file-path)
    ;; 打开java类文件的时候进行yas-expand操作
    (let ((ff-hook-fn-symbol (intern (concat "ff--" class-name "-fn"))))
      (defalias ff-hook-fn-symbol
        (lambda ()
          (interactive)
          (when (f-same? java-class-file-path (buffer-file-name))
            (beginning-of-line 5)
            (insert "cls1")
            (yas-expand))))
      (add-hook 'find-file-hook ff-hook-fn-symbol)
      (find-file java-class-file-path)
      (remove-hook 'find-file-hook ff-hook-fn-symbol)
      (fmakunbound ff-hook-fn-symbol))))


(provide 'init-lsp-java)

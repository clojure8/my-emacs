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

(use-package company-lsp :commands company-lsp)

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


(provide 'init-lsp-java)

;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package nox
  :ensure nil
  :config
  (require 'nox)
  (dolist (hook (list
                 'js-mode-hook
                 'rust-mode-hook
                 'python-mode-hook
                 'ruby-mode-hook
                 'java-mode-hook
                 'sh-mode-hook
                 'c-mode-common-hook
                 'c-mode-hook
                 'c++-mode-hook
                 ))
    (add-hook hook '(lambda () (nox-ensure)))))

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

(provide 'init-nox-java)

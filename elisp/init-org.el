
;;custom org mode
(csetq org-directory "~/orgs")
(csetq org-table '((t (:foreground "#6c71c4" :family "Ubuntu Mono"))))
(csetq org-default-notes-file (concat org-directory "/notes.org"))
(csetq org-download-image-dir "~/org/downloads")
(csetq org-agenda-files
       (list "~/org/agenda/gcal.org"
             "~/org/agenda/i.org"
             "~/org/agenda/schedule.org"))

(use-package org-download)

(use-package org-bullets
  :hook ((org-mode . org-bullets-mode)
         (org-mode . org-indent-mode)))

(use-package noflet)

(setq org-capture-templates
      '(("a" "Appointment" entry (file  "~/org/agenda/gcal.org" "Appointments")
         "* TODO %?\n:PROPERTIES:\n\n:END:\nDEADLINE: %^T \n %i\n")
        ("n" "Note" entry (file+headline "~/org/agenda/notes.org" "Notes")
         "* Note %?\n%T")
        ("l" "Link" entry (file+headline "~/org/agenda/links.org" "Links")
         "* %? %^L %^g \n%T" :prepend t)
        ("b" "Blog idea" entry (file+headline "~/org/agenda/i.org" "Blog Topics:")
         "* %?\n%T" :prepend t)
        ("t" "To Do Item" entry (file+headline "~/org/agenda/i.org" "To Do Items")
         "* %?\n%T" :prepend t)
        ("j" "Journal" entry (file+datetree "~/Dropbox/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("s" "Screencast" entry (file "~/org/agenda/screencastnotes.org")
         "* %?\n%i\n")))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-capture-destroy
    (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))


(defun make-capture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "capture")))
  (select-frame-by-name "capture")
  (delete-other-windows)
  (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
    (org-capture)))

(provide 'init-org)

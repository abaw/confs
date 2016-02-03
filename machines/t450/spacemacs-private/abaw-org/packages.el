;;; packages.el --- abaw-org layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ken Wu <abaw@abaw-pro>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `abaw-org-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `abaw-org/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `abaw-org/pre-init-PACKAGE' and/or
;;   `abaw-org/post -init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst abaw-org-packages
  '(org)
  )
(defun abaw-org/join-path (&rest components)
  "Join path components into a path and return it. For
  example, (abaw/join-path \"a\" \"b\" \"c\") returns \"a/b/c\"
  in unix."
  (when components
    (if (cdr components)
        (concat (file-name-as-directory (car components))
                (apply 'abaw-org/join-path (cdr components)))
      (car components))))

(defun abaw-org/pre-init-org ()
  (bind-keys*
   ("C-c c" . org-capture)
   ("<f12>" . org-agenda)))

(defun abaw-org/auto-git ()
  (let ((the-file (expand-file-name (buffer-file-name))))
    (when (string-suffix-p ".org" the-file)
      (loop for f in (cons org-default-notes-file org-agenda-files)
            when (equal the-file (expand-file-name f))
            do (progn
                 (message (message "auto git commit for  %s" the-file))
                 (start-process-shell-command "abaw-org-auto-git" "*abaw-org-auto-git*" (format "git commit -am 'Auto commited by emacs'" )))
            and return nil))))

(defun abaw-org/post-init-org ()
  "Configure org-mode"
  (setq org-directory "~/orgs")
  (setq org-default-notes-file (abaw-org/join-path org-directory "notes.org"))
  (setq org-agenda-files
        (mapcar (lambda (f) (abaw-org/join-path org-directory f))
                (list "todo.org" "diary.org" "refile.org")))

  ;; Create agenda files if they do not exist
  (loop for f in org-agenda-files
        unless (file-exists-p f)
        if (string-suffix-p "refile.org" f)
        do (progn
             (message "Writing initial refile.org: %s" f)
             (write-region "#+FILETAGS: REFILE\n" nil f))
        else do (progn (message "Write an empty-line file: %s" f)
                       (write-region "" nil f)))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "MAYBE(m)" "NEXT(n)" "WAITING(w@/!)" "|" "DONE(d)" "CANCELLED(c@)" "DELEGATED(D@)" )))

  (setq org-todo-state-tags-triggers
        '((done ("WAITING"))
          ("TODO" ("WAITING") ("CANCELLED") )
          ("MAYBE" ("WAITING") ("CANCELLED"))
          ("WAITING" ("WAITING" . t))
          ("CANCELLED" ("CANCELLED" . t) ("DELEGATED"))
          ("DELEGATED" ("DELEGATED" . t) ("CANCELLED"))
          ("DONE" ("WAITING") ("CANCELLED") ("DELEGATED"))
          ))

  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'after-save-hook #'abaw-org/auto-git t t)))

  (setq org-capture-templates
        '(("t" "Tasks" entry (file "refile.org") "* TODO %?\n:PROPERTIES:\n:ADDED: %U\n:END:\n" :clock-in t :clock-resume t)
          ("j" "Journal" entry (file+datetree "diary.org") "* %?\n%U" :clock-in t :clock-resume t)
          ("n" "Note" entry (file "refile.org") "* %? :NOTE:\n%U\n%a\n")))

  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))

  (setq org-agenda-custom-commands
        '((" " "Agenda"
           ((agenda "" nil)
            (tags "REFILE"
                  ((org-agenda-overriding-header "Tasks to Refile")
                   (org-tags-match-list-sublevels nil)))

            (tags-todo "-REFILE-CANCELLED-WAITING/!"
                       ((org-agenda-overriding-header "Tasks")
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-CANCELLED+WAITING/!"
                       ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                        (org-tags-match-list-sublevels nil)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-todo-ignore-deadlines 'future)))))))
  )


;;; packages.el ends here

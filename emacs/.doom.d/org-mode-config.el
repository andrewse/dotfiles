(require 'ox-md)
(require 'org-tempo)
(require 'org-expiry)
(require 'org-id)

(setq   org-directory "~/.asorganise/Orgmode"
        org-default-notes-file (concat org-directory "/notes.org")
        org-attach-directory (concat org-directory "/Attachments")
        ispell-program-name "aspell"
        org-archive-location (concat org-directory "/Archive/%s_archive::")
        org-special-ctrl-a/e t
        org-return-follows-link t
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t
        org-log-done 'time
        org-log-refile 'time
        org-log-repeat 'time
        org-log-reschedule 'time
        org-closed-keep-when-no-todo t
        org-log-into-drawer t
        org-agenda-text-search-extra-files (list 'agenda-archives)
        org-adapt-indentation nil         ; Makes org not indent everything to the level of the heading but instead things are left aligned
        org-agenda-bulk-custom-functions '((?C (lambda nil (org-agenda-todo ""))))         ; Bulk agenda command to remove TODO keywords from items
                                        ; org-agenda-todo-ignore-scheduled, org-agenda-todo-ignore-deadlines, org-agenda-todo-ignore-timestamp
                                        ; org-agenda-todo-ignore-with-date
                                        ; org-agenda-todo-list-sublevels
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-timestamp-if-done t
        org-id-link-to-org-use-id 'create-if-interactive
        org-startup-with-inline-images t
        org-link-abbrev-alist '(("attachment" . org-attach-expand-link))
        org-modules '(org-expiry org-tempo)
        org-export-backends (quote (
               beamer
               icalendar
               md))
        org-expiry-created-property-name "CREATED"      ; Name of property when an item is created
        org-expiry-inactive-timestamps   t              ; Don't have everything in the agenda view
        org-agenda-custom-commands '(
                ("cd" "TODO created today" tags-todo "+CREATED>\"<-1d>\"")
                ("cw" "TODO created in the last 7 days" tags-todo "+CREATED>=\"<-1w>\"")
        )
        org-refile-targets '((nil :maxlevel . 4) (org-agenda-files :maxlevel . 4))
        org-outline-path-complete-in-steps nil          ; Refile in a single go
        org-refile-use-outline-path 'file               ; Show full paths for refiling
        org-refile-allow-creating-parent-nodes t        ; Allow the Creation of refile targets dynamically
        org-capture-templates '(
                                      ("t" "TODO: create a TODO for either work or home")
                                      ("tw" "Work" entry (file (lambda () (concat org-directory "/atlassian.org" ))) "* TODO %?" :empty-lines 1)
                                      ("th" "Home" entry (file (lambda () (concat org-directory "/personal.org" ))) "* TODO %?" :empty-lines 1)
                                      ( "i" "Idea" entry (file  (lambda () (concat org-directory "/ideas.org"))) "* %?" :empty-lines 1 )
        )
        org-agenda-files (quote ("~/.asorganise/Orgmode/" "~/.asorganise/Orgmode/Archive"))
)



(defun semps/insert-created-timestamp()
    "Insert a CREATED property for each org note in the capture buffer using org-expiry.el. Reading the org-mode "
    (org-map-entries '(org-expiry-insert-created) nil nil)
    (org-map-entries '(org-id-get-create) nil nil)
)

(defun semps/create-custom-id-with-id-property(propery-name property-new-value)
  (interactive)
  (message "current id is %s" propery-name)
  (when (string= propery-name "ID")
    (message "property value is %s" property-new-value)
    (org-entry-put nil "CUSTOM_ID" property-new-value)))

(add-hook 'org-after-refile-insert-hook (lambda ()
              (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
              (auto-save-mode)))
(add-hook 'org-capture-prepare-finalize-hook 'semps/insert-created-timestamp)
(add-hook 'org-property-changed-functions 'semps/create-custom-id-with-id-property)
(add-hook 'org-mode-hook 'turn-on-flyspell)
(add-hook 'org-mode-hook 'git-auto-commit-mode)
(add-hook 'org-mode-hook (lambda ()
          (setq fill-column 120)
          (visual-fill-column-mode t)
          (visual-line-mode t)
          ))

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)
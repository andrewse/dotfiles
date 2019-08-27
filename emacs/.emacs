;; Information on this file
;; https://www.emacswiki.org/emacs/AquamacsFAQ
;; https://github.com/sirech/emacs
;; https://emacs.stackexchange.com/questions/271/what-is-the-difference-between-aquamacs-and-other-mac-versions-of-emacs
;; https://github.com/d12frosted/homebrew-emacs-plus
;; https://github.com/syl20bnr/spacemacs#macos
;; https://github.com/cpitclaudel/emacs/blob/master/etc/ORG-NEWS

;; Enable the org-mode contrib packages
;; https://emacs.stackexchange.com/questions/8182/how-to-use-org-plus-contrib/8231
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)


;; From http://stackoverflow.com/questions/803812/emacs-reopen-buffers-from-last-session-on-startup
;; Save the tabs from last time
;;(desktop-save-mode 1)

;; https://superuser.com/questions/695096/how-to-enable-flyspell-in-org-mode-by-default/695123
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Spelling.html
;; https://www.emacswiki.org/emacs/FlySpell
(add-hook 'org-mode-hook 'turn-on-flyspell)

;; Org protocol setup
;; http://orgmode.org/worg/org-contrib/org-protocol.html#orgheadline6
;; http://orgmode.org/worg/org-contrib/org-protocol.html
;;(server-start)
;;(add-to-list 'load-path "~/path/to/org/protocol/")
;;(require 'org-protocol)

;; Org-mode default location
(setq org-directory "~/.asorganise/Orgmode")

;; Make it impossible to complete a task if subtasks are not done
(setq org-enforce-todo-dependencies t)

;; Set the default location for a few things
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; Add a closed property to items when going to DONE
;; http://orgmode.org/manual/Closing-items.html
(setq org-log-done 'time)
(setq org-log-done 'note)

;; These are used to enable the bindings for org-mode
;; This came later http://orgmode.org/manual/Setting-up-capture.html#Setting-up-capture
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; You are able to define keybindings just for Org-mode via https://stackoverflow.com/questions/21773679/emacs-org-mode-file-local-key-binding#21774528


;; These are templates use to create the capture entires in org-mode
;; Categories
;; Meetings
;; Project note
;; People interaction
;; https://www.mail-archive.com/emacs-orgmode@gnu.org/msg113478.html
;; https://www.mail-archive.com/emacs-orgmode@gnu.org/msg113776.html
(setq org-capture-templates '(
    (   "w" "Work"
         entry (file (lambda () (concat org-directory "/atlassian.org" )))
	    "* %? %^g\n    SCHEDULED: %t"
	)
    (   "p" "People"
         entry (file (lambda () (concat org-directory "/people.org" )))
	    "* %^{prompt|Conversation|Scheduled 1+1} %^g\n%?"
	)
    (   "h" "Home"
         entry (file (lambda () (concat org-directory "/personal.org" )) )
	    "* %? %^g\n    SCHEDULED: %t"
	)
	(   "j" "Journal"
	    entry (file+datetree (lambda () (concat org-directory "/journal.org" )))
        "* %? \n"
    )
    ;; When writing meeting notes if they are written in Markdown then can be imported directly into Confluence
    ;; Using the + > Markup
    ;; https://confluence.atlassian.com/display/~jpaz/Markdown+syntax+guide
    ;; http://daringfireball.net/projects/markdown/syntax
    (   "m" "Meeting"
         entry (file+olp ( lambda () (concat org-directory "/atlassian.org" ) "Meetings" ))
	    "* %? \n"
	)
    
))

;; Set the place that org agenda will look for files to build the agenda based on
;; http://stackoverflow.com/questions/11384516/how-to-make-all-org-files-under-a-folder-added-in-agenda-list-automatically
;; http://superuser.com/questions/633746/loading-all-org-files-on-a-folder-to-agenda#633789
(setq org-agenda-files ( quote("~/.asorganise/Orgmode/"
			       "~/.asorganise/Orgmode/projects")))

;; From http://emacs.stackexchange.com/questions/26119/org-mode-adding-a-properties-drawer-to-a-capture-template
;; https://emacs.stackexchange.com/questions/21291/add-created-timestamp-to-logbook
;; Used to add a CREATED stamp to when items are created using the capture template
(defun add-created-date-as-property ()
  "Add CREATED property to the current item."
  (interactive)
  (org-set-property "CREATED" (concat "[" (format-time-string "%Y-%m-%d %a %H:%M") "]")))

;; (add-hook 'org-capture-before-finalize-hook 'add-created-date-as-property)

;; How to have a created property added to items
;; https://stackoverflow.com/questions/12262220/add-created-date-property-to-todos-in-org-mode#13285957
;; Allow automatically handing of created/expired meta data.

(require 'org-expiry)

;; Configure it a bit to my liking
(setq
  org-expiry-created-property-name "CREATED" ; Name of property when an item is created
  org-expiry-inactive-timestamps   t         ; Don't have everything in the agenda view
)

(defun semps/insert-created-timestamp()
  "Insert a CREATED property using org-expiry.el for TODO entries"
  (org-expiry-insert-created)
  (org-back-to-heading)
  (org-end-of-line)
  (insert " ")
)

(add-hook 'org-capture-before-finalize-hook 'semps/insert-created-timestamp)



;; http://emacs.stackexchange.com/questions/8150/show-done-items-in-current-calendar-week#8163
;; http://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
;; Doesn't currently work as expected
(setq org-agenda-custom-commands
      '(("W" "Weekly Review"
         ((agenda "" ((org-agenda-ndays 7))) ;; review upcoming deadlines and appointments type "l" in the agenda to review logged items 
          (stuck "") ;; review stuck projects as designated by org-stuck-projects
          (todo "DONE") ;; review all projects (assuming you use todo keywords to designate projects)
          (todo "MAYBE") ;; review someday/maybe items
          (todo "WAITING"))) ;; review waiting items 
         ;; ...other commands here
        ))

;; Specify some custom refile targets so that we can choose to refile across files as well as to any levels within the
;; current file
;; https://www.reddit.com/r/emacs/comments/4366f9/how_do_orgrefiletargets_work/
(setq org-refile-targets '((nil :maxlevel . 9)
                                (org-agenda-files :maxlevel . 9)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling

;; auto save on refile
(add-hook 'org-after-refile-insert-hook
        (lambda ()
              (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
(auto-save-mode)))
;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm
     auto-completion
     ;; better-defaults
     emacs-lisp
     git
     markdown
     org
     graphviz
     yaml
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     spell-checking
     semps
     finance
     syntax-checking
     ;; version-control
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; https://emacs.stackexchange.com/questions/12404/org-drill-doesnt-load-with-spacemacs-configuration/12424#12424
   dotspacemacs-additional-packages '(
        org-plus-contrib
        org-ref
        emojify
        company-emoji
        org-trello
   )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(
     ;; https://github.com/syl20bnr/spacemacs/issues/9374 not working in Spacemacs 0.200.13
     org-projectile
   )
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-but-keep-unused))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'emacs
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   ;;dotspacemacs-default-font '("Source Code Pro"
   ;;                           :size 13
   ;;                            :weight normal
   ;;                            :width normal
   ;;                            :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc?
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration executes.
This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."


(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))


;;(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t) ; Org-mode's repository
(add-to-list 'package-archives '("melpa-stable-org-trello" . "http://melpa-stable.milkbox.net/packages/") t) ; org-trello
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

;; This enables Emacs to save in UTF-8 encoding by default as well as creating a character map
;; That will autoconvert sequences into the UTF-8 equiv

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(define-abbrev-table 'global-abbrev-table '(
    ("alpha" "α")
    ("inf" "∞")
    ("ar" "→")
    ))

;; https://emacs.stackexchange.com/questions/18404/can-i-display-org-mode-attachments-as-inline-images-in-my-document
(require 'org-attach)
(setq org-link-abbrev-alist '(("att" . org-attach-expand-link)))
(setq org-startup-with-inline-images t)


;; This doesn't seem to be working at the moment and isn't enabled after spacemacs loads
(abbrev-mode 1) ; turn on abbrev mode

;; https://github.com/syl20bnr/spacemacs/issues/9549
(require 'helm-bookmark)

;; Information on this file
;; https://www.emacswiki.org/emacs/AquamacsFAQ
;; https://github.com/sirech/emacs
;; https://emacs.stackexchange.com/questions/271/what-is-the-difference-between-aquamacs-and-other-mac-versions-of-emacs
;; https://github.com/d12frosted/homebrew-emacs-plus
;; https://github.com/syl20bnr/spacemacs#macos
;; https://github.com/cpitclaudel/emacs/blob/master/etc/ORG-NEWS


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

;; Customise my org-mode setup to it feels juuuuust right
;; https://orgmode.org/worg/org-configs/org-customization-guide.html
;; 
(setq   org-directory "~/.asorganise/Orgmode"
        org-default-notes-file (concat org-directory "/notes.org")
        org-attach-directory (concat org-directory "/attachments")
        ispell-program-name "aspell"
        org-archive-location (concat org-directory "/archive/%s_archive::")
        org-special-ctrl-a/e t
        org-return-follows-link t
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t
        org-log-done 'time
        org-log-refile 'time
        org-log-repeat 'time
        org-log-reschedule 'time
        org-closed-keep-when-no-todo t
        org-agenda-text-search-extra-files (list 'agenda-archives)
        ; Bulk agenda command to remove TODO keywords from items
        org-agenda-bulk-custom-functions '((?C (lambda nil (org-agenda-todo ""))))
;        org-agenda-todo-ignore-scheduled, org-agenda-todo-ignore-deadlines, org-agenda-todo-ignore-timestamp
;        org-agenda-todo-ignore-with-date
                                        ;        org-agenda-todo-list-sublevels
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-timestamp-if-done t
        )


;; https://superuser.com/questions/132218/emacs-git-auto-commit-every-5-minutes
;; https://gist.github.com/defunkt/449668
;; https://github.com/ryuslash/git-auto-commit-mode

;; https://orgmode.org/worg/org-8.0.html#sec-6-1
;; Set the additional exporters to load which get loaded once the exporter dialogue is opened
(setq org-export-backends (quote (
       beamer
       confluence
       md)))
;; https://orgmode.org/worg/org-contrib/
;; https://emacs.stackexchange.com/questions/46988/easy-templates-in-org-9-2
(require 'org-tempo)
(setq org-modules '(org-expiry org-tempo) )

;;(setq org-velocity-bucket (expand-file-name "people.org" org-directory))
;;(global-set-key (kbd "C-c v") 'org-velocity)

;; Quelpa is apparently the package manager used by spacemacs
(require 'quelpa)
(quelpa '(git-auto-commit-mode :fetcher github :repo "ryuslash/git-auto-commit-mode"))
(add-hook 'org-mode-hook 'git-auto-commit-mode)

(quelpa '(visual-fill-column :fetcher github :repo "joostkremers/visual-fill-column"))
(require 'package) (package-initialize)
                                        ;(require 'visual-fill-column)
;; Copied settings to enable autofill mode for orgmode
;; https://github.com/syl20bnr/spacemacs/issues/5761
(add-hook 'org-mode-hook
        (lambda ()
          ;; Enable fill column indicator, but this requires
          ;; https://github.com/alpaker/Fill-Column-Indicator
          ;; (fci-mode t)
          ;; Turn off line numbering, it makes org so slow
          ;;(linum-mode -1)
          ;; Set fill column to 120
          (setq fill-column 90)
          (visual-fill-column-mode t)
          (visual-line-mode t)
          ;; https://stackoverflow.com/questions/3281581/how-to-word-wrap-in-emacs
          ;; https://stackoverflow.com/questions/8772488/emacs-word-wrap-at-specific-column-number
          ;; Enable automatic line wrapping at fill column
          ;;(auto-fill-mode t)
          ))

;; This isn't working in org-mode after a package update in Jan 2019
(global-set-key (kbd "M-<tab>") 'flyspell-auto-correct-word)

(require 'org-trello)
(custom-set-variables '(org-trello-files '("~/.asorganise/Orgmode/house-of-the-rising-moo.org")))


;; You are able to define keybindings just for Org-mode via
;; https://stackoverflow.com/questions/21773679/emacs-org-mode-file-local-key-binding#21774528
;; These are templates use to create the capture entires in org-mode
;; https://www.mail-archive.com/emacs-orgmode@gnu.org/msg113478.html
;; https://www.mail-archive.com/emacs-orgmode@gnu.org/msg113776.html
;; http://orgmode.org/manual/Template-elements.html#Template-elements
;; Multi line template keys
;; https://lists.gnu.org/archive/html/emacs-orgmode/2017-01/msg00382.html
(setq org-capture-templates '(
	(   "t" "TODO: create a TODO for either work or home"
    )
        (   "tw" "Work"
             entry (file (lambda () (concat org-directory "/atlassian.org" )))
            "* TODO %?"
            :empty-lines 1
        )
        (   "th" "Home"
             entry (file (lambda () (concat org-directory "/personal.org" )))
            "* TODO %?"
            :empty-lines 1
        )
    (   "p" "People" )
        (   "po" "One on One"
             entry (file (lambda () (concat org-directory "/people.org" )))
            "* Scheduled One on One %U :oneonone:%^G\n%?"
            :empty-lines 1
        )
        (   "pn" "Note"
             entry (file (lambda () (concat org-directory "/people.org" )))
            "* Note %U %? :note:%^g"
            :empty-lines 1
        )
        (   "pf" "Feedback"
             entry (file (lambda () (concat org-directory "/people.org" )))
            "* Feedback %U %? :feedback:%^g"
            :empty-lines 1
        )
	(   "j" "Journal" )
    	(   "jw" "Work"
    	    entry (file+olp+datetree (lambda () (concat org-directory "/atlassian-journal.org" )))
            "* %?"
            :empty-lines 1
        )
        (   "jh" "Home"
            entry (file+olp+datetree (lambda () (concat org-directory "/personal-journal.org" )))
            "* %?"
            :empty-lines 1
        )
        (   "jm" "Meeting notes"
             entry (file+olp+datetree ( lambda () (concat org-directory "/atlassian-journal.org" )))
            "* %? :meeting:"
            :empty-lines 1
            )
        ( "i" "Idea" entry (file  (lambda () (concat org-directory "/ideas.org"))) "* %?" :empty-lines 1 )
        ( "v" "Interview Feedback" entry (file+olp+datetree ( lambda () (concat org-directory "/atlassian-journal.org" )))
          "* Interview for %^{Candidates name} for %^{Interviewing for} :interview:
Summary: 

Key:
- = Note
- -Negative
- +Positive
- /Eh? Open for discussion, based on personal preference a positive or negative

- %?"
          :empty-lines 1
          )
        ( "c" "Save CLI Command" entry (file  (lambda () (concat org-directory "/commands.org"))) "* %^{Description of command}
#+begin_src sh
 %?
#+end_src" :empty-lines 1 )
))

;; Set the place that org agenda will look for files to build the agenda based on
;; http://stackoverflow.com/questions/11384516/how-to-make-all-org-files-under-a-folder-added-in-agenda-list-automatically
;; http://superuser.com/questions/633746/loading-all-org-files-on-a-folder-to-agenda#633789
(setq org-agenda-files ( quote("~/.asorganise/Orgmode/"
			       "~/.asorganise/Orgmode/archive")))

;; From http://emacs.stackexchange.com/questions/26119/org-mode-adding-a-properties-drawer-to-a-capture-template
;; https://emacs.stackexchange.com/questions/21291/add-created-timestamp-to-logbook
;; Used to add a CREATED stamp to when items are created using the capture template
(defun add-created-date-as-property ()
  "Add CREATED property to the current item."
  (interactive)
  (org-set-property "CREATED" (concat "[" (format-time-string "%Y-%m-%d %a %H:%M") "]")))

;; (add-hook 'org-capture-before-finalize-hook 'add-created-date-as-property)

;; How to have a created property added to items
;; https://stackoverflow.com/questions/12262220/add-created-date-property-to-todos-in-org-mode
;; Allow automatically handing of created/expired meta data.
(require 'org-expiry)

(setq
  org-expiry-created-property-name "CREATED" ; Name of property when an item is created
  org-expiry-inactive-timestamps   t         ; Don't have everything in the agenda view
)

(defun semps/insert-created-timestamp()
    "Insert a CREATED property for each org note in the capture buffer using org-expiry.el. Reading the org-mode "
    (org-map-entries '(org-expiry-insert-created) nil nil)
    (org-map-entries '(org-id-get-create) nil nil)
)

;; We run in the prepare finalise hook so that the buffer is still narrowed to the capture buffer and we can iterate the
;; org items that we have in the buffer
(add-hook 'org-capture-prepare-finalize-hook 'semps/insert-created-timestamp)

;; https://emacs.stackexchange.com/questions/10597/how-to-refile-into-a-datetree
;; http://orgmode.org/manual/Using-the-property-API.html
;; https://emacs.stackexchange.com/questions/8045/org-refile-to-a-known-fixed-location
(defun semps/org-refile-to (file headline)
    "Move current headline to specified location"
    (let ((pos (save-excursion
                 (find-file file)
                 (org-find-exact-headline-in-buffer headline))))
      (org-refile nil nil (list headline file nil pos))))

(defun semps/org-refile-to-datetree()
    "Refile a subtree to a datetree corresponding to it's timestamp.

The current time is used if the entry has no timestamp. If FILE
is nil, refile in the current file."
  (interactive)
  (let* ((datetree-date (or (org-entry-get nil "CREATED" t)
                            (org-entry-get nil "TIMESTAMP" t)
                            (org-read-date t nil "now")))
         (date (org-date-to-gregorian datetree-date)))
    (semps/org-refile-to "~/.asorganise/people.org" date)))

;; http://emacs.stackexchange.com/questions/8150/show-done-items-in-current-calendar-week#8163
;; http://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
;; https://orgmode.org/worg/org-tutorials/advanced-searching.html
;; https://emacs.stackexchange.com/questions/18179/org-agenda-command-with-org-agenda-filter-by-tag-not-working
(setq org-agenda-custom-commands
    '(
        ("cd" "TODO created today" tags-todo "+CREATED>\"<-1d>\"")
        ("cw" "TODO created in the last 7 days" tags-todo "+CREATED>=\"<-1w>\"")
    ))

;; Specify some custom refile targets so that we can choose to refile across files as well as to any levels within the
;; current file
;; https://www.reddit.com/r/emacs/comments/4366f9/how_do_orgrefiletargets_work/
;; The articles suggest 9 as the level but I've found that i've never wanted to go lower than 4
;; e.g 2018/2018-01 Janurary/2018-01-01/TODO Refile all the things

(setq org-refile-targets '((nil :maxlevel . 4)
                                (org-agenda-files :maxlevel . 4)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
;; file means that the file name
(setq org-refile-use-outline-path 'file)              ; Show full paths for refiling
(setq org-refile-allow-creating-parent-nodes t)       ; Allow the Creation of refile targets dynamically

;; auto save on refile
;; https://emacs.stackexchange.com/questions/477/how-do-i-automatically-save-org-mode-buffers#483
;(add-hook 'org-after-refile-insert-hook 'org-save-all-org-buffers)

(add-hook 'org-after-refile-insert-hook
        (lambda ()
              (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
              (auto-save-mode)))

;; Bindings that allow me to move between windows via control keys inside emacs
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

; https://stackoverflow.com/questions/10838235/emacs-set-shortcut-key-only-in-major-mode
(defun semps/custom-org-mode-keybindings ()
  (global-set-key (kbd "C-M-<return>") 'org-insert-item))

(add-hook 'org-mode-hook 'semps/custom-org-mode-keybindings)

;; Sourced from https://emacs.stackexchange.com/questions/26442/
(require 'org-archive)
(require 'org-id)

(defun org-copy-task-hierarchy (filename)
    "This function copies a task/project and its parent hierarchy tree to the given filename"
    (interactive)
    (let ((this-buffer (current-buffer))
          (file (abbreviate-file-name
                (or (buffer-file-name (buffer-base-buffer))
                    (error "No file associated to buffer")))))
        (save-excursion
            ; prepare the target buffer to paste things into
            (setq infile-p (equal file (abbreviate-file-name (or filename ""))))
            (unless filename (error "Invalid target location"))
            (if (> (length filename) 0)
                (setq newfile-p (not (file-exists-p filename))
                      visiting (find-buffer-visiting filename)
                      buffer (or visiting (find-file-noselect filename)))
              (setq buffer (current-buffer)))
            (unless buffer (error "Cannot access file \"%s\"" filename))

            ; construct hierarchy structure and copy current element
            (setq elem-id (org-id-get-create)
                  id-tree (org-copy-task-hierarchy--construct-id-tree))
            (org-copy-subtree nil nil nil t)

            ; move to the target buffer
            (set-buffer buffer)
            (org-mode)
            (goto-char (point-min)) ; move point to top of the buffer

            ; iterate through id-tree to paste in elements if needed
            (setq prev-id nil)
            (while (not (eq id-tree nil))
                (let* ((tree-elem-id       (caar id-tree))
                       (tree-elem-headline (nth 1 (car id-tree)))
                       (tree-elem-contents (nth 2 (car id-tree)))
                       (tree-elem-todo     (nth 3 (car id-tree)))
                       (tree-elem-fullbody (nth 4 (car id-tree)))
                       (target-location    (org-id-find-id-in-file tree-elem-id (buffer-file-name))))
                    (if (eq target-location nil) ; id doesn't exist
                        ; insert elem-fullbody
                        (progn
                            (if (eq prev-id nil)
                                (goto-char (point-max)) ; move point to end of buffer
                              (setq prev-location (org-id-find-id-in-file prev-id (buffer-file-name)))
                              (unless prev-location
                                (error "ERROR: Could not find id %s in file %s" prev-location buffer-file-name))
                              (goto-char (cdr prev-location))
                              (org-end-of-subtree t t)) ; move point after subtrees of previous entry
                            (insert tree-elem-fullbody)
                            (save-buffer)) ; needed for org-id-find-id-in-file to find newly added IDs

                        ; id already exists, check for what's different
                        (goto-char (cdr target-location)) ; move point to target
                        (let* ((target-id (org-id-get))
                               (target-headline (org-get-heading t t))
                               (target-contents (org-get-entry-no-subtrees t))
                               (target-todo     (org-get-todo-state))
                               (match-headline-p (equal target-headline tree-elem-headline))
                               (match-contents-p (equal target-contents tree-elem-contents))
                               (match-todo-p     (equal target-todo tree-elem-todo)))
                            (if (or (and match-headline-p match-contents-p (not match-todo-p)
                                         ;(equal tree-elem-todo "DONE") (equal tree-elem-todo "COMPLETE"))
                                         (member (tree-elem-todo) org-done-keywords))
                                    (and match-headline-p (not match-contents-p))
                                    (and (not match-headline-p) match-contents-p))
                                ; update target element with tree element
                                (org-replace-full-entry tree-elem-fullbody)
                                ; check if neither title nor contents match
                              (when (and (not match-headline-p) (not match-contents-p))
                                  (error "ERROR updating id %s -
                                          title and body too different from existing" tree-elem-id)))))
                    (setq prev-id tree-elem-id))
                ; headers, content, todo state are all same; skip and go to next elem
                (setq id-tree (cdr id-tree)))

            (when (not (eq this-buffer buffer))
                (save-buffer))
            (message "Subtree copied %s" (concat "in file: " (abbreviate-file-name filename))))))

(defun org-copy-task-hierarchy--construct-id-tree ()
    "Construct and return a list/tree containing the IDs and content of the hierarchy"
    (let ((id-tree nil))
        (save-excursion
            (while (org-up-heading-safe)
                (org-back-to-heading t) ; set point to heading beginning
                ; get current element's headline, content, and TODO state
                (setq elem-id       (org-id-get-create)
                      elem-headline (org-get-heading t t)             ; ignore todo and tags
                      elem-contents (org-get-entry-no-subtrees t)  ; ignore properties
                      elem-todo     (org-get-todo-state)
                      elem-fullbody (org-get-full-entry))
                ; build id tree in reverse in order
                (if (eq id-tree nil)
                    (setq id-tree (cons (list elem-id elem-headline elem-contents elem-todo elem-fullbody) nil))
                  (setq id-tree
                      (cons (list elem-id elem-headline elem-contents elem-todo elem-fullbody) id-tree)))
                ))
        ; append current element to tree
        (setq id-tree (nconc id-tree (cons (list (org-id-get-create)
                                                 (org-get-heading t t)
                                                 (org-get-entry-no-subtrees t)
                                                 (org-get-todo-state)
                                                 (org-get-full-entry)) nil)))
        id-tree))

(defun org-get-full-entry ()
    "Get the entry text, including heading, no subtrees.
     If no-properties is non-nil, ignore the text properties of the entry"
    (interactive)
    (save-excursion
        (let (beg end)
            (org-back-to-heading t) ; set point to heading beginning
            (setq beg (point))
            (skip-chars-forward " \t\r\n")
            (save-match-data (outline-next-heading)) ; move point to next heading line
            (setq end (point))
            (goto-char beg)
            (buffer-substring-no-properties beg end))))

(defun org-get-entry-no-subtrees (&optional no-properties)
    "Get the entry text, after heading, no subtrees.
     If no-properties is non-nil, ignore the text properties of the entry"
    (interactive)
    (save-excursion
        (let (beg end)
            (org-back-to-heading t) ; set point to heading beginning
            (setq beg (point-at-bol 2))
            (skip-chars-forward " \t\r\n")
            (save-match-data (outline-next-heading)) ; move point to next heading line
            (setq end (point))
            (goto-char beg)
            (if no-properties
                (buffer-substring-no-properties beg end)
              (buffer-substring beg end)))))

(defun org-replace-full-entry (&optional text)
    "Replace the entry text, including heading but excluding subtrees, with the given text.
     If text is nil, simply delete the entry text."
    (interactive)
    (save-excursion
        (let (beg end)
            (org-back-to-heading t) ; set point to heading beginning
            (setq beg (point))
            (skip-chars-forward " \t\r\n")
            (save-match-data (outline-next-heading)) ; move point to next heading line
            (setq end (point))
            (goto-char beg)
            (delete-region beg end)
            (when text
                (insert text)))))

;; Unused I don't trust just using this code at the moment I feel that there might be an easier alternative with modifying
;; The logic in the org-archive::org-archive-subtree command
(defun archive-log ()
    "Copy current subtree and ancestors to archive.org_archive, then archive to completed.org_archive"
    (interactive "P")
    (org-copy-task-hierarchy
        (concat "./archive-" (format-time-string "%Y%m" (current-time)) ".org_archive"))
    (org-archive-subtree-default))


; Adds the private directory to the load path. Yes this could be configured as a layer but I'm starting to think that the amount of effort that I need to put into maintaining the spacemacs configuration isn't really worth it.
(add-to-list 'load-path "~/.emacs.d/private/semps")

(require 'load-directory)

;(require 'emojify)
;(require 'company-emoji)

                                        ; https://github.com/iqbalansari/emacs-emojify
                                        ; https://github.com/dunn/company-emoji

;; Disbaling these for now as the Org-mode tags are being included in this and I really only want the
;; bodies and headlines to have this enabled
;;(add-to-list 'company-backends 'company-emoji)
;;(add-hook 'org-mode-hook #'emojify-mode)
;;(setq emojify-company-tooltips-p t)

)


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-trello-current-prefix-keybinding "C-c o")
 '(package-selected-packages
   (quote
    (visual-fill-column git-auto-commit-mode yaml-mode ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline smeargle restart-emacs rainbow-delimiters popwin persp-mode pcre2el paradox spinner orgit org-trello dash-functional request-deferred deferred org-ref pdf-tools key-chord ivy helm-bibtex biblio parsebib biblio-core tablist org-present org-pomodoro alert log4e gntp org-plus-contrib org-mime org-download org-bullets open-junk-file neotree move-text mmm-mode markdown-toc markdown-mode magit-gitflow magit-popup macrostep lorem-ipsum linum-relative link-hint ledger-mode indent-guide hydra lv hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile projectile helm-mode-manager helm-make helm-gitignore request helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag graphviz-dot-mode google-translate golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flycheck-ledger flycheck pkg-info epl flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist highlight evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit transient git-commit with-editor evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu emojify ht elisp-slime-nav dumb-jump f dash s diminish define-word company-statistics company-emoji company column-enforce-mode clean-aindent-mode bind-map bind-key auto-yasnippet yasnippet auto-highlight-symbol auto-dictionary auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

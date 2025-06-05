(use-package org
  :straight (:type built-in)
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode))
  :custom
  ;; File and Directory Settings
  (org-directory "~/org")
  (org-default-notes-file (expand-file-name "inbox.org" org-directory))
  (org-attach-id-dir (expand-file-name ".attach/" org-directory))
  
  ;; Display and Formatting
  (org-startup-indented t)
  (org-startup-folded 'content)
  (org-startup-with-inline-images t)
  (org-pretty-entities t)
  (org-hide-emphasis-markers t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)
  
  ;; Editing and Behavior
  (org-return-follows-link t)
  (org-use-speed-commands t)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-catch-invisible-edits 'smart)
  (org-ctrl-k-protect-subtree t)
  (org-yank-adjusted-subtrees t)
  
  ;; Source Block Settings
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)
  (org-src-fontify-natively t)
  (org-src-window-setup 'current-window)
  
  ;; Log and Archive Settings
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-log-state-notes-insert-after-drawers nil)
  (org-archive-location "%s_archive::datetree/")
  
  ;; Appearance
  (org-highlight-latex-and-related '(latex script entities))
  (org-ellipsis " ▾")
  
  ;; Links and Attachments
  (org-link-frame-setup
   '((file . find-file-other-window)
     (vm . vm-visit-folder-other-window)))
  (org-file-apps
   '((auto-mode . emacs)
     ("\\.pdf\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.png\\'" . default))))

(use-package org-roam
  :straight t
  :init
  (setq org-roam-directory "~/org-roam/")
  :custom
  (org-roam-v2-ack t)
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+date: %U\n#+filetags: ")
      :unnarrowed t)
     ("p" "project" plain
      (file "~/org-roam/templates/project.org")
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+title: ${title}\n#+date: %U\n#+filetags: :project:")
      :unnarrowed t)
     ("r" "reference" plain
      (file "~/org-roam/templates/reference.org")
      :target (file+head "references/${slug}.org"
                        "#+title: ${title}\n#+date: %U\n#+filetags: :reference:")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %<%I:%M %p>: %?"
      :target (file+head "%<%Y-%m-%d>.org"
                        "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n\n"))))
  :config
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :straight t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t)
  (org-roam-ui-ref-title-template "%^{citekey}: %^{title}")
  (org-roam-ui-custom-node-colors '((journal . "#00ff00")
                                   (project . "#ff0000")
                                   (reference . "#0000ff"))))

(use-package org-transclusion
  :straight t
  :after org
  :custom
  (org-transclusion-add-all-on-activate t)
  (org-transclusion-exclude-elements '(property drawer))
  :hook (org-mode . org-transclusion-mode))

(use-package org-roam-protocol
  :straight t
  :after org-roam
  :config
  ;; Protocol handlers
  (require 'org-protocol)
  
  ;; Register protocol handlers
  (add-to-list 'org-protocol-protocol-alist
               '("org-roam"
                 :protocol "roam"
                 :function org-roam-protocol-open-file))
  
  (add-to-list 'org-protocol-protocol-alist
               '("org-roam-ref"
                 :protocol "roam-ref"
                 :function org-roam-protocol-open-ref))

  ;; Custom capture templates for protocol
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain
           "%?"
           :target (file+head "references/${slug}.org"
                             "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+ROAM_REFS: ${ref}\n")
           :unnarrowed t)
          ("w" "web" plain
           ":PROPERTIES:\n:URL: ${ref}\n:END:\n\n* Source\n\n${title}\n\n* Notes\n\n%?"
           :target (file+head "web/${slug}.org"
                             "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+ROAM_REFS: ${ref}\n#+ROAM_TAGS: website\n")
           :unnarrowed t)
          ("p" "private" plain
           ":PROPERTIES:\n:URL: ${ref}\n:VISIBILITY: private\n:END:\n\n* Source\n\n${title}\n\n* Notes\n\n%?"
           :target (file+head "private/${slug}.org"
                             "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+ROAM_REFS: ${ref}\n#+ROAM_TAGS: private\n")
           :unnarrowed t)))

  ;; Desktop integration files
  (defun my/create-protocol-files ()
    "Create .desktop files for org-protocol integration."
    (interactive)
    (let* ((desktop-dir "~/.local/share/applications")
           (icon-path "/usr/share/icons/hicolor/scalable/apps/emacs.svg")
           (org-protocol-desktop
            (format "[Desktop Entry]
Name=Org Protocol
Comment=Handle org:// URLs
Keywords=org-protocol;
Exec=emacsclient %u
Icon=%s
Type=Application
Terminal=false
Categories=System;
MimeType=x-scheme-handler/org-protocol;
" icon-path))
           (org-roam-protocol-desktop
            (format "[Desktop Entry]
Name=Org Roam Protocol
Comment=Handle roam:// URLs
Keywords=org-protocol;roam;
Exec=emacsclient %u
Icon=%s
Type=Application
Terminal=false
Categories=System;
MimeType=x-scheme-handler/roam;
" icon-path)))
      ;; Create desktop files
      (make-directory desktop-dir t)
      (with-temp-file (expand-file-name "org-protocol.desktop" desktop-dir)
        (insert org-protocol-desktop))
      (with-temp-file (expand-file-name "org-roam-protocol.desktop" desktop-dir)
        (insert org-roam-protocol-desktop))
      ;; Set permissions
      (shell-command (format "chmod +x %s/org-protocol.desktop" desktop-dir))
      (shell-command (format "chmod +x %s/org-roam-protocol.desktop" desktop-dir))
      ;; Update desktop database
      (shell-command "update-desktop-database ~/.local/share/applications/")))

  ;; Browser bookmarklets
  (defun my/generate-roam-bookmarklets ()
    "Generate and display bookmarklets for org-roam-protocol."
    (interactive)
    (let ((capture-url "javascript:location.href = 'org-protocol://roam-ref?template=r&ref=' + encodeURIComponent(location.href) + '&title=' + encodeURIComponent(document.title);")
          (capture-web "javascript:location.href = 'org-protocol://roam-ref?template=w&ref=' + encodeURIComponent(location.href) + '&title=' + encodeURIComponent(document.title);")
          (capture-private "javascript:location.href = 'org-protocol://roam-ref?template=p&ref=' + encodeURIComponent(location.href) + '&title=' + encodeURIComponent(document.title);"))
      (with-current-buffer (get-buffer-create "*Org Roam Bookmarklets*")
        (erase-buffer)
        (insert "Org Roam Protocol Bookmarklets\n")
        (insert "==========================\n\n")
        (insert "Drag these links to your bookmarks bar:\n\n")
        (insert (format "Basic Capture:\n<a href=\"%s\">Roam Capture</a>\n\n" capture-url))
        (insert (format "Web Capture:\n<a href=\"%s\">Roam Web</a>\n\n" capture-web))
        (insert (format "Private Capture:\n<a href=\"%s\">Roam Private</a>\n\n" capture-private))
        (insert "\nUsage:\n")
        (insert "1. Click the bookmarklet while on any webpage\n")
        (insert "2. Emacs will open with a capture template\n")
        (insert "3. Fill in your notes and save\n\n")
        (html-mode)
        (display-buffer (current-buffer)))))

  ;; System configuration helper
  (defun my/setup-org-protocol ()
    "Setup org-protocol system integration."
    (interactive)
    ;; Create desktop files
    (my/create-protocol-files)
    ;; Generate bookmarklets
    (my/generate-roam-bookmarklets)
    ;; Display setup instructions
    (with-current-buffer (get-buffer-create "*Org Protocol Setup*")
      (erase-buffer)
      (insert "Org Protocol Setup Instructions\n")
      (insert "===========================\n\n")
      (insert "1. Desktop files have been created in ~/.local/share/applications/\n")
      (insert "2. Bookmarklets have been generated in the *Org Roam Bookmarklets* buffer\n\n")
      (insert "Additional Setup Steps:\n")
      (insert "1. Ensure emacsclient is running with: emacs --daemon\n")
      (insert "2. Test the protocol with: emacsclient org-protocol://roam-ref?template=r&title=test\n")
      (insert "3. If using Firefox, go to about:config and set:\n")
      (insert "   - network.protocol-handler.expose.org-protocol to false\n")
      (insert "   - network.protocol-handler.expose.roam to false\n")
      (insert "\nTroubleshooting:\n")
      (insert "- Check ~/.local/share/applications/mimeinfo.cache\n")
      (insert "- Verify emacsclient is in your PATH\n")
      (insert "- Check your desktop environment's protocol handler settings\n")
      (display-buffer (current-buffer))))

  ;; Initialize setup
  (when (display-graphic-p)
    (my/setup-org-protocol)))

("c" "custom" plain
 ":PROPERTIES:\n:URL: ${ref}\n:END:\n\n* Notes\n\n%?"
 :target (file+head "custom/${slug}.org"
                    "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+ROAM_REFS: ${ref}\n")
 :unnarrowed t)

(use-package org-super-agenda
  :straight t
  :custom
  (org-super-agenda-groups
   '((:name "Important"
            :priority "A")
     (:name "Quick Picks"
            :effort< "0:30")
     (:name "Next Items"
            :todo "NEXT")
     (:name "Waiting"
            :todo "WAIT")
     (:name "Projects"
            :tag "project")
     (:name "Deadlines"
            :deadline t)
     (:name "Overdue"
            :deadline past)
     (:name "Due Soon"
            :deadline future)))
  :config
  (org-super-agenda-mode))

(use-package org-web-tools
  :straight t
  :custom
  (org-web-tools-pandoc-sleep-time 1.0)
  :commands (org-web-tools-insert-link-for-url
             org-web-tools-read-url-as-org))

(use-package org-cliplink
  :straight t
  :custom
  (org-cliplink-max-length 80)
  (org-cliplink-ellipsis "...")
  :bind ("C-c i l" . org-cliplink))

(use-package toc-org
  :straight t
  :custom
  (toc-org-max-depth 3)
  (toc-org-hrefify-default "gh")
  :hook (org-mode . toc-org-mode))

(use-package org-bullets
  :straight t
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
  :hook (org-mode . org-bullets-mode))

(use-package org-modern
  :straight t
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  :custom
  (org-modern-star '("◉" "○" "●" "○" "●" "○" "●"))
  (org-modern-table-vertical 1)
  (org-modern-table-horizontal 0.2)
  (org-modern-list '((43 . "➤") (45 . "–") (42 . "•")))
  (org-modern-checkbox '((88 . "☑") (45 . "☐") (32 . "☐")))
  (org-modern-priority '((?A . "❗")
                        (?B . "⬆")
                        (?C . "⬇")))
  (org-modern-footnote
   (cons nil (cons "fn:%s" "fn:%s")))
  (org-modern-block-fringe nil)
  (org-modern-block-name
   '((t . t)
     ("src" "»" "«")
     ("example" "~" "~")
     ("quote" "❝" "❞"))))

(use-package calfw
  :straight t
  :custom
  (cfw:display-calendar-holidays t)
  (cfw:org-face-agenda-item-foreground-color "white"))

(use-package calfw-org
  :straight t
  :after calfw
  :custom
  (cfw:org-agenda-schedule-args '(:timestamp))
  (cfw:org-overwrite-default-keybinding t))

(use-package org-protocol-capture-html
  :straight t
  :after org
  :custom
  (org-protocol-capture-html-convert-command
   '("pandoc" "-f" "html" "-t" "org" "--wrap=none"))
  :config
  (require 'org-protocol-capture-html))

(use-package ox
  :straight (:type built-in)
  :after org
  :custom
  ;; General export settings
  (org-export-with-toc t)
  (org-export-with-section-numbers t)
  (org-export-with-smart-quotes t)
  (org-export-with-sub-superscripts '{})
  (org-export-with-timestamps t)
  
  ;; HTML export settings
  (org-html-doctype "html5")
  (org-html-html5-fancy t)
  (org-html-validation-link nil)
  (org-html-head-include-scripts nil)
  (org-html-head-include-default-style nil)
  
  ;; LaTeX export settings
  (org-latex-compiler "xelatex")
  (org-latex-packages-alist '(("" "minted" t)
                             ("" "fontspec" t)))
  (org-latex-src-block-backend 'minted)
  
  ;; Markdown export settings
  (org-md-headline-style 'atx))

(use-package org
  :straight (:type built-in)
  :custom
  (org-capture-templates
   `(;; Tasks
     ("t" "Tasks")
     ("tt" "Task" entry
      (file+headline org-default-notes-file "Tasks")
      ,(concat "* TODO %?\n"
               ":PROPERTIES:\n"
               ":CREATED: %U\n"
               ":END:\n"
               "%i\n"
               "%a"))
     ("tp" "Project" entry
      (file+headline ,(expand-file-name "projects.org" org-directory) "Projects")
      ,(concat "* %^{Project name}\n"
               ":PROPERTIES:\n"
               ":CREATED: %U\n"
               ":CATEGORY: %\\1\n"
               ":END:\n\n"
               "** Overview\n%?\n\n"
               "** Tasks\n\n"
               "** Notes\n"))
     
     ;; Notes
     ("n" "Notes")
     ("nn" "Note" entry
      (file+headline org-default-notes-file "Notes")
      ,(concat "* %?\n"
               ":PROPERTIES:\n"
               ":CREATED: %U\n"
               ":END:\n"
               "%i\n"))
     ("nr" "Reference" entry
      (file+headline ,(expand-file-name "references.org" org-directory) "References")
      ,(concat "* %^{Title}\n"
               ":PROPERTIES:\n"
               ":CREATED: %U\n"
               ":URL: %^{URL}\n"
               ":END:\n\n"
               "%?"))
     
     ;; Journal
     ("j" "Journal")
     ("jj" "Journal" entry
      (file+olp+datetree ,(expand-file-name "journal.org" org-directory))
      ,(concat "* %<%I:%M %p> %?\n"
               ":PROPERTIES:\n"
               ":CREATED: %U\n"
               ":END:\n"
               "%i\n"))
     ("jm" "Meeting" entry
      (file+olp+datetree ,(expand-file-name "meetings.org" org-directory))
      ,(concat "* %^{Meeting name} %^g\n"
               ":PROPERTIES:\n"
               ":CREATED: %U\n"
               ":ATTENDEES: %^{Attendees}p\n"
               ":END:\n\n"
               "** Agenda\n%?\n\n"
               "** Notes\n\n"
               "** Action Items\n\n"
               "** Next Steps\n"))
     
     ;; Ideas
     ("i" "Ideas")
     ("ii" "Idea" entry
      (file+headline ,(expand-file-name "ideas.org" org-directory) "Ideas")
      ,(concat "* %^{Idea title}\n"
               ":PROPERTIES:\n"
               ":CREATED: %U\n"
               ":CATEGORY: %^{Category|Personal|Work|Project|Other}\n"
               ":END:\n\n"
               "** Description\n%?\n\n"
               "** Potential Impact\n\n"
               "** Next Steps\n")))))

(use-package org-ql
  :straight t
  :after org
  :custom
  ;; Cache settings
  (org-ql-cache-duration 86400) ; Cache for 24 hours
  (org-ql-results-buffer-name "*Org QL Results*") ; Results buffer name
  (org-ql-view-buffer-name "*Org QL View*") ; View buffer name
  (org-ql-view-sidebar-width 40) ; Sidebar width in columns
  
  ;; Display settings
  (org-ql-view-display-buffer-action
   '((display-buffer-reuse-window
      display-buffer-in-side-window)
     (side . right)
     (window-width . 0.3)))
  
  ;; Default views
  (org-ql-views
   '(("Upcoming deadlines"
      :query (and (todo)
                 (deadline auto)
                 (not (done)))
      :sort (deadline)
      :title "Upcoming deadlines")
     ("Unscheduled TODOs"
      :query (and (todo)
                 (not (scheduled))
                 (not (deadline)))
      :sort (priority)
      :title "Unscheduled TODOs")
     ("Priority tasks"
      :query (and (todo)
                 (priority >= "B"))
      :sort (priority date)
      :title "Priority tasks")))
  
  :config
  ;; Custom search dispatcher
  (defun my/org-ql-search-all ()
    "Search across all org files in org-directory."
    (interactive)
    (org-ql-search
     (directory-files-recursively org-directory "\\.org$")
     (read-string "Org QL query: "))))

(use-package org-drill
  :straight t
  :after org
  :custom
  ;; Session settings
  (org-drill-add-random-noise-to-intervals-p t)
  (org-drill-adjust-intervals-for-early-and-late-repetitions-p t)
  (org-drill-learn-fraction 0.25)
  (org-drill-maximum-items-per-session 30)
  (org-drill-maximum-duration 20) ; minutes
  (org-drill-scope 'directory)
  
  ;; Algorithm settings
  (org-drill-spaced-repetition-algorithm 'sm2)
  (org-drill-sm5-initial-interval 4.0)
  (org-drill-forgotten-inter val 10)
  
  ;; Display settings
  (org-drill-hide-item-headings-p nil)
  (org-drill-left-cloze-delimiter "[")
  (org-drill-right-cloze-delimiter "]")
  (org-drill-use-visible-cloze-face-p t)
  
  ;; Save settings
  (org-drill-save-buffers-after-drill-sessions-p t)
  (org-drill-skip-entry-if-main-heading-has-tag-p t)
  
  :config
  ;; Custom card types
  (defun my/org-drill-create-simple-card ()
    "Create a simple drill card."
    (interactive)
    (org-insert-heading)
    (insert "Card Title                    :drill:\n\n")
    (insert "* Question\n\n")
    (insert "* Answer\n")))

(use-package org-projectile
  :straight t
  :after (org projectile)
  :custom
  ;; File settings
  (org-projectile-projects-file 
   (expand-file-name "projects.org" org-directory))
  (org-projectile-per-project-filepath "TODO.org")
  (org-projectile-capture-template
   "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:PROJECT: ${project}\n:END:\n\n%i\n")
  
  ;; Projects hierarchy
  (org-projectile-projects-prefix "Projects")
  (org-projectile-subheading-selection t)
  
  ;; Integration settings
  (org-projectile-strategy 'top-level) ; or 'nested
  (org-projectile-allow-tramp-projects nil)
  
  :config
  ;; Initialize org-projectile
  (org-projectile-per-project)
  
  ;; Create the projects file if it doesn't exist
  (unless (file-exists-p org-projectile-projects-file)
    (write-region "" nil org-projectile-projects-file))
  
  ;; Add projects file to agenda
  (add-to-list 'org-agenda-files org-projectile-projects-file)
  
  ;; Custom project note creation
  (defun my/org-projectile-create-project-note ()
    "Create a new note for the current project."
    (interactive)
    (let ((project (projectile-project-name)))
      (org-capture nil "p")
      (org-set-property "PROJECT" project))))

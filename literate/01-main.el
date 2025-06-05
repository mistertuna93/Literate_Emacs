;; [[file:01-main.org::*📈 Recent Changes][📈 Recent Changes:1]]
(with-temp-buffer
  (let ((default-directory mistertuna-config--literate-path))
    (call-process "git" nil t nil "log" "-5" "--pretty=format:%h %as %s")
    (buffer-string)))
;; 📈 Recent Changes:1 ends here

;; [[file:01-main.org::*🎯 Todo Items][🎯 Todo Items:1]]
(let ((files (directory-files mistertuna-config--literate-path t "\\.org$"))
      todos)
  (dolist (file files)
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ TODO " nil t)
        (push (concat "- " (file-name-nondirectory file) ": "
                     (buffer-substring-no-properties
                      (point) (line-end-position)))
              todos))))
  (nreverse todos))
;; 🎯 Todo Items:1 ends here

;; [[file:01-main.org::*Dashboard Configuration][Dashboard Configuration:1]]
(use-package dashboard
  :straight t
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Welcome to TunaMacs")
  (setq dashboard-startup-banner 'official)
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents  . 5)
                         (projects . 5)
                         (agenda . 5)
                         (registers . 5)))
  
  ;; Custom item to show configuration files
  (defun dashboard-insert-configuration-files (list-size)
    (dashboard-insert-section
     "Configuration Files:"
     (mapcar (lambda (file)
               (let* ((path (expand-file-name file mistertuna-config--literate-path))
                      (filename (file-name-nondirectory path)))
                 (cons filename path)))
             (directory-files mistertuna-config--literate-path nil "\\.org$"))
     list-size
     'file
     "c"
     `(lambda (&rest _)
        (find-file ,el))
     (format "%s" (car el))))
  
  ;; Add the configuration files section
  (add-to-list 'dashboard-item-generators  '(config-files . dashboard-insert-configuration-files))
  (add-to-list 'dashboard-items '(config-files . 5) t)
  
  ;; Custom footer with system info
  (setq dashboard-footer-messages
        (list (format "Emacs Version: %s | System: %s | Last Updated: %s"
                     emacs-version
                     system-type
                     (format-time-string "%Y-%m-%d %H:%M:%S"))))
  
  :config
  (dashboard-setup-startup-hook))

;; Make sure dashboard works with client/server
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
;; Dashboard Configuration:1 ends here

;; [[file:01-main.org::parse-orgmode-files][parse-orgmode-files]]
(let ((table '(("Module" "filename" "enabled" "description") ("Main" "01-main.org" "yes" "The Entry point into Literate Config.") ("" "02-evilGen.org" "yes" "Evil-mode and General Keybinds.") ("UI Improvement" "20-ui.org" "yes" "UI modifications (Theme, Modeline, Icons, ETC.)") ("Editing Enhance" "30-edit.org" "yes" "Editing Enhancements") ("Navigation" "40-navigation.org" "yes" "Navigation and completion") ("" "41-snippetAbrev.org" "yes" "Adds snippets and abbreviations") ("Project Management" "50-projects.org" "yes" "Project Management and Version Control") ("Org Mode" "60-org.org" "yes" "Org-mode customization and package config") ("" "61-orgTemplate.org" "yes" "Defines Capture and Structure Templates.") ("" "62-orgCap.org" "yes" "") ("Tools" "70-tools.org" "yes" "Misc. tools (AI, DevOps, shell)") ("Ansible Tools" "71-tools-Ansible.org" "yes" "Scripting for Ansible scaffold Literate Doc") ("Keybinds" "80-keybinds.org" "no" "Setup Keymaps for Modules(Replacing w/ 02-evilGen") ("" "" "" ""))))
(defun parse-table-row (row)
  "Parse a single ROW from the org table."
  (let ((module (nth 0 row))
        (filename (nth 1 row))
        (enabled (nth 2 row))
        (description (nth 3 row)))
    (when (and filename (not (string= filename "filename")))
      (list (if (string= module "") nil module)
            (string-trim filename)
            (string-trim enabled)
            (if description (string-trim description) "")))))

(delq nil (mapcar #'parse-table-row table))
)
;; parse-orgmode-files ends here

;; [[file:01-main.org::*Module Management Functions][Module Management Functions:2]]
(defun mistertuna/update-module-list ()
  "Update the module list in the Quick Start section of 01-main.org."
  (interactive)
  (with-current-buffer (find-file-noselect 
                       (expand-file-name "01-main.org" mistertuna-config--literate-path))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "~/Documents/emacs-config/      # Configuration root" nil t)
        ;; Find the existing module list
        (forward-line 2)
        (let* ((start (point))
              (end (progn
                    (search-forward "#+end_src" nil t)
                    (line-beginning-position)))
              (modules (directory-files mistertuna-config--literate-path nil "\\.org$"))
              (module-text "├── literate/                  # Literate configuration files\n"))
          
          ;; Sort modules by name
          (setq modules (sort modules 'string<))
          
          ;; Create the module list
          (dolist (module modules)
            (unless (string= module ".")
              (setq module-text 
                    (concat module-text 
                           (format "│   ├── %-20s # %s\n" 
                                 module 
                                 (or (mistertuna/get-module-description module) ""))))))
          
          ;; Replace the old list with the new one
          (delete-region start end)
          (goto-char start)
          (insert module-text))))))

(defun mistertuna/get-module-description (module)
  "Get the description for MODULE from the table or return a default."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "|--" nil t)
      (let ((case-fold-search t))
        (when (re-search-forward (format "| [^|]+ | %s | [^|]+ | \\([^|]+\\) |" module) nil t)
          (string-trim (match-string 1)))))))

(defun mistertuna/update-module-table ()
  "Update the module table with new files from the literate directory."
  (interactive)
  (with-current-buffer (find-file-noselect 
                       (expand-file-name "01-main.org" mistertuna-config--literate-path))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "|--" nil t)
        (let* ((table-start (line-beginning-position))
               (table-end (progn 
                           (search-forward "|--" nil t)
                           (search-forward "\n\n" nil t)
                           (point)))
               (current-table (buffer-substring table-start table-end))
               (modules (directory-files mistertuna-config--literate-path nil "\\.org$"))
               (grouped-modules (mistertuna/group-modules-by-prefix modules))
               new-table)
          
          ;; Create new table header
          (setq new-table "| Module | filename | enabled | description |\n")
          (setq new-table (concat new-table "|---+---+---+---|\n"))
          
          ;; Add modules to table
          (dolist (group grouped-modules)
            (let* ((prefix (car group))
                   (files (cdr group))
                   (first-file (car files))
                   (module-name (mistertuna/get-module-name prefix))
                   (existing-enabled (mistertuna/get-existing-enabled first-file current-table))
                   (existing-desc (mistertuna/get-existing-description first-file current-table)))
              
              ;; For new modules, ask if they should be enabled
              (unless existing-enabled
                (setq existing-enabled
                      (if (y-or-n-p (format "Enable new module %s? " first-file))
                          "yes" "no"))
                (setq existing-desc
                      (read-string (format "Enter description for %s: " first-file))))
              
              ;; Add main module line
              (setq new-table
                    (concat new-table
                            (format "| %s | %s | %s | %s |\n"
                                    module-name
                                    first-file
                                    existing-enabled
                                    existing-desc)))
              
              ;; Add additional files in group
              (dolist (additional-file (cdr files))
                (setq new-table
                      (concat new-table
                              (format "|  | %s | %s | |\n"
                                      additional-file
                                      existing-enabled))))))
          
          ;; Replace old table with new one
          (delete-region table-start table-end)
          (goto-char table-start)
          (insert new-table))))))

(defun mistertuna/group-modules-by-prefix (modules)
  "Group MODULE-LIST by their numeric prefixes."
  (let ((groups '()))
    (dolist (module modules)
      (when (string-match "^\\([0-9]+\\)-" module)
        (let* ((prefix (match-string 1 module))
               (group (assoc prefix groups)))
          (if group
              (setcdr group (cons module (cdr group)))
            (push (cons prefix (list module)) groups)))))
    ;; Sort groups by prefix and sort files within each group
    (setq groups (sort groups (lambda (a b) (string< (car a) (car b)))))
    (mapc (lambda (group) 
            (setcdr group (sort (cdr group) 'string<)))
          groups)
    groups))

(defun mistertuna/get-module-name (prefix)
  "Get a human-readable name for the module PREFIX."
  (let ((prefix-names
         '(("01" . "Main Config")
           ("10" . "TunaMacs Core")
           ("20" . "UI Configuration")
           ("30" . "Editing")
           ("40" . "Navigation")
           ("50" . "Project Management")
           ("60" . "Org Mode")
           ("70" . "Tools")
           ("80" . "Key Bindings"))))
    (or (cdr (assoc prefix prefix-names))
        (format "Module %s" prefix))))

(defun mistertuna/get-existing-enabled (file table-string)
  "Get the enabled status for FILE from TABLE-STRING."
  (with-temp-buffer
    (insert table-string)
    (goto-char (point-min))
    (if (re-search-forward (format "| [^|]+ | %s | \\([^|]+\\) |" file) nil t)
        (string-trim (match-string 1))
      nil)))

(defun mistertuna/get-existing-description (file table-string)
  "Get the description for FILE from TABLE-STRING."
  (with-temp-buffer
    (insert table-string)
    (goto-char (point-min))
    (if (re-search-forward (format "| [^|]+ | %s | [^|]+ | \\([^|]+\\) |" file) nil t)
        (string-trim (match-string 1))
      "")))
;; Module Management Functions:2 ends here

;; [[file:01-main.org::load-configs][load-configs]]
(let ((files '(("Module" "filename" "enabled" "description") ("Main" "01-main.org" "yes" "The Entry point into Literate Config.") ("" "02-evilGen.org" "yes" "Evil-mode and General Keybinds.") ("UI Improvement" "20-ui.org" "yes" "UI modifications (Theme, Modeline, Icons, ETC.)") ("Editing Enhance" "30-edit.org" "yes" "Editing Enhancements") ("Navigation" "40-navigation.org" "yes" "Navigation and completion") ("" "41-snippetAbrev.org" "yes" "Adds snippets and abbreviations") ("Project Management" "50-projects.org" "yes" "Project Management and Version Control") ("Org Mode" "60-org.org" "yes" "Org-mode customization and package config") ("" "61-orgTemplate.org" "yes" "Defines Capture and Structure Templates.") ("" "62-orgCap.org" "yes" "") ("Tools" "70-tools.org" "yes" "Misc. tools (AI, DevOps, shell)") ("Ansible Tools" "71-tools-Ansible.org" "yes" "Scripting for Ansible scaffold Literate Doc") ("Keybinds" "80-keybinds.org" "no" "Setup Keymaps for Modules(Replacing w/ 02-evilGen") ("" "" "" ""))))
(dolist (record files)
  (let* ((filename (cadr record))
         (enabled (caddr record))
         (path (expand-file-name filename mistertuna-config--elisp-path)))
    (if (and (string= "yes" enabled)
             (file-readable-p path))
        (progn 
          (message "load: %s" filename)
          (org-babel-load-file path)))))
)
;; load-configs ends here

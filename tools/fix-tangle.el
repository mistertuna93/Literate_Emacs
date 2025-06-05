(defun mistertuna/get-project-root ()
  "Get the root directory of the literate config project."
  (expand-file-name "~/Documents/.emacs.default/"))

(defun mistertuna/tangle-org-file (org-file)
  "Tangle ORG-FILE into the elisp/ directory in the project root."
  (let* ((base-name (file-name-base org-file))
         (dest-dir (expand-file-name "elisp" (mistertuna/get-project-root)))
         (dest-file (expand-file-name (concat base-name ".el") dest-dir)))
    ;; Create destination directory if it doesn't exist
    (unless (file-directory-p dest-dir)
      (make-directory dest-dir t))
    ;; Ensure the org file exists before tangling
    (when (file-exists-p org-file)
      ;; Use org-babel-tangle-file with explicit source and destination
      (condition-case err
          (progn
            (org-babel-tangle-file org-file dest-file "emacs-lisp")
            (message "Tangled %s → %s" 
                    (file-relative-name org-file (mistertuna/get-project-root))
                    (file-relative-name dest-file (mistertuna/get-project-root))))
        (error
         (message "Error tangling %s: %s" 
                 (file-relative-name org-file (mistertuna/get-project-root))
                 (error-message-string err)))))))

(defun mistertuna/tangle-all-literate-org-files ()
  "Tangle all .org files in the literate directory into elisp/ directory."
  (interactive)
  (let* ((literate-dir (expand-file-name "literate/" (mistertuna/get-project-root)))
         (org-files (directory-files literate-dir t "\\.org$")))
    (dolist (f org-files)
      (when (and (file-regular-p f)
                 (not (string-match-p "\\.#" f))) ; Skip lock files
        (mistertuna/tangle-org-file f)))))

(defun mistertuna/auto-tangle-on-save ()
  "Auto-tangle the current file or all files if Main is being saved."
  (when (and (eq major-mode 'org-mode)
             (buffer-file-name)
             (string-prefix-p 
              (expand-file-name "literate/" (mistertuna/get-project-root))
              (buffer-file-name)))
    (let ((filename (buffer-file-name)))
      (when (and filename (file-exists-p filename))
        (if (string-equal (file-name-nondirectory filename) "01-main.org")
            (mistertuna/tangle-all-literate-org-files)
          (mistertuna/tangle-org-file filename))))))

;; Add the hook for auto-tangling
(add-hook 'after-save-hook #'mistertuna/auto-tangle-on-save)

;; Optional: Add a message to confirm the configuration is loaded
(message "Tangle configuration loaded - %s" 
         (format-time-string "%Y-%m-%d %H:%M:%S" (current-time) t))

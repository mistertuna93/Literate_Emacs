;;; elpaca-to-straight.el --- Migration script from elpaca to straight.el

;; Author: Copilot
;; Keywords: internal
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; This script helps migrate from elpaca to straight.el
;; To use: M-x eval-buffer, then M-x migrate-to-straight

;;; Code:

(defvar migrate-backup-dir (expand-file-name "elpaca-backup" user-emacs-directory)
  "Directory to store backup files.")

(defun migrate-backup-file (file)
  "Create a backup of FILE before modifying it."
  (let* ((backup-dir migrate-backup-dir)
         (backup-file (expand-file-name (file-name-nondirectory file) backup-dir)))
    (make-directory backup-dir t)
    (copy-file file backup-file t)))

(defun migrate-replace-in-file (file old-text new-text)
  "Replace OLD-TEXT with NEW-TEXT in FILE."
  (with-temp-file file
    (insert-file-contents file)
    (while (search-forward old-text nil t)
      (replace-match new-text nil t))))

(defvar straight-bootstrap-code
  ";; Initialize straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name \"straight/repos/straight.el/bootstrap.el\" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         \"https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el\"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomsg))

;; Configure use-package to use straight.el
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-ensure t)
")

(defun migrate-find-literate-files ()
  "Find all .org files in the literate directory."
  (directory-files-recursively
   (expand-file-name "literate" user-emacs-directory)
   "\\.org$"))

(defun migrate-to-straight ()
  "Migrate from elpaca to straight.el."
  (interactive)
  
  ;; Confirm before proceeding
  (unless (yes-or-no-p "This will migrate from elpaca to straight.el. Proceed? ")
    (user-error "Migration cancelled"))
  
  ;; Step 1: Create backups
  (message "Creating backups...")
  (dolist (file (migrate-find-literate-files))
    (migrate-backup-file file))
  
  ;; Step 2: Replace elpaca initialization in 01-main.org
  (let ((main-config (expand-file-name "literate/01-main.org" user-emacs-directory)))
    (when (file-exists-p main-config)
      (message "Updating main configuration...")
      (migrate-backup-file main-config)
      
      ;; Remove elpaca initialization and replace with straight.el
      (with-temp-file main-config
        (insert-file-contents main-config)
        (goto-char (point-min))
        (when (re-search-forward "(setq package-enable-at-startup nil)[\n\r]*" nil t)
          (let ((start (match-beginning 0)))
            (when (re-search-forward "(setq use-package-always-ensure t)" nil t)
              (let ((end (match-end 0)))
                (delete-region start end)
                (goto-char start)
                (insert straight-bootstrap-code))))))))
  
  ;; Step 3: Update use-package declarations
  (message "Updating use-package declarations...")
  (dolist (file (migrate-find-literate-files))
    (with-temp-file file
      (insert-file-contents file)
      (while (re-search-forward ":elpaca[ \t\n]+" nil t)
        (replace-match ":straight " nil t))))
  
  ;; Step 4: Clean up elpaca directory
  (when (and (file-exists-p (expand-file-name "elpaca" user-emacs-directory))
             (yes-or-no-p "Delete elpaca directory? "))
    (delete-directory (expand-file-name "elpaca" user-emacs-directory) t))
  
  ;; Final message
  (message "Migration completed! Please restart Emacs for changes to take effect.
Backups of your configuration files have been stored in %s" migrate-backup-dir))

(provide 'elpaca-to-straight)

;;; elpaca-to-straight.el ends here
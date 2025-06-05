;;; org-header-standardize.el --- Standardize org file headers in literate config

;; Author: Charles Staub
;; Created: 2025-06-04 00:15:02
;; Version: 1.0
;; Package-Requires: ((emacs "27.1") (org "9.4"))
;; Keywords: tools, org-mode
;; URL: https://github.com/mistertuna93/Literate_Emacs

;;; Commentary:
;; This script standardizes headers across literate Emacs configuration files
;; with interactive property selection and custom tag management

;;; Code:

(require 'org)
(require 'cl-lib)

(defgroup org-header-standardize nil
  "Customization group for org-header-standardize."
  :group 'org)

(defcustom org-header-default-author "mistertuna93"
  "Default author name for org files."
  :type 'string
  :group 'org-header-standardize)

(defvar org-header-property-options
  '(("header-args" . ((:tangle . ("yes" "no" "path/to/file"))
                      (:results . ("silent" "replace" "append" "prepend" "none" "output" "value" "raw" "drawer" "table"))
                      (:exports . ("code" "results" "both" "none"))
                      (:eval . ("never" "query" "never-export" "no-export"))
                      (:cache . ("yes" "no"))
                      (:comments . ("yes" "no" "link" "org" "both" "noweb"))
                      (:noweb . ("yes" "no" "tangle" "no-export" "strip-export"))
                      (:mkdirp . ("yes" "no"))
                      (:padline . ("yes" "no"))))
    ("OPTIONS" . ((:toc . ("nil" "1" "2" "3" "4" "5"))
                 (:num . ("nil" "t"))
                 (:author . ("nil" "t"))
                 (:email . ("nil" "t"))
                 (:creator . ("nil" "t"))
                 (:timestamp . ("nil" "t"))
                 (:title . ("nil" "t"))))
    ("STARTUP" . ((:showeverything . ("t" "nil"))
                 (:indent . ("t" "nil"))
                 (:overview . ("t" "nil"))
                 (:showall . ("t" "nil"))
                 (:showstars . ("t" "nil"))
                 (:inlineimages . ("t" "nil"))))
    ("PROPERTY" . ((:cache . ("yes" "no"))
                  (:results . ("silent" "replace" "none"))
                  (:exports . ("code" "results" "both" "none")))))
  "Available options for different org file properties.")

(defun org-header-get-file-title (filename)
  "Generate a title from FILENAME."
  (let* ((base (file-name-base filename))
         (name (replace-regexp-in-string "^[0-9]+-" "" base))
         (words (split-string name "-"))
         (capitalized (mapcar #'capitalize words)))
    (string-join capitalized " ")))

(defun org-header-get-description (filename)
  "Generate a default description from FILENAME."
  (let ((title (org-header-get-file-title filename)))
    (format "Configuration for %s" title)))

(defun org-header-format-datetime ()
  "Format current date and time in UTC."
  (format-time-string "%Y-%m-%d %H:%M:%S" (current-time) t))

(defun org-header-select-property-values (property)
  "Let user select values for PROPERTY from available options."
  (let* ((options (cdr (assoc property org-header-property-options)))
         (selected-options '()))
    (when options
      (dolist (option options)
        (let* ((key (car option))
               (values (cdr option))
               (prompt (format "Select value for %s %s (empty to skip): " property key))
               (completion-ignore-case t)
               (value (completing-read prompt values nil t)))
          (unless (string-empty-p value)
            (push (cons (substring (symbol-name key) 1) value) selected-options)))))
    selected-options))

(defun org-header-format-property-string (property values)
  "Format PROPERTY string with selected VALUES."
  (let ((formatted-values
         (mapconcat (lambda (kv)
                     (format ":%s %s" (car kv) (cdr kv)))
                   values " ")))
    (if (string= property "PROPERTY")
        formatted-values
      (format "%s" formatted-values))))

(defun org-header-parse-file (filename)
  "Parse existing headers from FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (let ((headers '()))
      (goto-char (point-min))
      (while (re-search-forward "^#\\+\\([A-Za-z_]+\\):\\s-*\\(.+\\)$" nil t)
        (push (cons (match-string 1) (match-string 2)) headers))
      headers)))

(defun org-header-prompt-for-headers (filename)
  "Prompt for header values for FILENAME."
  (let ((headers '()))
    ;; Basic headers
    (push (cons "TITLE" (read-string "Title: " (org-header-get-file-title filename))) headers)
    (push (cons "AUTHOR" (read-string "Author: " org-header-default-author)) headers)
    (push (cons "DATE" (read-string "Date: " (org-header-format-datetime))) headers)
    (push (cons "DESCRIPTION" (read-string "Description: " (org-header-get-description filename))) headers)
    
    ;; Complex headers with multiple options
    (dolist (property '("PROPERTY" "OPTIONS" "STARTUP"))
      (when (yes-or-no-p (format "Configure %s? " property))
        (let ((values (org-header-select-property-values property)))
          (when values
            (push (cons property (org-header-format-property-string property values)) headers)))))
    
    headers))

;;;###autoload
(defun org-standardize-file-headers (filename)
  "Standardize headers in org FILENAME with interactive prompts."
  (interactive "fOrg file to standardize: ")
  (let ((existing-headers (org-header-parse-file filename)))
    (with-current-buffer (find-file-noselect filename)
      (let ((new-headers (org-header-prompt-for-headers filename)))
        ;; Backup the file
        (copy-file filename (concat filename ".bak") t)
        
        ;; Remove existing headers
        (goto-char (point-min))
        (while (re-search-forward "^#\\+[A-Za-z_]+:.*\n" nil t)
          (replace-match ""))
        
        ;; Insert new headers
        (goto-char (point-min))
        (dolist (header new-headers)
          (let* ((key (car header))
                 (value (or (cdr header)
                          (cdr (assoc key existing-headers))
                          "")))
            (insert (format "#+%s: %s\n" key value))))
        (insert "\n")
        
        ;; Handle code blocks
        (let ((code-blocks '()))
          (org-element-map (org-element-parse-buffer) 'src-block
            (lambda (block)
              (let ((parameters (org-element-property :parameters block)))
                (when parameters
                  (push (list (org-element-property :begin block)
                            (org-element-property :end block)
                            parameters)
                        code-blocks)))))
          
          (when code-blocks
            (message "\nFound code blocks with inline headers in %s:" (file-name-nondirectory filename))
            (dolist (block code-blocks)
              (let* ((start (nth 0 block))
                     (end (nth 1 block))
                     (params (nth 2 block))
                     (choice (read-char-choice
                             (format "\nBlock at line %d with params:\n%s\n[k]eep, [r]emove inline params, [g]lobalize, or [s]kip? "
                                     (line-number-at-pos start)
                                     params)
                             '(?k ?r ?g ?s))))
                (cond
                 ((eq choice ?r)
                  (goto-char start)
                  (re-search-forward "^#\\+begin_src" end t)
                  (if (looking-at ".*$")
                      (replace-match " emacs-lisp")))
                 ((eq choice ?g)
                  (let* ((block-props (org-babel-parse-header-arguments params))
                         (existing-props (org-babel-parse-header-arguments 
                                        (cdr (assoc "PROPERTY" new-headers))))
                         (merged-props (append block-props existing-props)))
                    ;; Update global PROPERTY with merged properties
                    (setf (alist-get "PROPERTY" new-headers nil nil #'equal)
                          (org-header-format-property-string 
                           "PROPERTY" 
                           (mapcar (lambda (prop)
                                    (cons (substring (symbol-name (car prop)) 1)
                                          (cdr prop)))
                                   merged-props)))
                    ;; Remove inline properties
                    (goto-char start)
                    (re-search-forward "^#\\+begin_src" end t)
                    (if (looking-at ".*$")
                        (replace-match " emacs-lisp")))))))))))
    (message "Standardized headers in %s" filename)))

;;;###autoload
(defun org-standardize-all-headers ()
  "Standardize headers in all org files in the literate directory."
  (interactive)
  (let* ((default-directory (expand-file-name "~/Documents/.emacs.default/literate/"))
         (org-files (directory-files default-directory t "\\.org$")))
    (dolist (file org-files)
      (when (and (file-regular-p file)
                 (not (string-match-p "\\.bak$" file)))
        (message "Processing %s..." (file-name-nondirectory file))
        (org-standardize-file-headers file)))))

;; Add commands to the org-mode keymap
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c h s") #'org-standardize-file-headers)
  (define-key org-mode-map (kbd "C-c h a") #'org-standardize-all-headers))

(provide 'org-header-standardize)

;;; org-header-standardize.el ends here

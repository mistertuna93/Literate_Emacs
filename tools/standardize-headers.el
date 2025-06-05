;;; standardize-headers.el --- Standardize org file headers in literate config

;; Author: Charles Staub
;; Created: 2025-06-04 00:11:40

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

;; Property options definition
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

;; Helper functions for file names and titles
(defun org-header-get-file-title (filename)
  "Generate a title from FILENAME."
  (let* ((base (file-name-base filename))
         ;; Remove leading numbers and dashes (like "01-" from "01-main.org")
         (name (replace-regexp-in-string "^[0-9]+-" "" base))
         ;; Split on hyphens and capitalize each word
         (words (split-string name "-"))
         (capitalized (mapcar #'capitalize words)))
    (string-join capitalized " ")))

(defun org-header-get-description (filename)
  "Generate a default description from FILENAME."
  (let ((title (org-header-get-file-title filename)))
    (format "Configuration for %s" title)))

;; Date and time formatting functions
(defun org-header-format-datetime ()
  "Format current date and time in UTC."
  (format-time-string "%Y-%m-%d %H:%M:%S" (current-time) t))

(defun org-header-format-date ()
  "Format current date in UTC."
  (format-time-string "%Y-%m-%d" (current-time) t))

;; Rest of the functions remain the same...

(provide 'org-header-standardize)

;;; standardize-headers.el ends here

(defun mistertuna/update-table-links ()
  "Update the orgmode-files table with links to the actual files."
  (interactive)
  (save-excursion
    (with-current-buffer (find-file-noselect 
                         (expand-file-name "literate/01-main.org" 
                                         (mistertuna/get-project-root)))
      (goto-char (point-min))
      (when (search-forward "#+TBLNAME: orgmode-files" nil t)
        (org-table-next-row)  ; Skip header
        (org-table-next-row)  ; Skip separator
        (while (org-at-table-p)
          (org-table-goto-column 1)  ; Module column
          (when (org-table-blank-field-p)
            (org-table-next-row))
          (unless (org-table-blank-field-p)
            (let* ((module-name (substring-no-properties 
                               (org-table-get-field 1)))
                   (filename (substring-no-properties 
                            (org-table-get-field 2)))
                   (file-path (expand-file-name 
                              filename 
                              (expand-file-name "literate" 
                                              (mistertuna/get-project-root))))
                   (link (format "[[file:%s][%s]]" filename module-name)))
              (when (and (not (string-empty-p module-name))
                        (file-exists-p file-path))
                (org-table-put nil 1 link))))
          (org-table-next-row)))
      (save-buffer))))
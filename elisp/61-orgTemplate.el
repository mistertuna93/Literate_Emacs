(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/inbox.org" "Tasks")
         "* TODO %?\n  %u\n  %a")
        ("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
         "* %? :NOTE:\n  %u\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("m" "Meeting" entry (file+headline "~/org/meetings.org" "Meetings")
         "* MEETING with %? :MEETING:\n  %u\n  %a")
        ;; Metadata template for quick org headers
        ("h" "Header Metadata" entry (file "~/org/templates/headers.org")
         "* %^{Title}\n#+AUTHOR: %^{Author|Your Name}\n#+DATE: %U\n#+DESCRIPTION: %^{Description}\n#+TAGS: %^{Tags}\n\n%?")
        ))

;; Global keybinding for org-capture
(global-set-key (kbd "C-c c") #'org-capture)

(with-eval-after-load 'org
  (setq org-structure-template-alist
        '(("s" . "src")        ;; Source code block
          ("e" . "example")    ;; Example block
          ("q" . "quote")      ;; Quote block
          ("v" . "verse")      ;; Verse block
          ("c" . "center")     ;; Center block
          ("l" . "latex")      ;; LaTeX block
          ("h" . "html")       ;; HTML block
          ("a" . "ascii")      ;; ASCII art block
          ("p" . "plantuml")   ;; PlantUML block
          ("j" . "javascript") ;; JavaScript src block
          )))

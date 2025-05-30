((hello
  "Hello, " (p "name") "!")
 (defun
  "(defun " (p "name") " (" (p "args") ")\n  " (p "body") "\n)")
 (org-block
  "#+begin_src " (p "lang") "\n" r "\n#+end_src"))

(setq-default abbrev-mode t)

(define-abbrev-table 'global-abbrev-table
  '(("afaik" "as far as I know")
    ("btw"   "by the way")
    ("imho"  "in my humble opinion")
    ("ty"    "thank you")
    ("yw"    "you're welcome")
    ("pls"   "please")))

(setq save-abbrevs 'silently)
(setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))

(when (file-exists-p abbrev-file-name)
  (quietly-read-abbrev-file))

(use-package yasnippet
  :ensure t
  :hook ((prog-mode text-mode org-mode) . yas-minor-mode)
  :config
  (setq yas-snippet-dirs
        (list (expand-file-name "snippets" user-emacs-directory)))
  (yas-reload-all))

(use-package tempel
  :ensure t
  :bind (("M-+" . tempel-complete)  ;; expand or cycle templates
         ("M-*" . tempel-insert))   ;; insert without completion
  :hook ((prog-mode . tempel-setup-capf)
         (text-mode . tempel-setup-capf)))

(use-package tempel-collection
  :ensure t)

(hello
 "Hello, " (p "name") "!")

(defun
 "(defun " (p "name") " (" (p "args") ")\n  " (p "body") "\n)")

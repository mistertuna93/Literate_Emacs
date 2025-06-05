(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (projectile-mode +1)
  :config
  (setq projectile-completion-system 'default
        projectile-project-search-path '("~/projects/"))
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package magit
  :ensure t
  :commands magit-status
  :bind (("C-x g" . magit-status)))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package forge
  :after magit
  :ensure t)

(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package consult-project-extra
  :ensure t)

(use-package browse-at-remote
  :ensure t)

(use-package vc-msg
  :ensure t)

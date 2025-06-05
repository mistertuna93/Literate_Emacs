(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package smartparens
  :ensure t
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

(use-package undo-fu
  :ensure t
  :config
  (setq undo-limit 800000
        undo-strong-limit 12000000
        undo-outer-limit 120000000))

(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))

(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character))

(use-package rainbow-mode
  :ensure t
  :hook (prog-mode . rainbow-mode))

(use-package color-identifiers-mode
  :ensure t
  :hook (prog-mode . color-identifiers-mode))

(use-package avy
  :ensure t
  :bind
  ("M-s" . avy-goto-char)
  ("M-g f" . avy-goto-line))

(use-package ws-butler
  :ensure t
  :hook ((prog-mode text-mode) . ws-butler-mode))

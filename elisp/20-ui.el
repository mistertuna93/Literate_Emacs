(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 25))

(use-package golden-ratio
  :ensure t
  :config
  (golden-ratio-mode 1))

(use-package rainbow-mode
  :ensure t
  :hook (prog-mode . rainbow-mode))

(use-package highlight-indent-guides
  :ensure t
  :hook ((prog-mode yaml-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-auto-character-face-perc 20))

(use-package color-identifiers-mode
  :ensure t
  :hook (prog-mode . color-identifiers-mode))

;; Enable visual line wrapping in text modes
(add-hook 'text-mode-hook #'visual-line-mode)

;; Show column number in mode line
(column-number-mode 1)

;; Show matching parentheses
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Set fill column indicator at 80 columns
(setq-default fill-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(defun toggle-transparency ()
  "Toggle between transparent and opaque frame."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (or (not alpha) (= (cdr alpha) 100))
         '(85 . 85) '(100 . 100)))))

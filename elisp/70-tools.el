;; GPTel — Emacs client for LLMs
(use-package gptel
  :ensure t
  :commands (gptel gptel-send)
  :config
  (setq gptel-api-key "your-api-key-here"
        gptel-model "gpt-4"))

;; Aider — AI pair programming tool frontend
(use-package aider
  :ensure t
  :commands (aider-start aider-mode))

;; org-ai — Integrate LLMs with org-mode
(use-package org-ai
  :ensure t
  :commands (org-ai-mode)
  :hook (org-mode . org-ai-mode)
  :config
  (setq org-ai-default-model "gpt-4"))

;; Kubernetes — manage K8s clusters from Emacs
(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

;; Docker — manage Docker from Emacs
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

;; Dockerfile Mode — syntax highlighting and image building
(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

;; vterm — fast terminal emulator using libvterm
(use-package vterm
  :ensure t
  :commands (vterm))

;; multi-vterm — handle multiple vterm instances
(use-package multi-vterm
  :ensure t
  :after vterm
  :commands (multi-vterm))

;; eat — alternative terminal emulator
(use-package eat
  :ensure t
  :commands (eat))

;; shell-pop — toggle shell quickly
(use-package shell-pop
  :ensure t
  :commands (shell-pop)
  :config
  (setq shell-pop-shell-type '("vterm" "*vterm*" (lambda () (vterm)))
        shell-pop-term-shell "/bin/bash"
        shell-pop-universal-key "C-t"
        shell-pop-window-size 30
        shell-pop-full-span t
        shell-pop-window-position "bottom"))

;; Compile — run compilation commands
(global-set-key (kbd "C-c c") 'compile)

;; nyan-mode — nyan cat in the mode-line
(use-package nyan-mode
  :ensure t
  :init
  (nyan-mode 1))

;; gnugo — play Go inside Emacs
(use-package gnugo
  :ensure t
  :commands (gnugo))

;; pacmacs — Pac-Man clone in Emacs
(use-package pacmacs
  :ensure t
  :commands (pacmacs-start))

;; fireplace — cozy fireplace animation
(use-package fireplace
  :ensure t
  :commands (fireplace))

(map! :leader
      (:prefix ("o" . "custom tools")
       :desc "GPTel Chat" "g" #'gptel
       :desc "Aider" "a" #'aider-start
       :desc "Org AI" "o" #'org-ai-mode
       :desc "Kubernetes Dashboard" "k" #'kubernetes-overview
       :desc "Docker Status" "d" #'docker
       :desc "New VTerm" "v" #'multi-vterm
       :desc "Eat Terminal" "e" #'eat
       :desc "Shell Pop" "s" #'shell-pop
       :desc "Compile" "c" #'compile
       :desc "Nyan Mode" "n" #'nyan-mode
       :desc "Play Go" "G" #'gnugo
       :desc "Pacmacs" "p" #'pacmacs-start
       :desc "Fireplace" "f" #'fireplace))

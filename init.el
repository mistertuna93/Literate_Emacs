;;; init.el --- Init File -*- lexical-binding: t -*-

;; Copyright (C) 2025 mistertuna93

;; Author: mistertuna93
;; Created: 2025-06-05 01:09:51

;;; Commentary:
;; Main initialization file for Emacs

;;; Code:

;; Set up paths for the literate configuration
(defvar mistertuna-config--root-path
  (expand-file-name "~/Documents/.emacs.default/"))

(defvar mistertuna-config--literate-path
  (expand-file-name "literate" mistertuna-config--root-path))

(defvar mistertuna-config--elisp-path
  (expand-file-name "elisp" mistertuna-config--root-path))

;; Add elisp directory to load path
(add-to-list 'load-path mistertuna-config--elisp-path)

;; Disable the default startup screen
(setq inhibit-startup-screen t)

;; Don't create the scratch buffer
(setq initial-scratch-message nil)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install and configure use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-ensure t)

;; Install dashboard early
(use-package dashboard
  :straight t)

;; Load the main configuration
(require 'org)
(org-babel-load-file
 (expand-file-name "01-main.org" mistertuna-config--literate-path))

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; init.el ends here

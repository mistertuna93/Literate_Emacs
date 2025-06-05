;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;; Copyright (C) 2025 mistertuna93

;; Author: mistertuna93
;; Created: 2025-06-04

;;; Commentary:
;; Early initialization file for Emacs

;;; Code:

;; Prevent package.el loading packages prior to init.el loading
(setq package-enable-at-startup nil)

;; Don't load site-wide init files or any defaults, these will be set in init.el
(setq site-run-file nil)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the font.
;; By inhibiting this, we easily halve startup times with fonts that are larger
;; than the system default.
(setq frame-inhibit-implied-resize t)

;;; early-init.el ends here
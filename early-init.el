;;; early-init.el --- early-init file for Emacs -*- lexical-binding: t -*-

;; Author: Masayuki Hatta <mhatta@gnu.org>

;;; Code:

;;; Commentary:

;; Boilerplate configuration file for modern Emacs experience.

;;;
;;; Profiler (start)
;;;
(require 'profiler)
(profiler-start 'cpu)

;; Prevent package.el loading packages prior to their init-file loading (for straight.el)
(setq package-enable-at-startup nil)

;; Debugging
;;(setq debug-on-error t)
(setq warning-minimum-level :error)

;; Suppress cl warning
(setq byte-compile-warnings '(cl-functions))

;;; early-init.el ends here

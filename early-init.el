;;; early-init.el --- early-init file for Emacs -*- lexical-binding: t -*-

;; Author: Masayuki Hatta <mhatta@gnu.org>

;;; Code:

;; Prevent package.el loading packages prior to their init-file loading (for straight.el)
(setq package-enable-at-startup nil)

;; Debugging
;;(setq debug-on-error t)
(setq warning-minimum-level :error)

;;; early-init.el ends here
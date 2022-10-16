;;; package --- init.el
;;; Commentary:
;;; Code:

(setq package-enable-at-startup nil)
(setq gc-cons-threshold 128000000)
(setq read-process-output-max 16777216)

(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)

(setq initial-major-mode 'fundamental-mode)
(setq inhibit-splash-screen t)

(setq default-frame-alist '((font . "Source Code Pro-17")))

(setq visible-bell t)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(horizontal-scroll-bar-mode -1)


(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-command-modifier 'meta)
  (setq mac-allow-anti-aliasing t))

;;; early-init.el ends here

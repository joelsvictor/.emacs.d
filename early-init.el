;;; package --- init.el
;;; Commentary:
;;; Code:

(setq-default line-spacing 6)
;; (setq-default linum-format " %4d ")
(setq-default display-line-numbers-width-start 4)
(setq frame-resize-pixelwise t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq gc-cons-threshold most-positive-fixnum)
(setq native-comp-deferred-compilation nil)
(setq load-prefer-newer noninteractive)
(setq package-enable-at-startup nil)
(setq read-process-output-max 16777216)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(setq initial-major-mode 'fundamental-mode)
(setq inhibit-splash-screen t)
(setq default-frame-alist '((undecorated-rouded . t)
                            (font . "Fira Code-17")))
(setq visible-bell t)
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(horizontal-scroll-bar-mode -1)


(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-command-modifier 'meta)
  (setq mac-allow-anti-aliasing t))

;;; early-init.el ends here

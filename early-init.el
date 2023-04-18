;;; package --- init.el
;;; Commentary:
;;; Code:

(setq-default line-spacing 0.35)
(setq-default native-comp-deferred-compilation nil)
(setq-default user-full-name "Joel Victor")
(setq-default custom-safe-themes t)
(setq-default pixel-scroll-precision-mode t)
(setq frame-resize-pixelwise t)
(setq gc-cons-threshold most-positive-fixnum)
(setq load-prefer-newer noninteractive)
(setq package-enable-at-startup nil)
(setq read-process-output-max 16777216)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(setq initial-major-mode 'fundamental-mode)
(setq inhibit-splash-screen t)
(setq visible-bell t)
(add-to-list 'default-frame-alist '(undecorated-rouded . t))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (add-to-list 'default-frame-alist '(internal-border-width . 24))
(add-to-list 'default-frame-alist '(font . "Fira Code-14:regular:normal:normal"))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(add-hook 'after-init-hook
          (lambda ()
            (progn (tool-bar-mode -1)
                   (scroll-bar-mode -1)
                   (menu-bar-mode -1)
                   (horizontal-scroll-bar-mode -1)
                   (save-place-mode +1)
                   (blink-cursor-mode -1)
                   (column-number-mode +1)
                   (size-indication-mode +1)
                   (global-auto-revert-mode +1))))

(add-hook 'prog-mode-hook
          (lambda ()
            (progn
              (prettify-symbols-mode)
              (subword-mode)
              (hs-minor-mode))))

(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-command-modifier 'meta)
  (setq mac-allow-anti-aliasing t))

(setq show-paren-style 'parenthesis)
(setq cursor-type 'box)
(setq-default indent-tabs-mode nil)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("" . ,(expand-file-name "backups/" user-emacs-directory))))
(setq auto-save-default t)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'material-light t))
    ('dark (load-theme 'material t))))

(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

;;; early-init.el ends here

;;; package --- init.el
;;; Commentary:
;;; Code:

(setq gc-cons-threshold most-positive-fixnum)
(setq native-comp-deferred-compilation nil)
(setq load-prefer-newer noninteractive)
(setq package-enable-at-startup nil)
(setq read-process-output-max 16777216)
(setq straight-check-for-modifications 'live-with-find)
(setq use-package-enable-imenu-support t)
(setq use-package-verbose t)
(setq straight-vc-git-default-protocol 'ssh)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(setq initial-major-mode 'fundamental-mode)
(setq inhibit-splash-screen t)
(setq default-frame-alist '((font . "Source Code Pro-17")))
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


(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  ;; This takes a second
  (load bootstrap-file nil 'nomessage))


(straight-use-package 'use-package)
(straight-use-package 'use-package-ensure-system-package)
;;; early-init.el ends here

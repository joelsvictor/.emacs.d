;;;
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (setq mac-right-command-modifier 'hyper)
  (setq mac-allow-anti-aliasing t))

(add-hook 'after-init-hook (lambda () (progn
                                   (global-prettify-symbols-mode)
                                   (save-place-mode +1)
                                   (blink-cursor-mode -1)
                                   (global-linum-mode +1)
                                   (column-number-mode +1)
                                   (tool-bar-mode -1)
                                   (scroll-bar-mode -1)
                                   (show-paren-mode t))))


;; "-*-JetBrains Mono-bold-normal-normal-*-15-*-*-*-m-0-iso10646-"
(set-frame-font (font-spec :family "Iosevka"
                           :size 20
                           :weight 'normal))


(defalias 'yes-or-no-p 'y-or-n-p)


(add-hook 'before-save-hook 'whitespace-cleanup)


(setq cursor-type 'box)
(setq inhibit-splash-screen t)
(setq-default indent-tabs-mode nil)
(setq visible-bell t)
(setq backup-by-copying t)
(setq backup-directory-alist `(("" . ,(expand-file-name "backups/" user-emacs-directory))))
(setq auto-save-default t)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))


(package-initialize)

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu-elpa-devel" . "http://elpa.gnu.org/devel/") t)

(or (file-exists-p "~/.emacs.d/elpa/archives/melpa/archive-contents") (package-refresh-contents))

(or (package-installed-p 'use-package) (package-install 'use-package))

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode lisp-mode) . paredit-mode))

(use-package cider
  :ensure t
  :hook ((clojure-mode cider-repl-mode) . paredit-mode))

(use-package clj-refactor
  :ensure t
  :hook (clojure-mode . clj-refactor-mode))

(use-package flycheck
  :ensure t
  :pin "melpa")

(use-package lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((clojurescript-mode clojurec-mode clojure-mode) . lsp)
  :config (add-hook 'before-save-hook (lambda ()
                                        (progn (lsp-format-buffer)
                                               (lsp-organize-imports))))
  :commands lsp)

;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode)

;; (use-package lsp-treemacs
;;   :ensure t
;;   :commands lsp-treemacs-errors-list)

(use-package magit
  :ensure t)

(use-package git-gutter-fringe
  :ensure t
  :config (global-git-gutter-mode))

(load-theme 'leuven t)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package company
  :ensure t
  :init (global-company-mode)
  :config (setq company-show-numbers 'left
                company-selection-wrap-around t))

(use-package vertico
  :ensure t
  :pin "gnu-elpa-devel"
  :hook ((after-init) .  vertico-mode))

(use-package marginalia
  :ensure t
  :pin "melpa"
  :hook ((after-init) . marginalia-mode))

;; (use-package consult
;;   :ensure t
;;   :pin "gnu-elpa-devel")


(use-package embark
  :ensure t
  :pin "gnu-elpa-devel"
  :bind (("M-." . embark-dwim)
         ("C-." . embark-act)))


(use-package orderless
  :ensure t
  :pin "melpa"
  :config (setq completion-styles '(orderless)))


(use-package yaml-mode
  :ensure t
  :pin "melpa")


(use-package projectile
  :ensure t
  :config (progn
            (projectile-mode +1)
            (setq projectile-sort-order 'recentf)
            (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))


(use-package restclient
  :ensure t)

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$" "" (shell-command-to-string
                                          "$SHELL --login -c 'echo $PATH'"
                                                    ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)


;; (use-package eaf
;;   :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
;;   :custom
;;   ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
;;   (eaf-browser-continue-where-left-off t)
;;   (eaf-browser-enable-adblocker t)
;;   (browse-url-browser-function 'eaf-open-browser)
;;   :config
;;   (progn
;;     (require 'eaf-demo)
;;     (require 'eaf-file-sender)
;;     (require 'eaf-music-player)
;;     (require 'eaf-camera)
;;     (require 'eaf-rss-reader)
;;     (require 'eaf-terminal)
;;     (require 'eaf-image-viewer)
;;     (require 'eaf-vue-demo)
;;     (require 'eaf-pdf-viewer)
;;     (require 'eaf-browser)
;;     (require 'eaf-markdown-previewer)
;;     (require 'eaf-file-browser)
;;     ;; (require 'eaf-mermaid)
;;     (require 'eaf-file-manager)
;;     (require 'eaf-mindmap)
;;     (require 'eaf-video-player)
;;     (require 'eaf-org-previewer)
;;     (require 'eaf-airshare)
;;     (require 'eaf-jupyter)
;;     (require 'eaf-netease-cloud-music)
;;     (require 'eaf-system-monitor)
;;     (defalias 'browse-web #'eaf-open-browser)
;;     (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;;     (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;;     (eaf-bind-key take_photo "p" eaf-camera-keybinding)
;;     (eaf-bind-key nil "M-q" eaf-browser-keybinding))
;;   )
;; unbind, see more in the Wiki

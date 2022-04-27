;;; package --- init.el
;;; Commentary:
;;; Code:

(add-to-list 'default-frame-alist
             '(font . "JetBrains Mono-14"))


(run-at-time "10:00" nil (lambda () (load-theme 'gruvbox-light-hard t)))
(run-at-time "14:00" nil (lambda () (load-theme 'gruvbox-dark-hard t)))


(add-hook 'after-init-hook
          (lambda () (progn
                  (global-prettify-symbols-mode)
                  (save-place-mode +1)
                  (blink-cursor-mode +1)
                  (global-linum-mode +1)
                  (column-number-mode +1)
                  (tool-bar-mode -1)
                  (scroll-bar-mode -1)
                  (show-paren-mode t)
                  (subword-mode +1)
                  (savehist-mode +1))))


(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-command-modifier 'meta)
  (setq mac-allow-anti-aliasing t))


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


(require 'windmove)
(windmove-default-keybindings)


(require 'winner)
(winner-mode)


(defvar bootstrap-version)


(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'straight)

(straight-use-package '(use-package :type git :host github :repo "jwiegley/use-package"))


(setq use-package-verbose t)


(use-package gruvbox-theme
  :straight t
  :ensure t
  :config
  (load-theme 'gruvbox-light-hard t))


;; (use-package evil
;;   :straight t
;;   :ensure t
;;   :commands evil-mode
;;   :init (evil-mode)
;;   :demand t
;;   :config
;;   (evil-set-initial-state 'special-mode 'emacs)
;;   (progn (evil-set-leader 'normal (kbd "SPC"))
;;          (require 'evil))
;;   (define-key evil-ex-map "f " 'find-file)
;;   (define-key evil-ex-map "b " 'ibuffer))


(use-package paredit
  :straight t
  :ensure t
  :hook ((emacs-lisp-mode lisp-mode) . paredit-mode))


;; (use-package flycheck-clj-kondo
;;   :straight t)


;; (use-package cljstyle-mode
;;   :straight t
;;   :ensure-system-package
;;   (cljstyle . "brew install --cask cljstyle")
;;   :straight (:host github :repo "jstokes/cljstyle-mode" :branch "master"))


(use-package cider
  :straight t
  :ensure t
  :hook ((clojure-mode cider-repl-mode) . paredit-mode)
  :custom
  (cider-prompt-for-symbol nil)
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-repl-prompt-function 'cider-repl-prompt-abbreviated)
  (cider-repl-buffer-size-limit 100000)
  (nrepl-log-messages t)
  (cider-auto-test-mode t)
  :bind (:map clojure-mode-map
              ("C-c c d d" . cider-debug-defun-at-point))
  :commands cider-debug-defun-at-point)


(use-package clj-refactor
  :straight t
  :ensure t
  :hook (clojure-mode . clj-refactor-mode))


(use-package flycheck
  :straight t
  :ensure t
  :custom
  (flycheck-indication-mode 'left-margin)
  (flycheck-highlighting-mode 'symbols)
  :config
  (global-flycheck-mode))


(use-package lsp-mode
  :straight t
  :ensure t
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((clojurescript-mode clojurec-mode clojure-mode) . lsp)
  :custom
  (lsp-lens-enable t)
  (lsp-signature-auto-activate nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-enable-indentation nil) ; uncomment to use cider indentation instead of lsp
  (lsp-enable-completion-at-point nil) ; uncomment to use cider completion instead of lsp
  :commands (lsp))


(use-package lsp-ui
  :straight t
  :ensure t
  :hook ((lsp-mode) . lsp-ui-mode)
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-sideline-enable nil))


(use-package lsp-treemacs
  :straight t
  :ensure t
  :custom
  (treemacs-space-between-root-nodes nil)
  :commands lsp-treemacs-errors-list)


(use-package magit
  :straight t
  :ensure t
  :config
  ;; (evil-set-initial-state 'git-commit-mode 'emacs)
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash")))


(use-package git-gutter-fringe
  :straight t
  :ensure t
  :config (global-git-gutter-mode))


(use-package company
  :straight t
  :ensure t
  :custom
  (company-minimum-prefix-length 1)
  :init (global-company-mode)
  :config (setq company-show-numbers 'left
                company-selection-wrap-around t))


(use-package vertico
  :straight t
  :ensure t
  :hook ((after-init) .  vertico-mode))


(use-package marginalia
  :straight t
  :ensure t
  :hook ((after-init) . marginalia-mode))


(use-package consult
  :straight t
  :ensure t)


(use-package embark
  :straight t
  :ensure t
  :bind (("C-." . embark-act)))


(use-package orderless
  :straight t
  :ensure t
  :config (setq completion-styles '(orderless)))


(use-package yaml-mode
  :straight t
  :ensure t)


(use-package projectile
  :straight t
  :ensure t
  :config (progn
            (projectile-mode +1)
            (setq projectile-sort-order 'recentf)
            (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
            ;; (evil-define-key 'normal 'projectile-mode-map (kbd "<leader>p") 'projectile-command-map)
            ))


(use-package verb
  :straight t
  :ensure t)


(use-package org
  :custom
  (org-babel-clojure-backend 'cider)
  (org-babel-python-command "python3")
  (org-hide-emphasis-markers t)
  (org-src-fontify-natively t)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (ruby . t)
     ;; ( io . t )
     ;; ( erlang . t )
     (haskell . t)
     (clojure . t)
     (python . t)
     (shell . t)
     (plantuml . t)
     (sql . t)
     (verb . t))))


;; (use-package org-bullets
;;   :straight t
;;   :config
;;   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


;; (let* ((variable-tuple
;;         (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
;;               ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;               ((x-list-fonts "JetBrains Mono")         '(:font "JetBrains Mono"))
;;               ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;               ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;               ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;               (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;        (base-font-color     (face-foreground 'default nil 'default))
;;        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

;;   (custom-theme-set-faces
;;    'user
;;    `(org-level-8 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-7 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-6 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-5 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;;    `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
;;    `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
;;    `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
;;    `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

;; (custom-theme-set-faces
;;    'user
;;    '(variable-pitch ((t (:family "JetBrains Mono" :height 180 :weight thin))))
;;    '(fixed-pitch ((t ( :family "Ayuthaya" :height 160)))))

;; (add-hook 'org-mode-hook 'variable-pitch-mode)

;; (custom-theme-set-faces
;;    'user
;;    '(org-block ((t (:inherit fixed-pitch))))
;;    '(org-code ((t (:inherit (shadow fixed-pitch)))))
;;    '(org-document-info ((t (:foreground "dark orange"))))
;;    '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
;;    '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
;;    '(org-link ((t (:foreground "royal blue" :underline t))))
;;    '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;;    '(org-property-value ((t (:inherit fixed-pitch))) t)
;;    '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;;    '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
;;    '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
;;    '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))


(use-package lsp-haskell
  :straight t
  :ensure t)


(use-package haskell-mode
  :straight t
  :ensure t
  :config (setq haskell-process-type 'stack-ghci))


(use-package lsp-java
  :straight t
  :ensure t)


(use-package ansible
  :straight t
  :ensure t
  :hook
  (yaml-mode . ansible)
  :config
  (add-hook 'ansible-hook 'ansible-auto-decrypt-encrypt))


(use-package which-key
  :straight t
  :ensure t
  :config (which-key-mode))


(use-package yasnippet
  :straight t
  :ensure t
  :custom
  (yas-verbosity 2)
  (yas-wrap-around-region t)
  :config
  (yas-global-mode))


(use-package yasnippet-snippets
  :straight t
  :ensure t)


(use-package command-log-mode
  :straight t
  :ensure t
  :hook (after-init . global-command-log-mode)
  :commands (global-command-log-mode))


(use-package docker
  :straight t
  :ensure t)



(use-package dockerfile-mode
  :straight t
  :ensure t)


(use-package eldoc
  :straight t
  :ensure t
  :config (global-eldoc-mode))


(use-package json-mode
  :straight t
  :ensure t)


(use-package multiple-cursors
  :straight t
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" .  mc/mark-previous-like-this)))


(use-package groovy-mode
  :straight t
  :ensure t)


(use-package git-timemachine
  :straight t
  :ensure t)


(use-package anzu
  :straight t
  :ensure t)


(use-package auto-virtualenv
  :straight t
  :ensure t
  :config
  (add-hook 'window-configuration-change-hook 'auto-virtualenv-set-virtualenv))


(use-package tramp
  :config
  (setq tramp-default-method "ssh"))


(use-package io-mode-inf
  :straight (:host github :repo "slackorama/io-emacs"
             :branch "master"))


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

(server-start)

(toggle-frame-maximized)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;;; init.el ends here

;;; package --- init.el
;;; Commentary:
;;; Code:

(save-place-mode +1)
(blink-cursor-mode +1)
(column-number-mode +1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(add-hook 'after-init-hook (lambda () (toggle-frame-maximized)))
(add-hook 'prog-mode-hook 'prettify-symbols-mode)
(add-hook 'prog-mode-hook 'subword-mode)
(size-indication-mode +1)


(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-command-modifier 'meta)
  (setq mac-allow-anti-aliasing t))


(fset 'yes-or-no-p 'y-or-n-p)


(add-hook 'before-save-hook 'whitespace-cleanup)

(setq show-paren-style 'expression)
(setq cursor-type 'box)
(setq-default indent-tabs-mode nil)
(setq backup-by-copying t)
(setq backup-directory-alist `(("" . ,(expand-file-name "backups/" user-emacs-directory))))
(setq auto-save-default t)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))


(require 'windmove)
(windmove-default-keybindings)


(require 'winner)
(winner-mode)


(defvar bootstrap-version)


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


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


(require 'use-package)


(setq use-package-verbose t)


(use-package hi-lock
  :defer t
  :config
  (setq hi-lock-auto-select-face t)
  :bind ("C-x w p" . highlight-phrase))


(use-package zerodark-theme
  :straight t
  :ensure t
  :config
  (load-theme 'zerodark t))


;; this takes a second, this is becase of my .zshrc
(use-package exec-path-from-shell
  :straight (:type git
                   :host github
                   :repo "purcell/exec-path-from-shell"
                   :branch "master")
  :ensure t
  :config (exec-path-from-shell-initialize))


(use-package expand-region
  :straight t
  :defer t
  :config
  (pending-delete-mode)
  :bind
  ("C-@" . er/expand-region))


(use-package selectrum
  :straight t
  :defer t
  :hook (after-init . selectrum-mode))


(use-package selectrum-prescient
  :straight t
  :defer t
  :hook (selectrum-mode . selectrum-prescient-mode)
  :init
  (add-hook 'selectrum-prescient-mode-hook 'prescient-persist-mode))


(use-package ctrlf
  :straight t
  :config (ctrlf-mode +1))


(use-package apheleia
  :ensure-system-package (cljstyle . "brew install --cask cljstyle")
  :defer t
  :straight t
  :hook (prog-mode . apheleia-mode)
  :config
  (push '(cljstyle . ("cljstyle" "pipe")) apheleia-formatters)
  (setf (alist-get 'clojure-mode apheleia-mode-alist)
        '(cljstyle)))


(require 'clojure)


(use-package flycheck
  :straight t
  :defer t
  :ensure t
  :config
  (setq flycheck-indication-mode 'left-margin)
  (setq flycheck-highlighting-mode 'symbols)
  :hook (prog-mode . flycheck-mode))


(use-package paredit
  :straight t
  :defer t
  :ensure t
  :bind (("M-{" . paredit-wrap-curly)
         ("M-[" . paredit-wrap-square))
  :hook ((emacs-lisp-mode clojure-mode cider-repl-mode) . paredit-mode)
  :config (add-hook 'paredit-mode-hook 'show-paren-mode))


(use-package lsp-mode
  :straight t
  :ensure t
  :defer t
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((clojurescript-mode clojurec-mode clojure-mode) . lsp)
  :config
  (setq lsp-diagnostics-disabled-modes '(clojurescript-mode
                                         clojurec-mode
                                         clojure-mode))
  (setq lsp-lens-enable t)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-enable-indentation nil) ; uncomment to use cider indentation instead of lsp
  (setq lsp-enable-completion-at-point nil) ; uncomment to use cider completion instead of lsp
  :commands (lsp))


(use-package lsp-ui
  :straight t
  :defer t
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-modeline-code-actions-enable nil))


(use-package lsp-treemacs
  :straight t
  :ensure t
  :after (lsp)
  :defer t)


(use-package magit
  :straight t
  :defer t
  :ensure t
  :config
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash")))


(use-package git-timemachine
  :straight t
  :defer t
  :ensure t
  :after (magit))


(use-package diff-hl
  :straight t
  :defer t
  :ensure t
  :hook (prog-mode . diff-hl-mode))


(use-package company
  :straight t
  :defer t
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 3)
  (setq company-show-numbers 'right)
  (setq company-selection-wrap-around t))


(use-package marginalia
  :straight t
  :ensure t
  :defer t
  :hook (selectrum-mode . marginalia-mode))


(use-package yaml-mode
  :straight t
  :defer t
  :ensure t)


(use-package rg
  :defer t
  :straight t
  :ensure t)


(use-package projectile
  :straight t
  :ensure t
  :config
  (projectile-mode +1)
  (setq projectile-sort-order 'recentf)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


(use-package verb
  :defer t
  :mode "\\.http\\'"
  :straight t
  :ensure t)


(use-package org
  :straight t
  :ensure-system-package (plantuml . "brew install plantuml")
  :defer t
  :config
  (setq org-babel-clojure-backend 'cider)
  (setq org-babel-python-command "python3")
  (setq org-hide-emphasis-markers t)
  (setq org-src-fontify-natively t)
  (setq org-plantuml-exec-mode 'plantuml)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (ruby . t)
     (haskell . t)
     (clojure . t)
     (python . t)
     (shell . t)
     (plantuml . t)
     (sql . t)
     (verb . t))))


(use-package lsp-haskell
  :defer t
  :straight t
  :ensure t)


(use-package haskell-mode
  :defer t
  :straight t
  :ensure t
  :config (setq haskell-process-type 'stack-ghci))


(use-package lsp-java
  :defer t
  :straight t
  :ensure t)


(use-package ansible
  :straight t
  :defer t
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
  :defer t
  :straight t
  :ensure t
  :hook (prog-mode . yas-global-mode)
  :config
  (setq yas-wrap-around-region t))


(use-package yasnippet-snippets
  :after (yasnippet)
  :straight t
  :ensure t)


(use-package docker
  :defer t
  :straight t
  :ensure t)


(use-package dockerfile-mode
  :defer t
  :straight t
  :ensure t)


(use-package eldoc
  :straight t
  :ensure t
  :defer t
  :hook (prog-mode . eldoc-mode))


(use-package json-mode
  :defer t
  :straight t
  :ensure t)


(use-package multiple-cursors
  :defer t
  :straight t
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" .  mc/mark-previous-like-this)))


(use-package groovy-mode
  :defer t
  :straight t
  :ensure t)


(use-package tramp
  :defer t
  :config (setq tramp-default-method "ssh"))


(use-package io-mode-inf
  :defer t
  :straight (:host github :repo "slackorama/io-emacs"
             :branch "master"))


(server-start)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)


(setq initial-scratch-message (concat ";; Took " (emacs-init-time)
                                      " for initializing emacs. Spent "
                                      (format "%f"  gc-elapsed)
                                      " seconds performing "
                                      (format "%d" gcs-done)
                                      " GCs"))

;;; init.el ends here

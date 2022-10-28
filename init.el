;;; package --- init.el
;;; Commentary:
;;; Code:

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


(setq-default straight-check-for-modifications 'live-with-find)


(straight-use-package 'use-package)
(straight-use-package 'use-package-ensure-system-package)

(require 'use-package)
(setq use-package-enable-imenu-support t)
(setq use-package-verbose t)


(use-package hi-lock
  :defer t
  :custom
  (hi-lock-auto-select-face t)
  :bind
  ("C-c h p" . highlight-phrase))


(use-package windmove
  :defer 1
  :config
  (windmove-default-keybindings))


(use-package winner
  :defer 1
  :config
  (winner-mode))


(use-package kaolin-themes
  :straight t
  :defer t
  :custom
  (kaolin-themes-italic-comments t)
  (kaolin-themes-comments-style 'alt)
  (kaolin-themes-modeline-border t)
  :hook
  (after-init . (lambda () (load-theme 'kaolin-blossom t))))


;; this takes a second, this is becase of my .zshrc
(use-package exec-path-from-shell
  :straight (:type git
                   :host github
                   :repo "purcell/exec-path-from-shell"
                   :branch "master")
  :straight t
  :defer 1
  :config (exec-path-from-shell-initialize))


(use-package expand-region
  :straight t
  :defer t
  :config (pending-delete-mode)
  :bind ("C-c C-a" . er/expand-region))


(use-package selectrum
  :straight t
  :defer 1
  :config
  (selectrum-mode))


(use-package orderless
  :straight t
  :after (selectrum)
  :defer t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


(use-package ctrlf
  :straight t
  :defer 1
  :config (ctrlf-mode))


(use-package apheleia
  :ensure-system-package ((cljstyle . "brew install cljstyle")
                          (pg_format . "brew install pgformatter"))
  :defer t
  :straight t
  :hook
  (prog-mode . apheleia-mode)
  :config
  ;; fixme: stopped working after emacs update.
  (push '(cljstyle . ("cljstyle" "pipe")) apheleia-formatters)
  (setf (alist-get 'clojure-mode apheleia-mode-alist)
        '(cljstyle))
  (push '(pg_format . ("pg_format" "-g")) apheleia-formatters)
  (setf (alist-get 'sql-mode apheleia-mode-alist)
        '(pg_format)))


(require 'clojure)


(use-package flymake
  :defer t
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("C-c ! l" . flymake-show-project-diagnostics)))


(use-package paredit
  :straight t
  :defer t
  :bind
  (("M-{" . paredit-wrap-curly)
   ("M-[" . paredit-wrap-square))
  :hook
  (((emacs-lisp-mode clojure-mode cider-repl-mode lisp-data-mode) . paredit-mode)
   (paredit-mode . show-paren-mode)))


(use-package eglot
  :ensure-system-package
  ((clojure-lsp . "brew install clojure-lsp/brew/clojure-lsp-native")
   (sqls . "go install github.com/lighttiger2505/sqls@latest"))
  :straight t
  :defer t
  :bind (:map eglot-mode-map
              ("C-c l r r" . eglot-rename)
              ("C-c l a a" . eglot-code-actions))
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-ignored-server-capabilities '(:hoverProvider
                                       :documentOnTypeFormattingProvider
                                       :executeCommandProvider))
  :hook
  ((clojure-mode clojurec-mode clojurescript-mode sql-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(sql-mode . ("sqls"))))


(use-package magit
  :straight t
  :defer 1
  :config
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash")))


(use-package git-modes
  :straight t
  :mode (("\\.gitattributes\\'" . gitattributes-mode)
         ("\\.gitconfig\\'" . gitconfig-mode)
         ("\\.gitignore\\'" . gitignore-mode)))


(use-package git-timemachine
  :straight (:host gitlab
                   :repo "pidu/git-timemachine"
                   :fork (:host github
                                :repo "emacsmirror/git-timemachine"))
  :defer 1
  :bind (:map prog-mode-map
              ("C-c g t" . git-timemachine)))


(use-package diff-hl
  :straight t
  :defer t
  :hook (prog-mode . diff-hl-mode))


(use-package corfu
  :straight (:host github
                   :repo "minad/corfu"
                   :branch "main"
                   :files (:defaults "extensions/*.el"))
  :defer t
  :hook ((prog-mode . corfu-mode)
         (corfu-mode . corfu-history-mode))
  :bind (:map corfu-map
              ("C-q" . #'corfu-quick-insert))
  :config
  (setq corfu-on-exact-match 'quit)
  (setq corfu-cycle t)
  (setq corfu-auto t)
  (setq corfu-quit-at-boundary nil)
  (setq corfu-quit-no-match t)
  (setq corfu-scroll-margin 5))


(use-package corfu-doc
  :straight t
  :after (corfu)
  :custom
  (corfu-doc-auto nil)
  (corfu-doc-max-width 85)
  (corfu-doc-max-height 20)
  :bind (:map corfu-map
              ("C-c C-d" . #'corfu-doc-toggle)
              ("M-n" . #'corfu-doc-scroll-down)
              ("M-p" . #'corfu-doc-scroll-up))
  :hook (corfu-mode . corfu-doc-mode))


(use-package kind-icon
  :straight t
  :after (corfu)
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


(use-package emacs
  :defer 1
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)
  :bind
  ("C-c C-w" . #'world-clock)
  :custom
  (world-clock-list
   '(("Asia/Calcutta" "Pune")
     ("America/Los_Angeles" "San Francisco")
     ("America/New_York" "New York")
     ("Europe/Vienna" "Austria")
     ("Asia/Dubai" "Dubai")
     ("Australia/Perth" "Perth")
     ("Etc/UTC" "UTC"))
   (world-clock-time-format "%a, %d %b %I:%M %p %Z"))
  (use-short-answers t)
  :config
  (setq-default cursor-in-non-selected-windows nil))


(use-package marginalia
  :straight t
  :after (selectrum)
  :config (marginalia-mode))


(use-package yaml-mode
  :straight t
  :defer t
  :mode "\.ya?ml\'")


(use-package rg
  :ensure-system-package (rg . "brew install ripgrep")
  :straight t
  :defer 1
  :bind (:map global-map
              ("C-c r g" . rg)))


(use-package verb
  :defer t
  :mode "\\.http\\'"
  :straight t)


(use-package org
  :straight t
  :defer 1
  :custom
  (org-default-notes-file (expand-file-name "~/org/capture.org"))
  (org-agenda-files (list "~/org"))
  (org-bookmark-names-plist nil)
  :config
  (setq org-startup-folded t
        org-ellipsis " ↓ "
        org-startup-indented t
        org-hide-emphasis-markers t
        org-adapt-indentation t
        org-hide-leading-stars t
        org-src-fontify-natively t
        org-babel-clojure-backend 'cider
        org-babel-python-command "python3"
        org-plantuml-exec-mode 'plantuml)
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
     (verb . t)
     (shell . t)
     (js . t)))
  :bind (("C-c C-o c" . org-capture)
         ("C-c C-o a" . org-agenda)))


(use-package org-contrib
  :straight t
  :after (org))


(use-package org-bullets
  :straight t
  :after (org)
  :defer t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◯"
                             "⟶"
                             "→"
                             "•")))


(use-package org-appear
  :straight t
  :after (org)
  :hook (org-mode . org-appear-mode))


(use-package org-present
  :straight (:type git :host github :repo "rlister/org-present" :branch "master")
  :after (org)
  :hook ((org-present-mode . (lambda ()
                               (org-present-big)
                               (org-display-inline-images)
                               (org-present-hide-cursor)
                               (org-present-read-only)
                               (toggle-frame-fullscreen)))
         (org-present-mode-quit . (lambda ()
                                    (org-present-small)
                                    (org-remove-inline-images)
                                    (org-present-show-cursor)
                                    (org-present-read-write)
                                    (toggle-frame-fullscreen)))))


;; (use-package lsp-haskell
;;   :defer t
;;   :straight t)


(use-package haskell-mode
  :defer t
  :straight t
  :config (setq haskell-process-type 'stack-ghci))


;; (use-package lsp-java
;;   :defer t
;;   :straight t)


(use-package ansible
  :straight t
  :defer t
  :hook (yaml-mode . ansible)
  :hook (ansible . ansible-auto-decrypt-encrypt))


(use-package which-key
  :straight t
  :defer t
  :hook (after-init . which-key-mode))


(use-package docker
  :straight t
  :defer t)


(use-package dockerfile-mode
  :straight t
  :defer t)


(use-package eldoc
  :straight t
  :defer t
  :hook (prog-mode . eldoc-mode))


(use-package json-mode
  :defer t
  :straight t)


(use-package groovy-mode
  :defer t
  :straight t)


(use-package tramp
  :defer 1
  :config
  (setq tramp-default-method "rsync")
  (setq remote-file-name-inhibit-cache nil)
  (setq tramp-verbose 1))


(use-package io-mode-inf
  :defer t
  :straight (:host github :repo "slackorama/io-emacs"
                   :branch "master"))


(use-package csv-mode
  :defer t
  :straight t)


(use-package yasnippet
  :straight t
  :defer 1
  :config (yas-global-mode))


(use-package yasnippet-snippets
  :straight t
  :after (yasnippet))


(use-package rainbow-delimiters
  :straight t
  :defer t
  :custom (rainbow-delimiters-max-face-count 5)
  :hook ((electric-pair-mode paredit-mode) . rainbow-delimiters-mode))


(use-package rainbow-identifiers
  :straight t
  :custom (rainbow-identifiers-face-count 5)
  :defer t
  :hook (prog-mode . rainbow-identifiers-mode))


(use-package vterm
  :straight t
  :ensure-system-package ((cmake . "brew install cmake")
                          (fish . "brew install fish"))
  :bind (("C-c v t" . vterm))
  :custom (vterm-shell "/usr/local/bin/fish")
  :defer t)


(use-package treemacs
  :straight t
  :defer t
  :bind (:map global-map
              ("C-c C-t t" . treemacs))
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-git-mode)
  :config
  (treemacs-git-mode 'deferred)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))


(use-package sql
  :defer t
  :custom (sql-product 'postgres))


(use-package elec-pair
  :defer t
  :hook (sql-mode . electric-pair-mode))


(use-package avy
  :straight t
  :defer t
  :bind (:map global-map
              ("C-c a c t" . avy-goto-char-timer)))


(use-package uniquify
  :custom (uniquify-buffer-name-style 'forward))


(use-package tree-sitter
  :straight t
  :defer 1
  :config (global-tree-sitter-mode +1)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))


(use-package tree-sitter-langs
  :straight t
  :after (tree-sitter))


(use-package elisp-slime-nav
  :straight t
  :defer t
  :hook (emacs-lisp-mode . turn-on-elisp-slime-nav-mode))


(use-package logview
  :straight t
  :defer t)


(use-package plantuml-mode
  :straight t
  :ensure-system-package (plantuml)
  :defer t
  :custom(plantuml-exec-mode 'executable))


(use-package dashboard
  :straight t
  :defer t
  :hook (after-init . dashboard-setup-startup-hook)
  :custom
  (dashboard-items '((recents  . 10)
                     (bookmarks . 10)
                     (projects . 10)))
  (dashboard-projects-backend 'project-el))


(use-package flyspell
  :straight nil
  :defer t
  :ensure-system-package (hunspell aspell)
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . turn-on-flyspell)))


(use-package langtool
  :ensure-system-package (languagetool)
  :straight t
  :after (flyspell)
  :init
  (setq langtool-http-server-host "localhost"
        langtool-http-server-port 8081))


(use-package all-the-icons
  :straight t
  :defer 10
  :if (display-graphic-p)
  :init (setq inhibit-compacting-font-caches t)
  :custom (all-the-icons-fonts-subdirectory "AllTheIcons")
  :config (unless (file-directory-p "/Users/joelvictor/Library/Fonts/AllTheIcons")
            (call-interactively 'all-the-icons-install-fonts)))


(use-package all-the-icons-completion
  :straight t
  :after (all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :config (all-the-icons-completion-mode))


(use-package all-the-icons-dired
  :straight t
  :after (all-the-icons)
  :hook (dired-mode . all-the-icons-dired-mode))


(use-package ibuffer
  :defer t
  :custom
  (ibuffer-saved-filter-groups
   (quote (("Home"
            ("Clojure" (or (mode . clojure-mode)
                           (mode . clojurec-mode)
                           (mode . clojurescript-mode)))
            ("Terminals" (mode . vterm-mode))
            ("Cider REPL" (mode . cider-repl-mode))
            ("Org" (mode . org-mode))
            ("Emacs" (or
                      (name . "^\\*scratch\\*$")
                      (name . "^\\*Messages\\*$")
                      (name . "^\\*dashboard\\*$")
                      (name . "^\\*straight-process\\*$")))
            ("ELisp" (mode . emacs-lisp-mode))
            ("Dired" (mode . dired-mode))))))
  :bind ("C-x C-b" . ibuffer))


(use-package doom-modeline
  :straight t
  :custom
  (doom-modeline-hud t)
  (doom-modeline-support-imenu t)
  (doom-modeline-buffer-file-name-style 'truncate-all)
  :hook (after-init . doom-modeline-mode))


(use-package xref
  :straight nil
  :bind (:map prog-mode-map
              (("C-c x r" . xref-find-references)
               ("C-c x d" . xref-find-definitions))))


(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(add-hook 'before-save-hook 'whitespace-cleanup)


(require 'server)


(unless (server-running-p)
  (server-start))


(setq initial-scratch-message
      (concat ";; Took " (emacs-init-time)
              " for initializing emacs. Spent "
              (format "%f"  gc-elapsed)
              " seconds performing "
              (format "%d" gcs-done)
              " GC's."))

;;; init.el ends here

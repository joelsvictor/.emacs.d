;;; package --- init.el
;;; Commentary:
;;; Code:

(defvar bootstrap-version)

(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "HTTP://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
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


(use-package expand-region
  :straight t
  :defer t
  :config (pending-delete-mode)
  :bind (:map global-map
              ("C-c C-a" . er/expand-region)))


(use-package selectrum
  :straight t
  :config
  (selectrum-mode))


(use-package orderless
  :straight t
  :after (selectrum)
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


(use-package ctrlf
  :straight t
  :defer t
  :config (ctrlf-mode))


(use-package apheleia
  :defer t
  :ensure-system-package ((cljstyle . "brew install cljstyle")
                          (pg_format . "brew install pgformatter"))
  :straight t
  :hook
  (prog-mode . apheleia-mode)
  :config
  ;; FIXME: stopped working after emacs update.
  ;; (push '(cljstyle . ("cljstyle" "pipe")) apheleia-formatters)
  ;; (setf (alist-get 'clojure-mode apheleia-mode-alist)
  ;;       '(cljstyle))
  (push '(pg_format . ("pg_format"
                       "--comma-break"
                       "--wrap-comment"
                       "--nogrouping"
                       "--keep-newline")) apheleia-formatters)
  (setf (alist-get 'sql-mode apheleia-mode-alist)
        '(pg_format)))


(require 'clojure)


(use-package flymake
  :defer t
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("C-c f p" . flymake-show-project-diagnostics)
              ("C-c f l" . flymake-show-buffer-diagnostics)))


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
  :defer t
  :ensure-system-package
  ((clojure-lsp . "brew install clojure-lsp/brew/clojure-lsp-native")
   (sqls . "go install github.com/lighttiger2505/sqls@latest"))
  :straight t
  :bind (:map eglot-mode-map
              ("C-c e c a" . eglot-code-actions)
              ("C-c e r" . eglot-rename))
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  ;; (eglot-ignored-server-capabilities '(:hoverProvider
  ;;                                      :documentOnTypeFormattingProvider
  ;;                                      :executeCommandProvider))
  :hook
  ((clojure-mode clojurec-mode clojurescript-mode sql-mode haskell-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(sql-mode . ("sqls")))
  (setq-default eglot-workspace-configuration
                '((haskell
                   (plugin
                    (stan
                     (globalOn . :json-false))))))  ;; disable stan
  )

(use-package magit
  :defer t
  :straight t
  :config
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash")))


(use-package git-modes
  :defer t
  :straight t
  :mode (("\\.gitattributes\\'" . gitattributes-mode)
         ("\\.gitconfig\\'" . gitconfig-mode)
         ("\\.gitignore\\'" . gitignore-mode)))


(use-package git-timemachine
  :defer t
  :straight (:host gitlab
                   :repo "pidu/git-timemachine"
                   :fork (:host github
                                :repo "emacsmirror/git-timemachine"))
  :bind (:map prog-mode-map
              ("C-c g t" . git-timemachine)))


(use-package diff-hl
  :defer t
  :straight t
  :hook (prog-mode . diff-hl-mode))


(use-package corfu
  :defer t
  :straight (:host github
                   :repo "minad/corfu"
                   :branch "main"
                   :files (:defaults "extensions/*.el"))
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
  :defer t
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


(use-package cape
  :defer t
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  :bind (("C-c c p f" . cape-file))
  :straight t
  :after (corfu))


(use-package emacs
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
  :defer t
  :straight t
  :mode "\.ya?ml\'")


(use-package deadgrep
  :defer t
  :ensure-system-package (rg . "brew install ripgrep")
  :straight t
  :bind (:map global-map
              ("C-c r g" . deadgrep)))


(use-package verb
  :defer t
  :mode ("\\.http\\'" . verb-mode)
  :straight t
  :custom (verb-base-headers `(("User-Agent" . ,(concat (user-full-name) "@" (system-name))))))


(use-package org
  :defer t
  :straight t
  :custom
  (org-default-notes-file (expand-file-name "/Users/joelvictor/Documents/Org/capture.org"))
  (org-agenda-files (list "/Users/joelvictor/Documents/Org/"))
  (org-bookmark-names-plist nil)
  :config
  (setq org-babel-default-header-args:verb
        '((:op . "send get-body") (:wrap . "src ob-verb-response")))
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
         ("C-c C-o a" . org-agenda))
  :hook (org-babel-after-execute . (lambda ()
                                     (org-display-inline-images))))


(use-package org-contrib
  :defer t
  :straight t
  :after (org))


(use-package haskell-mode
  :defer t
  :straight t)


(use-package lsp-java
  :straight t)


(use-package ansible
  :defer t
  :straight t
  :hook (yaml-mode . ansible)
  :hook (ansible . ansible-auto-decrypt-encrypt))


(use-package which-key
  :defer t
  :straight t
  :hook (after-init . which-key-mode))


(use-package docker
  :straight t
  :defer t)


(use-package dockerfile-mode
  :straight t
  :defer t)


(use-package eldoc
  :defer t
  :straight t
  :hook (prog-mode . eldoc-mode))


(use-package json-mode
  :defer t
  :straight t
  :bind (:map json-mode-map
              ("C-c f" . json-pretty-print-buffer)))


(use-package groovy-mode
  :straight t)


(use-package tramp
  :config
  (setq tramp-default-method "rsync")
  (setq remote-file-name-inhibit-cache nil)
  (setq tramp-verbose 1))


(use-package csv-mode
  :straight t)


(use-package yasnippet
  :straight t
  :config (yas-global-mode))


(use-package yasnippet-snippets
  :straight t
  :after (yasnippet))


(use-package vterm
  :straight t
  :ensure-system-package ((cmake . "brew install cmake")
                          (fish . "brew install fish"))
  :after (project)
  :bind (("C-c v t" . vterm-other-window)
         (:map project-prefix-map
               ("t" . project-vterm)))
  :custom (vterm-shell "/opt/homebrew/bin/fish")
  :config
  (defun project-vterm ()
    "Start an inferior shell in the current project's root directory.
If a buffer already exists for running a shell in the project's root,
switch to it.  Otherwise, create a new shell buffer.
With \\[universal-argument] prefix arg, create a new inferior shell buffer even
if one already exists."
    (interactive)
    (require 'comint)
    (let* ((default-directory (project-root (project-current t)))
           (default-project-shell-name (project-prefixed-buffer-name "vterm"))
           (shell-buffer (get-buffer default-project-shell-name)))
      (if (and shell-buffer (not current-prefix-arg))
          (if (comint-check-proc shell-buffer)
              (pop-to-buffer shell-buffer (bound-and-true-p display-comint-buffer-action))
            (vterm-other-window shell-buffer))
        (vterm-other-window (generate-new-buffer-name default-project-shell-name)))))
  :hook (vterm-mode . (lambda ()
                        (goto-address-mode))))


(use-package sql
  :defer t
  :custom (sql-product 'postgres))


(use-package elec-pair
  :defer t
  :hook (sql-mode . electric-pair-mode))


(use-package avy
  :defer t
  :straight t
  :bind (:map global-map
              ("C-c a c" . avy-goto-char-timer)))


(use-package uniquify
  :custom (uniquify-buffer-name-style 'forward))


(use-package tree-sitter
  :straight t
  :config (global-tree-sitter-mode +1)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))


(use-package tree-sitter-langs
  :straight t
  :after (tree-sitter))


(use-package elisp-slime-nav
  :defer t
  :straight t
  :hook (emacs-lisp-mode . turn-on-elisp-slime-nav-mode))


(use-package logview
  :defer t
  :straight t)


(use-package plantuml-mode
  :defer t
  :straight t
  :ensure-system-package (plantuml)
  :custom (plantuml-exec-mode 'executable))


(use-package flyspell
  :defer t
  :straight nil
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


(use-package ibuffer
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
  :defer t
  :straight nil
  :bind (:map prog-mode-map
              (("C-c x r" . xref-find-references)
               ("C-c x d" . xref-find-definitions))))


(use-package goggles
  :defer t
  :straight t
  :hook (prog-mode . goggles-mode))


(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c b u") 'browse-url-at-point)
(add-hook 'before-save-hook 'whitespace-cleanup)


(use-package string-inflection
  :defer t
  :straight t
  :bind (:map prog-mode-map
              ("C-c C-s" . string-inflection-all-cycle)))


(use-package zig-mode
  :defer t
  :straight t)


(use-package material-theme
  :straight t)


(require 'server)


(unless (server-running-p)
  (server-start))

;;; init.el ends here

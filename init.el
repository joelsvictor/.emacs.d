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
(set-frame-font (font-spec :family "Ayuthaya"
			   :size 18
			   :weight 'bold))


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
(add-to-list 'package-archives '("gnu-elpa" . "http://elpa.gnu.org/") t)

(or (file-exists-p "~/.emacs.d/elpa/archives/melpa/archive-contents") (package-refresh-contents))

(or (package-installed-p 'use-package) (package-install 'use-package))

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode lisp-mode) . paredit-mode))

(use-package cider
  :ensure t
  :hook ((clojure-mode cider-repl-mode) . paredit-mode)
  :config (add-hook 'before-save-hook (lambda ()
                                        (clojure-sort-ns))))

(use-package clj-refactor
  :ensure t
  :hook (clojure-mode . clj-refactor-mode)
  ;; :config (add-hook 'before-save-hook (lambda ()
  ;;                                       (cljr-clean-ns)))
  )


(use-package flycheck
  :ensure t
  :pin "melpa")


(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (clojure-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
;; (use-package which-key
;;     :config
;;     (which-key-mode))


;; (use-package lsp-mode
;;   :ensure t
;;   :config (progn
;; 	    (add-hook 'clojure-mode-hook (lambda () (progn
;; 						 (add-hook 'before-save-hook 'lsp-format-buffer)
;; 						 (add-hook 'before-save-hook 'lsp-organize-imports)
;; 						 (lsp-mode)
;; 						 (lsp))))))

;; (use-package lsp-treemacs
;;   :ensure t
;;   :hook ((lsp-mode) . (lambda () (lsp-treemacs-sync-mode 1))))

;; (use-package lsp-ui
;;   :ensure t
;;   :config (progn
;;             (setq lsp-ui-doc-position 'top)
;;             (setq lsp-ui-sideline-show-code-actions nil)))

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

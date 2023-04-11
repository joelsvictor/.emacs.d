;;; package --- clojure.el
;;; Commentary:
;;; Code:

(require 'use-package)


(use-package cider
  :straight t
  :bind (:map clojure-mode-map
              ("C-c c d d" . cider-debug-defun-at-point))
  :commands cider-debug-defun-at-point
  :custom (cider-use-xref nil)
  :config
  (setq cider-prompt-for-symbol nil)
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-repl-prompt-function 'cider-repl-prompt-abbreviated)
  (setq cider-repl-buffer-size-limit 100000)
  (setq nrepl-log-messages t)
  (setq cider-auto-test-mode t)
  (setq cider-eldoc-display-context-dependent-info t))


(use-package flymake-kondor
  :ensure-system-package (clj-kondo  . "brew install borkdude/brew/clj-kondo")
  :straight t
  :hook (clojure-mode . flymake-kondor-setup))


(use-package clj-refactor
  :straight t
  :hook (clojure-mode . clj-refactor-mode)
  :config
  ;; disable clj-refactor adding ns to blank files
  (setq cljr-add-ns-to-blank-clj-files nil))


(provide 'clojure)
;;; clojure.el ends here

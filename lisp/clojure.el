;;; package --- clojure.el
;;; Commentary:
;;; Code:

(require 'use-package)


(use-package cider
  :defer 60
  :straight t
  :ensure t
  :bind (:map clojure-mode-map
              ("C-c c d d" . cider-debug-defun-at-point))
  :commands cider-debug-defun-at-point
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
  :defer t
  :after (flymake cider)
  :hook (clojure-mode . flymake-kondor-setup))


(use-package clj-refactor
  :defer t
  :straight t
  :ensure t
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (setq cljr-add-ns-to-blank-clj-files nil) ; disable clj-refactor adding ns to blank files
  )


(provide 'clojure)
;;; clojure.el ends here

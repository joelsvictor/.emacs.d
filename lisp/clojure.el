;;; package --- clojure.el
;;; Commentary:
;;; Code:

(require 'use-package)


(use-package cider
  :defer t
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
  (setq cider-auto-test-mode t))


(use-package flycheck-clj-kondo
  :after (flycheck)
  :straight t
  :ensure-system-package (clj-kondo  . "brew install borkdude/brew/clj-kondo"))


(use-package clj-refactor
  :defer t
  :straight t
  :ensure t
  :hook (clojure-mode . clj-refactor-mode))


(provide 'clojure)
;;; clojure.el ends here

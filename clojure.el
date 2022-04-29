;;; package --- clojure.el
;;; Commentary:
;;; Code:

(require 'use-package)


(use-package cider
  :straight t
  :ensure t
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

(use-package paredit
  :straight t
  :ensure t
  :hook ((emacs-lisp-mode lisp-mode clojure-mode cider-repl-mode) . paredit-mode))


(use-package flycheck-clj-kondo
  :straight t
  :ensure-system-package (clj-kondo  . "brew install borkdude/brew/clj-kondo"))


(use-package clj-refactor
  :straight t
  :ensure t
  :hook (clojure-mode . clj-refactor-mode))


(provide 'clojure)
;;; clojure.el ends here

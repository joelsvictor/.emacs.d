;;; package --- init.el
;;; Commentary:
;;; Code:

(setq package-enable-at-startup nil)
(setq gc-cons-threshold 1024000000)
(setq read-process-output-max 16777216)
(add-to-list 'default-frame-alist
             '(font . "JetBrains Mono-14"))

;;; early-init.el ends here

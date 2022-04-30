;;; package --- init.el
;;; Commentary:
;;; Code:

(setq package-enable-at-startup nil)
(setq gc-cons-threshold 1024000000)
(setq read-process-output-max 16777216)
(add-to-list 'default-frame-alist
             '(font . "JetBrains Mono-14"))
(setq-default mode-line-format
              '("%e"
                (:propertize
                 ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
                 display
                 (min-width
                  (5.0)))
                mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position (vc-mode vc-mode)
                mode-line-misc-info
                " [%I]"))
;;; early-init.el ends here

;;; package --- init.el
;;; Commentary:
;;; Code:

(defalias 'yes-or-no-p 'y-or-n-p)
(setq package-enable-at-startup nil)
(setq gc-cons-threshold 1024000000)
(setq read-process-output-max 16777216)

;;; early-init.el ends here

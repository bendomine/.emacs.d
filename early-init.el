;;; early-init.el --- Pre-frame initialization  -*- lexical-binding: t; -*-

;; Raise the GC threshold during startup so loading ~50 packages doesn't
;; trigger constant collections; drop it back once Emacs is idle.
(setq gc-cons-threshold (* 100 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024))))

;; Apply basic frame parameters before the first frame is created, so there's
;; no visible flash of a toolbar or light-appearance frame before init.el runs.
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(ns-appearance . dark) default-frame-alist)

;;; early-init.el ends here

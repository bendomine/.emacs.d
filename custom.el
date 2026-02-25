(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor-type 'bar)
 '(custom-safe-themes
   '("8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098"
     "dd4582661a1c6b865a33b89312c97a13a3885dc95992e2e5fc57456b4c545176"
     default))
 '(electric-quote-mode t)
 '(fringe-mode '(1 . 1) nil (fringe))
 '(inhibit-startup-screen t)
 '(org-babel-python-command "python3")
 '(org-confirm-babel-evaluate nil)
 '(org-format-latex-options
   '(:foreground default :background default :scale 1.55 :html-foreground
		 "Black" :html-background "Transparent" :html-scale
		 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-hide-emphasis-markers t)
 '(org-highlight-latex-and-related '(native latex script entities))
 '(org-preview-latex-default-process 'dvisvgm)
 '(org-return-follows-link t)
 '(org-safe-remote-resources '("\\`https://imgs\\.xkcd\\.com\\(?:/\\|\\'\\)"))
 '(org-support-shift-select t)
 '(package-selected-packages
   '(auctex cdlatex centaur-tabs consult consult-projectile corfu
	    doom-modeline doom-themes emmet-mode exec-path-from-shell
	    flycheck highlight-indent-guides lsp-treemacs lsp-ui magit
	    marginalia multiple-cursors olivetti orderless org-modern
	    org-roam-ui treemacs-projectile treesit-auto valign
	    vertico-posframe yasnippet))
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(send-mail-function 'mailclient-send-it)
 '(tab-bar-new-tab-choice "*Splash Screen*"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:inherit default :weight bold :foreground "gray80" :height 1.6 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "gray80" :height 1.5))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "gray80" :height 1.3))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "gray80" :height 1.2))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "gray80" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "gray80"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "gray80"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "gray80"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "gray80"))))
 '(tab-bar-tab ((t (:box (:line-width (10 . 10) :style flat-button) :underline (:color foreground-color :style line :position t))))))

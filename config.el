(defun config ()
    "Find the config file."
    (interactive)
    (find-file "~/.emacs.d/config.org"))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(load "~/.emacs.d/my-project-manager.el")
(load "~/.emacs.d/my-delete.el")

(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(set-frame-parameter nil 'ns-appearance 'dark)

(defmacro append-to-list (target suffix)
    "Append SUFFIX to TARGET in place."
    `(setq ,target (append ,target ,suffix)))

(require 'package)

(append-to-list package-archives
    '(("melpa" . "http://melpa.org/packages/")
         ("melpa-stable" . "http://stable.melpa.org/packages/")
         ("org-elpa" . "https://orgmode.org/elpa/")))

(package-initialize)

(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

(setq
    use-package-always-ensure t
    use-package-verbose t)

(setq mac-command-modifier 'control)
(setq mac-control-modifier 'super)

(global-set-key (kbd "M-F") 'forward-sexp)
(global-set-key (kbd "M-B") 'backward-sexp)

(pixel-scroll-precision-mode)

(use-package nerd-icons
    :custom
    (nerd-icons-font-family "Symbols Nerd Font Mono")
    )

(use-package doom-themes
    :init
    (load-theme 'doom-dracula))

(set-face-attribute 'line-number-current-line nil
    :foreground (face-attribute 'highlight :background))

(use-package doom-modeline
    :ensure t
    :init (doom-modeline-mode 1))

(setq inhibit-compacting-font-caches t)

(setq doom-modeline-icon t)

(setq display-time-default-load-average nil)
(setq doom-modeline-time-icon t)
(setq doom-modeline-time t)
(display-time)

(setq battery-update-interval 30)
(setq doom-modeline-battery t)
(display-battery-mode +1)

;; (let ((color (face-attribute 'highlight :background)))
;; 	(set-face-attribute 'tab-bar-tab nil :box '(:line-width 10 :style flat-button) :underline `(:color ,color :style line :position t)
;; 	))
(defun my-setup-tab-bar-faces ()
  "Apply GUI-specific faces for the tab bar."
  ;; Optional: Ensure we are in a GUI before setting GUI attributes
  (when (display-graphic-p)
    (let ((color (face-attribute 'highlight :background nil t)))
      ;; Only apply if a valid color was actually returned
      (when (and color (not (eq color 'unspecified)))
        (set-face-attribute 'tab-bar-tab nil 
                            :box '(:line-width 10 :style flat-button) 
                            :underline `(:color ,color :style line :position t))))))

;; If starting as a daemon, wait for the frame. 
;; If starting normally (standalone app), run it immediately.
(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'my-setup-tab-bar-faces)
  (my-setup-tab-bar-faces))

(add-hook 'tab-bar-mode-hook (lambda () (setq tab-bar-close-button
	#(" " 0 1
	 (close-tab t rear-nonsticky t help-echo "Click to close tab" face
		 (:box nil) display
		 (image :type svg :file
			 "/opt/homebrew/Cellar/emacs-plus@30/30.2/share/emacs/30.2/etc/images/symbols/cross_16.svg"
			 :height (1 . em) :scale 1 :margin 1 :ascent center
			 :transform-smoothing t))))))

(use-package vertico-posframe
    :after vertico
    :config
    (setq vertico-posframe-handler
        'posframe-poshandler-frame-center)
    (vertico-posframe-mode 1))

(use-package projectile
    :ensure t
    :init
    (setq projectile-project-search-path '("~/Coding"))
    :config
    (projectile-mode +1)
    :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package avy
    :ensure t
    :config
    (setq avy-timeout-seconds 0.15)
    :bind (("M-j" . avy-goto-char)))

(defun pulse-symbol-at-point ()
    "Briefly pulse the symbol under the current point."
    (interactive)
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
              (first (car bounds))
              (last (cdr bounds)))
        (pulse-momentary-highlight-region first last)))

(require 'avy)

(advice-add 'avy-goto-char-timer :after #'pulse-symbol-at-point)

(add-hook 'isearch-mode-end-hook #'pulse-symbol-at-point)

(use-package ace-window)
(bind-key* "M-o" 'ace-window)

(defun my/comment-uncomment ()
	"Runs comment-uncomment on the region if active, or the current line if not."
	(interactive)
    (if (region-active-p)
		(progn (comment-or-uncomment-region (region-beginning) (region-end)))
		(progn (push-mark)
			(beginning-of-line)
			(push-mark)
			(end-of-line)
			(comment-or-uncomment-region (region-beginning) (region-end))
			(set-mark-command t)
			(set-mark-command t))))

;; (global-set-key (kbd "C-/") 'my/comment-uncomment)

(use-package scroll-restore)

(defun swap-line-up ()
    "Swap the current line with the one above it, retaining cursor position."
    (interactive)
    (if (/= (line-number-at-pos) 1)
        (let ((col (current-column)) (line (line-number-at-pos)))
            (transpose-lines 1)
            (goto-line (- line 1))
            (move-to-column col)
            )))

(defun swap-line-down ()
    "Swap the current line with the one below it, retaining cursor position."
    (interactive)
    (if (/= (line-number-at-pos) (count-lines (point-min) (point-max)))
        (let ((col (current-column)) (line (line-number-at-pos)))
            (forward-line 1)
            (transpose-lines 1)
            (goto-line (+ line 1))
            (move-to-column col)
            )))

(global-set-key (kbd "M-<up>") 'swap-line-up)
(global-set-key (kbd "M-<down>") 'swap-line-down)

(global-set-key (kbd "M-S-<up>") 'duplicate-line)

(global-set-key (kbd "M-S-<down>") (lambda ()
                                       (interactive)
                                       (let ((col (current-column)))
                                           (duplicate-line)
                                           (forward-line 1)
                                           (move-to-column col))))

(add-hook 'text-mode-hook 'visual-line-mode)

(use-package yasnippet
	:hook (prog-mode . yas-minor-mode)
	:config
	(yas-reload-all))
(yas-global-mode 1)

(setq require-final-newline nil)
(setq mode-require-final-newline nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq confirm-kill-emacs 'yes-or-no-p)

(global-auto-revert-mode 1)

(setq gc-cons-threshold 1000000)

(setq make-backup-files nil)
(setq auto-save-default nil)

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
;;(global-set-key [remap move-beginning-of-line]
               ;; 'smarter-move-beginning-of-line)

(use-package which-key
    :ensure t
    :config
    (which-key-mode +1))

(use-package vertico
    :init
    (vertico-mode)
    :bind (:map vertico-map
    ("C-j" . vertico-next)
    ("C-k" . vertico-previous)))

(use-package emacs
    :custom
    (read-extended-command-predicate #'command-completion-default-include-p))

(use-package marginalia
    :init
    (marginalia-mode))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-cycle t) ;; Enable cycling for `corfu-next/previous'
  :hook ((prog-mode . corfu-mode)
	 (shell-mode . corfu-mode)
	 (eshell-mode . corfu-mode))
  :bind
  (:map corfu-map
        ;;     ("TAB" . corfu-next)
        ;;     ("C-n" . corfu-next)
        ;;     ([tab] . corfu-next)
        ;;     ("C-p" . corfu-previous)
        ;;     ("S-TAB" . corfu-previous)
        ;;     ([backtab] . corfu-previous)
	("M-<backspace>" . backward-kill-word)
	("C-<backspace>" . backward-kill-word)
	;; ("ESC" . corfu-quit)
	))

(setq corfu-auto-delay 0)
(setq corfu-auto-prefix 2)

(use-package orderless
    :custom
    (completion-styles '(orderless basic))
    (completion-category-defaults nil)
    (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult)
(use-package consult-projectile
  :after (consult projectile))

(global-set-key (kbd "C-s") #'consult-line)
(global-set-key (kbd "C-x b") #'consult-buffer)
(global-set-key (kbd "C-M-l") #'consult-imenu)
(global-set-key (kbd "M-y") #'consult-yank-pop)

(use-package prescient
  :config
  (prescient-persist-mode 1))

(use-package vertico-prescient
  :after vertico
  :config
  (setq vertico-prescient-enable-filtering nil)
  (vertico-prescient-mode 1))

(use-package corfu-prescient
  :after corfu
  :config
  (setq corfu-prescient-enable-filtering nil)
  (corfu-prescient-mode 1))

(package-install 'org-modern)
(with-eval-after-load 'org (global-org-modern-mode))

(setq org-modern-table nil)

(package-install 'olivetti)
(setq olivetti-body-width 0.6)
(add-hook 'org-mode-hook #'olivetti-mode)

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(package-install 'cdlatex)
(require 'cdlatex)

(setq cdlatex-command-alist
    '(("lap" "Insert Laplace transform" "\\mathcal L {" cdlatex-lr-pair nil nil t)
         ("ilap" "Insert inverse Laplace transform" "\\mathcal L^{-1} {" cdlatex-lr-pair nil nil t)
         ("lim" "Insert limit" "\\lim_{?}" cdlatex-position-cursor nil nil t)
         ("()" "Insert inline math" "\\(?\\)" cdlatex-position-cursor nil t nil)
	 ("tt" "Insert \\text{}" "\\text{?}" cdlatex-position-cursor nil nil t)
         ("hb" "Insert \\hbar" "\\hbar" nil nil nil t)
	 ("bb" "Insert \\mathbb" "\\mathbb " nil nil nil t)))

(setq cdlatex-math-modify-alist
      '((118 "\\vb*" nil t nil nil)))
;; 6 items: key, mathcmd, textcmd, type (t if has argument, nil if style), rmdot (for if the dot on i and j should be removed idk), and t if italic correction is required.

(defun begin-inline-math ()
    "Begins an inline math environment with \\(\\).  Requires cdlatex."
    (interactive)
    (insert "\\(?\\)")
    (cdlatex-position-cursor))
(add-hook 'org-cdlatex-mode-hook (lambda ()
                                     (keymap-set org-cdlatex-mode-map "C-(" #'begin-inline-math)))

(add-hook 'org-mode-hook #'turn-on-org-cdlatex)

(package-install 'auctex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-crypt-key nil)

(setq org-crypt-tag-matcher "crypt")
(setq org-tags-exclude-from-inheritance '("crypt"))

(use-package org-roam
	:ensure t
	:custom
	(org-roam-directory "~/org-roam")
	:bind (("C-c n l" . org-roam-buffer-toggle)
			  ("C-c n f" . org-roam-node-find)
			  ("C-c n i" . org-roam-node-insert)
			  :map org-roam-dailies-map
			  ("Y" . org-roam-dailies-capture-yesterday)
			  ("T" . org-roam-dailies-capture-tomorrow))
	:bind-keymap
	("C-c n d" . org-roam-dailies-map)
	:config
	(require 'org-roam-dailies)
	(org-roam-setup))

(add-to-list 'display-buffer-alist
	'("\\*org-roam\\*"
		 (display-buffer-in-direction)
		 (direction . right)
		 (window-width . 0.33)
		 (window-height . fit-window-to-buffer)))

(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
  	 :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
  			    "#+title: ${title}\n")
  	 :unnarrowed t
	 :empty-lines 1
	 )
	("p" "physics" plain "%?"
  	 :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
  			    "#+startup: latexpreview\n#+latex_header: \\usepackage{physics}\n#+filetags: :physics:\n#+title: ${title}")
  	 :unnarrowed t
	 )
	("m" "math" plain "%?"
  	 :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
  			    "#+startup: latexpreview\n#+filetags: :math:\n#+title: ${title}")
  	 :unnarrowed t
	 )))

(setq org-roam-node-display-template
      (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

(setq org-roam-dailies-directory "daily/")

(setq org-roam-dailies-capture-templates
    '(("d" "default" plain
          "* %?"
          :target (file+head "%<%Y-%m-%d>.org"
                      "#+title: %<%Y-%m-%d>\n")
		  :empty-lines 1)
		 ("j" "journal" entry
			 "* Journal: %^{Title}\n\n%?"
			 :target (file+head "%<%Y-%m-%d>.org"
						 "#+title: %<%Y-%m-%d>\n")
			 :empty-lines 1)
		 ))

(use-package org-roam-ui
    :after org-roam
	;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
	;;         a hookable mode anymore, you're advised to pick something yourself
	;;         if you don't care about startup time, use
	;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(defun org-fold-all-reveal ()
	"Cycle global visibility, then reveal."
	(interactive)
	(org-shifttab 1)
	(org-fold-reveal)
	(message "")
	)

(define-key org-mode-map (kbd "C-c C-h") #'org-fold-all-reveal)

(use-package valign
	:hook (org-mode . valign-mode)
	:config (setq valign-fancy-bar t))

(org-babel-do-load-languages
	'org-babel-load-languages '((python . t)))

(setq org-babel-python-command "/usr/bin/python3")

(use-package eglot
  :ensure t
  :hook ((python-ts-mode . eglot-ensure)
         (c-ts-mode      . eglot-ensure)
         (c++-ts-mode    . eglot-ensure)
         (js-ts-mode     . eglot-ensure)
         (java-ts-mode   . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '(html-mode . ("vscode-html-language-server" "--stdio"))))

;; (if (eq system-type 'darwin)
;; 	(setq lsp-clients-clangd-executable "/opt/homebrew/bin/clangd"))

(use-package emmet-mode
	:hook (html-mode css-mode web-mode)
	:config
	(keymap-set emmet-mode-keymap "TAB" #'emmet-expand-line))

;; (add-hook 'mhtml-mode-hook (lambda ()
;; 			     (run-hooks 'prog-mode-hook)))
(derived-mode-add-parents 'mhtml-mode '(prog-mode))

(use-package treesit
  ;; No :ensure t because it's built into Emacs 30
  :ensure nil
  :config
  (setq treesit-language-source-alist
        '((python "https://github.com/tree-sitter/tree-sitter-python")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (java "https://github.com/tree-sitter/tree-sitter-java")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (html "https://github.com/tree-sitter/tree-sitter-html"))))

(setq treesit-extra-load-path '("~/.emacs.d/tree-sitter-gdscript/src"))

;; Remap standard modes to Tree-sitter modes
(setq major-mode-remap-alist
      '((python-mode     . python-ts-mode)
        (c-mode          . c-ts-mode)
        (c++-mode        . c++-ts-mode)
        (js-mode         . js-ts-mode)
	(javascript-mode . js-ts-mode)
        (java-mode       . java-ts-mode)
        (css-mode        . css-ts-mode)
        (html-mode       . html-ts-mode)
        (typescript-mode . typescript-ts-mode)))

(add-hook 'prog-mode-hook #'hl-line-mode)

(use-package flymake
  :bind (("M-n" . flymake-goto-next-error)  ; Move to next error
         ("M-p" . flymake-goto-prev-error)) ; Move to previous error
  :hook (prog-mode . flymake-mode))

(use-package magit)

(use-package forge
  :after magit)

(use-package indent-bars
  :ensure t
  :hook ((prog-mode . indent-bars-mode))
  :config
  (setq 
        indent-bars-prefer-character t

        ;; Show indent guides starting from the first column.
        indent-bars-starting-column 0
        ;; Make indent guides subtle; the default is too distractingly colorful.
        indent-bars-width-frac 0.15  ; make bitmaps thinner
        indent-bars-color-by-depth nil
        indent-bars-color '(font-lock-comment-face :face-bg nil :blend 0.425)
        ;; Don't highlight current level indentation; it's distracting and is
        ;; unnecessary overhead for little benefit.
        indent-bars-highlight-current-depth nil
        ;; The default is `t', which shows indent-bars even on blank lines
        ;; beyond the end of an indented block. Setting it to `nil' will cause
        ;; gaps in the indent guides, which looks odd. `least' is a good
        ;; compromise, and doesn't suffer the scrolling issue.
        indent-bars-display-on-blank-lines 'least))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-width-start t)
(setq display-line-numbers-grow-only t)

;; (add-hook 'prog-mode-hook 'electric-pair-local-mode)

(use-package smartparens
  :ensure smartparens
  :hook (prog-mode text-mode markdown-mode)
  :config
  (require 'smartparens-config))

(defun my/newline-and-indent-handler (&rest _ignored)
  "Vertically split an expression inside of a pair."
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(require 'smartparens)
(sp-local-pair 'prog-mode "{" nil :post-handlers '((my/newline-and-indent-handler "RET")))

(defun my/org-babel-get-session ()
"Return the name of the current Babel source block session."
(interactive)
(cdr (assoc :session (nth 2 (org-babel-get-src-block-info)))))

(defun my/org-babel-execute-session (&optional arg)
  "Execute all source blocks in the buffer, belonging to the current session."
  (interactive)
  (let ((current-session (my/org-babel-get-session)))
    (org-babel-eval-wipe-error-buffer)
    (org-save-outline-visibility t
      (org-babel-map-executables nil
	(if (and
	     (my/org-babel-get-session)
	     (string= current-session (my/org-babel-get-session))
	     (not (string= (my/org-babel-get-session) "none")))
	    (if (org-element-type-p
		 (org-element-context) '(babel-call inline-babel-call))
		(org-babel-lob-execute-maybe)
              (org-babel-execute-src-block arg))))))
  (org-display-inline-images))

(keymap-set org-babel-map "C-v" 'my/org-babel-execute-session)

(use-package gdscript-mode)

(setq auth-sources "~/.authinfo.gpg")

(add-hook 'prog-mode-hook 'hs-minor-mode)

(require 'projectile)
(defun start-web-server-in-project ()
    "Start a simple Python web server in root directory of current project."
    (interactive)
    (projectile-run-async-shell-command-in-root "python3 -m http.server"))

(define-key projectile-mode-map (kbd "C-c p w") #'start-web-server-in-project)

(use-package treemacs
    :ensure t
    :defer t
    :bind
	(:map global-map
		("C-c t" . treemacs)))

(use-package treemacs-projectile
	:after (treemacs projectile)
	:ensure t)

(define-minor-mode writing-mode
	"Minor mode for writing text in a distraction-free environment."
	:lighter "Writing"
	:init-value nil
	(if writing-mode
		(progn
			(if (member "Domine" (font-family-list))
				(setq-local writing--family "Domine")
				(setq-local writing--family "Times New Roman"))
			(setq-local face-remapping-alist
				`((default
					  :background "#ffffff"
					  :foreground "#000000"
					  :family ,writing--family
					  :height 140)))
			;; (setq-local face-remapping-alist
			;; 	(append face-remapping-alist
			;; 		 '((mode-line (:height 1))
			;; 		 (mode-line-inactive (:height 1)))))
			(olivetti-mode 1)
			(line-number-mode -1)
			(setq-local writing--mode-line-format mode-line-format)
			(setq-local mode-line-format nil)
			(when (bound-and-true-p centaur-tabs-mode)
				(centaur-tabs-local-mode)
				(message "toggled")))
		(buffer-face-mode -1)
		(kill-local-variable 'face-remapping-alist)
		(olivetti-mode -1)
		(setq-local mode-line-format writing--mode-line-format)))

(use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize))

(defun center-and-newline (text)
      "Insert the provided TEXT, center and newline."
      (insert text)
      (center-line)
      (insert "\n"))

(defun bendomine/create-splash-screen()
	(read-only-mode -1)
	(erase-buffer)

	(set-fill-column (window-body-width nil))

	(let* (
              (height (- (window-body-height nil) 1))
              (offset 10)
              (vertical-padding (- (/ height 2) offset)))
		(insert-char ?\n vertical-padding))

	(center-and-newline "+---------------------------------------------------------+")
	(center-and-newline "| 8888888888888b     d888       d8888 .d8888b.  .d8888b.  |")
	(center-and-newline "| 888       8888b   d8888      d88888d88P  Y88bd88P  Y88b |")
	(center-and-newline "| 888       88888b.d88888     d88P888888    888Y88b.      |")
	(center-and-newline "| 8888888   888Y88888P888    d88P 888888        \"Y888b.   |")
	(center-and-newline "| 888       888 Y888P 888   d88P  888888           \"Y88b. |")
	(center-and-newline "| 888       888  Y8P  888  d88P   888888    888      \"888 |")
	(center-and-newline "| 888       888   \"   888 d8888888888Y88b  d88PY88b  d88P |")
	(center-and-newline "| 8888888888888       888d88P     888 \"Y8888P\"  \"Y8888P\"  |")
	(center-and-newline "+---------------------------------------------------------+")

	(insert "\n")

	(center-and-newline "Customized by Ben Domine")
	(insert "\n\n")

	(insert-text-button "Open config file" 'action (lambda (_) (config)) 'follow-link t)
	(center-line) (insert "\n\n")
	;; (local-set-key (kbd "c") (lambda (_) (config)))

	(insert-text-button "Create org document" 'action (lambda (_)
                                                          (let ((buffer (generate-new-buffer "New org document")))
                                                              (switch-to-buffer buffer)
                                                              (org-mode))) 'follow-link t)
	(center-line) (insert "\n\n")
	(insert-text-button "New journal entry" 'action (lambda (_) (org-roam-dailies-capture-today nil "j")) 'follow-link t)
	;; (local-set-key (kbd "j") (lambda (_) (org-roam-dailies-capture-today nil "j")))
	
	(center-line) (insert "\n")

	(setq mode-line-format nil)
	(setq cursor-type nil)
	(setq horizontal-scroll-bar nil)
	(setq vertical-scroll-bar nil)

	(read-only-mode 1)
	(buffer-disable-undo))

(defun bendomine/splash-screen ()
    "Display my custom splash screen."
    (interactive)

    (let ((new-buffer (get-buffer-create "*Splash Screen*")))
        (with-current-buffer new-buffer
            (bendomine/create-splash-screen))

        (switch-to-buffer new-buffer)
        (when (bound-and-true-p centaur-tabs-mode) (if (or centaur-tabs-mode centaur-tabs-local-mode) (centaur-tabs-local-mode)))
        (message "")))

(add-hook 'window-setup-hook (lambda ()
                                 (run-at-time "0.3 sec" nil #'bendomine/splash-screen)))



(global-set-key (kbd "C-<wheel-up>") nil)
(global-set-key (kbd "C-<wheel-down>") nil)

(use-package emacs-everywhere
  :ensure t)

(use-package helpful
  :ensure t)

;; Enable Evil
(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil) ;; Required for evil-collection to work properly
  (setq evil-want-C-u-scroll t)   ;; Allow C-u to scroll up like in Vim
  (setq evil-want-C-i-jump t)     ;; Allow C-i to jump forward like in Vim
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-commentary
  :config
  (evil-commentary-mode))

(use-package evil-escape
  :ensure t
  :init
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.15)
  :config
  (evil-escape-mode 1))

(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "gj") 'evil-next-line)
(define-key evil-normal-state-map (kbd "gk") 'evil-previous-line)

(require 'ibuffer)
(require 'helpful)
(require 'magit)
(require 'projectile)
(use-package general
  :ensure t
  :config
  ;; Tell general to integrate with Evil
  (general-evil-setup t)

  ;; Create a custom definer for your leader keys
  (general-create-definer my-leader-def
			  :states '(normal visual insert emacs)
			  :keymaps 'override
			  :prefix "SPC"
			  ;; This allows you to use Alt+Space as the leader in insert mode
			  :non-normal-prefix "M-SPC") 

  ;; Now define your keybindings using your new definer
  (my-leader-def
   "SPC" 'execute-extended-command ;; SPC SPC for M-x
   "."   'find-file
   ","   'switch-to-buffer
   "s"   'save-buffer

   ;; File commands
   "f"   '(:ignore t :which-key "file")
   "ff"  'find-file
   "fs"  'save-buffer

   ;; Buffer commands
   "b"   '(:ignore t :which-key "buffer")
   "bb"  'switch-to-buffer
   "bi"  'ibuffer-other-window
   "bk"  'kill-buffer
   "bd"  'kill-current-buffer

   ;; Help commands
   "h"   '(:ignore t :which-key "describe")
   "hf"  'helpful-callable
   "hv"  'helpful-variable
   "hm"  'describe-mode
   "hk"  'helpful-key

   ;; Window commands
   "w"   '(:ignore t :which-key "window")
   "wc"  'delete-window
   "ww"  'delete-other-windows
   "wr"  'split-window-right
   "wd"  'split-window-below
   "wh"  'evil-window-left
   "wj"  'evil-window-down
   "wk"  'evil-window-up
   "wl"  'evil-window-right

   ;; Quit commands
   "q"   '(:ignore t :which-key "quit")
   "qr"  'restart-emacs
   "qq"  'save-buffers-kill-terminal

   ;; Magit commands
   "g"   '(:ignore t :which-key "magit")
   "gg"  'magit-status

   ;; Projectile commands
   "p"   '(:ignore t :which-key "projectile")
   "pp"  'projectile-switch-project
   "pf"  'projectile-find-file
   "pa"  'projectile-add-known-project))

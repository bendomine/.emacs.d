(defun config ()
    "Find the config file."
    (interactive)
    (find-file "~/.emacs.d/config.org"))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(load "~/.emacs.d/my-project-manager.el")

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

(if (eq system-type 'darwin)
    (global-set-key (kbd "C-j") 'set-mark-command))

(pixel-scroll-precision-mode)

(use-package nerd-icons
    :custom
    (nerd-icons-font-family "Symbols Nerd Font Mono")
    )

(use-package doom-themes
    :init
    (load-theme 'doom-dark+))

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

(use-package centaur-tabs
    :demand
    :config
    (set-face-attribute 'centaur-tabs-default nil :height 5)
    (centaur-tabs-mode t)
    )

(setq centaur-tabs-set-icons t)
(setq centaur-tabs-icon-type 'nerd-icons)

(setq centaur-tabs-style "bar")
(setq centaur-tabs-set-modified-marker t)
(setq centaur-tabs-set-bar 'left)
(set-face-attribute 'centaur-tabs-default nil :height 5)

(centaur-tabs-group-by-projectile-project)

(add-hook 'special-mode-hook 'centaur-tabs-local-mode)

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
              ("C-c C-p" . projectile-command-map)))

(use-package avy
    :ensure t
    :config
    ;;(setq avy-timeout-seconds 0.25)
    :bind (("M-j" . avy-goto-char-timer)))

(defun pulse-symbol-at-point ()
    "Briefly pulse the symbol under the current point."
    (interactive)
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
              (first (car bounds))
              (last (cdr bounds)))
        (pulse-momentary-highlight-region first last)))

(require 'avy)

;; (advice-add 'avy-goto-char-timer :around #'pulse-symbol-at-point)

(add-hook 'isearch-mode-end-hook #'pulse-symbol-at-point)

(package-install 'multiple-cursors)
(require 'multiple-cursors)

(global-set-key (kbd "C-d") 'mc/mark-next-like-this-word)
(global-set-key (kbd "C-S-d") 'mc/unmark-next-like-this)

(delete-selection-mode 1)
(add-hook 'multiple-cursors-mode-hook (lambda () (delete-selection-mode -1)))
(add-hook 'multiple-cursors-mode-disabled-hook (lambda () (delete-selection-mode 1)))

(define-key mc/keymap (kbd "<return") nil)

(with-eval-after-load 'c++-mode
    (define-key c++-mode-map (kbd "C-d") nil))

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
(yas-global-mode)

(use-package which-key
    :ensure t
    :config
    (which-key-mode +1))

(use-package vertico
    :init
    (vertico-mode))

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
        ("ESC" . corfu-quit)))

(global-corfu-mode)

;;  (setq corfu-auto-delay 0.2)

(use-package orderless
    :custom
    (completion-styles '(orderless basic))
    (completion-category-defaults nil)
    (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
        :hook (completion-list-mode . consult-preview-at-point-mode)
        :custom
        (consult-preview-key nil)
        (consult-narrow-key nil)
        :config
        (consult-customize consult-theme consult-line consult-line-at-point :preview-key '(:debounce 0.2 any))
        )

(package-install 'org-modern)
(with-eval-after-load 'org (global-org-modern-mode))

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
         ("hb" "Insert \\hbar" "\\hbar" nil nil nil t)))

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
	(org-roam-directory "~/RoamNotes")
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
		  :unnarrowed t)))

(setq org-roam-dailies-directory "daily/")

(setq org-roam-dailies-capture-templates
    '(("d" "default" plain
          "* %?"
          :target (file+head "%<%Y-%m-%d>.org"
                      "#+title: %<%Y-%m-%d>\n"))))

(use-package lsp-mode
	:init
	(setq lsp-keymap-prefix "C-c l")
	:hook (
			  (python-mode . lsp)
			  (javascript-mode . lsp)
			  (lsp-mode . lsp-enable-which-key-integration))
	:commands lsp
	:custom
	(lsp-enable-snippet t)
	(lsp-completion-provider :none))

(use-package lsp-ui :commands lsp-ui-mode)

(add-to-list 'load-path (expand-file-name "lib/lsp-mode" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lib/lsp-mode/clients" user-emacs-directory))

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(require 'lsp-treemacs)
(lsp-treemacs-sync-mode 1)

;; (if (eq system-type 'darwin)
;; 	(setq lsp-clients-clangd-executable "/opt/homebrew/bin/clangd"))

(use-package emmet-mode
	:hook (html-mode css-mode web-mode))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package flycheck
      :config
      (add-hook 'prog-mode-hook 'flycheck-mode)
      (add-hook 'after-init-hook #'global-flycheck-mode))

(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11" flycheck-clang-language-standard "c++11")))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))

(use-package magit)

(setq-default indent-tabs-mode t)

;; idk if it’s necessary, but just to be extra sure:
(add-hook 'prog-mode-hook (lambda () (setq indent-tabs-mode t)))

(setq-default tab-width 4)
(setq-default standard-indent 4)

(setq js-indent-level 4
    typescript-indent-level 4
	python-indent-offset 4
	c-basic-offset 4
	lisp-indent-offset 4)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-width-start t)
(setq display-line-numbers-grow-only t)

(add-hook 'prog-mode-hook 'electric-pair-local-mode)

(require 'projectile)
(defun start-web-server-in-project ()
    "Start a simple Python web server in root directory of current project."
    (interactive)
    (projectile-run-async-shell-command-in-root "python3 -m http.server"))

(define-key projectile-mode-map (kbd "C-c C-p w") #'start-web-server-in-project)

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
              (offset 20)
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

	(insert-text-button "Create org document" 'action (lambda (_)
                                                          (let ((buffer (generate-new-buffer "New org document")))
                                                              (switch-to-buffer buffer)
                                                              (org-mode))) 'follow-link t)
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
        (if (or centaur-tabs-mode centaur-tabs-local-mode) (centaur-tabs-local-mode))
        (message "")))

(add-hook 'window-setup-hook (lambda ()
                                 (run-at-time "0.3 sec" nil #'bendomine/splash-screen)))

(global-set-key (kbd "C-<wheel-up>") nil)
(global-set-key (kbd "C-<wheel-down>") nil)

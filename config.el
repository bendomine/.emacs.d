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
(display-time)
(setq doom-modeline-time t)
(setq doom-modeline-time-icon t)

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

(with-eval-after-load 'centaur-tabs
    (require 'projectile)

    (setq centaur-tabs-buffer-groups-function
        #'centaur-tabs-projectile-buffer-groups)

    (defun my-centaur-tabs-visible-p ()
        (projectile-project-root))

    (add-hook 'centaur-tabs-hide-tabs-hooks
        (lambda ()
            (not (my-centaur-tabs-visible-p)))))

(use-package projectile
    :ensure t
    :init
    (projectile-mode +1)
    :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c C-p" . projectile-command-map)))

;; Slurp environment variables from the shell.
;; a.k.a. The Most Asked Question On r/emacs
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))



;; Completions
;; Vertico for vertical completion for the emacs minibuffer.
(use-package vertico
  :init
  (vertico-mode))

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p))

;; A bunch of great search and navigation commands
(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (consult-preview-key nil)
  (consult-narrow-key nil)
  :config
  (consult-customize consult-theme consult-line consult-line-at-point :preview-key '(:debounce 0.2 any))
  )

;; Annotations in the minibuffer, i.e a description of the function next to the name in M-x
(use-package marginalia
  :init
  (marginalia-mode))

;; In buffer completions, think lsp completions
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-cycle t) ;; Enable cycling for `corfu-next/previous'
  :bind
  (:map corfu-map
	("TAB" . corfu-next)
	("C-n" . corfu-next)
	([tab] . corfu-next)
	("C-p" . corfu-previous)
	("S-TAB" . corfu-previous)
	([backtab] . corfu-previous)
	("ESC" . corfu-quit))
  :init
  (global-corfu-mode))

(setq corfu-auto-delay 0.2
      corfu-auto-prefix 2)

;; Completion style and fuzzy matching
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))



;; Flycheck
(use-package flycheck
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode) ;; always lint my code
  (add-hook 'after-init-hook #'global-flycheck-mode))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11" flycheck-clang-language-standard "c++11")))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))


;; (define-key eglot-mode-map (kbd "C-c a") 'eglot-code-actions)

;; Tree!
(use-package treemacs
  :ensure t
  :defer t
  :config
  ;; Integrate with project.el
  (setq treemacs-project-follow-cleanup t))
(global-set-key (kbd "C-c t") 'treemacs)


;; Snippets and LaTeX
(add-to-list 'load-path
	     "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
;;CDLatex
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



				   ;; Tab bar
				   (defun centaur-tabs-buffer-groups ()
				     "`centaur-tabs-buffer-groups' control buffers' group rules.
Group centaur-tabs with mode if buffer is derived from `eshell-mode'
`emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer names starting with * will group to \"Emacs\".
Other buffers group by `centaur-tabs-get-group-name' with project name.
Buffers starting with \"Untitled\" will be treated as part of all groups."
				     (if (string-prefix-p "Untitled" (buffer-name))
					 ;; Make buffer appear in all groups
					 '("Emacs" "Editing" "Dired" "Help" "OrgMode" "Other")
				       ;; Otherwise, use normal grouping logic
				       (list
					(cond
					 ((or (string-equal "*" (substring (buffer-name) 0 1))
					      (memq major-mode '(magit-process-mode
								 magit-status-mode
								 magit-diff-mode
								 magit-log-mode
								 magit-file-mode
								 magit-blob-mode
								 magit-blame-mode)))
					  "Emacs")
					 ((derived-mode-p 'prog-mode)
					  "Editing")
					 ((derived-mode-p 'dired-mode)
					  "Dired")
					 ((memq major-mode '(helpful-mode
							     help-mode))
					  "Help")
					 ((memq major-mode '(org-mode
							     org-agenda-clockreport-mode
							     org-src-mode
							     org-agenda-mode
							     org-beamer-mode
							     org-indent-mode
							     org-bullets-mode
							     org-cdlatex-mode
							     org-agenda-log-mode
							     diary-mode))
					  "OrgMode")
					 (t
					  (centaur-tabs-get-group-name (current-buffer)))))))

				   ;; (use-package centaur-tabs
				   ;;     :demand
				   ;;     :bind
				   ;;     (("s-t" . centaur-tabs--create-new-tab)
				   ;;         ("s-w" . kill-this-buffer))
				   ;;     :init
				   ;;     (setq centaur-tabs-style "bar"

				   ;;         ;; Show icons
				   ;;         centaur-tabs-set-icons t
				   ;;         centaur-tabs-icon-type 'nerd-icons

				   ;;         ;; Modified marker
				   ;;         centaur-tabs-set-modified-marker t

				   ;;         ;; Bar position: 'over, 'under, nil
				   ;;         centaur-tabs-set-bar 'left
				   ;;         ;;centaur-tabs-active-bar

				   ;;         ;; Don’t show the "+" button for new tabs
				   ;;         centaur-tabs-show-new-tab-button nil

				   ;;         ;; Cycle only through tabs, not groups
				   ;;         centaur-tabs-cycle-scope 'tabs

				   ;;         ;; Grouping
				   ;;         centaur-tabs-buffer-groups-function 'centaur-tabs-buffer-groups
				   ;;         )
				   ;;     :config
				   ;;     ;; Enable tabs
				   ;;     (set-face-attribute 'centaur-tabs-default nil :height 5)
				   ;;     (centaur-tabs-mode t)
				   ;;     )
				   (setq centaur-tabs-show-navigation-buttons t)
				   (add-hook 'special-mode-hook 'centaur-tabs-local-mode)



				   ;; Multiple Cursors
				   ;; https://github.com/magnars/multiple-cursors.el?tab=readme-ov-file#command-overview
				   (require 'multiple-cursors)
				   (global-set-key (kbd "C-d") 'mc/mark-next-like-this-word)
				   (global-set-key (kbd "C-S-d") 'mc/unmark-next-like-this)
				   (add-hook 'multiple-cursors-mode-hook (lambda () (delete-selection-mode -1)))
				   (add-hook 'multiple-cursors-mode-disabled-hook (lambda () (delete-selection-mode 1)))
				   (delete-selection-mode 1)

				   (with-eval-after-load 'c++-mode
				     (define-key c++-mode-map (kbd "C-d") nil))

				   (define-key mc/keymap (kbd "<return>") nil)


				   ;; Other programming languages
				   (use-package glsl-mode
				     :ensure t
				     ;; For some reason this package rebinds this
				     :bind (:map glsl-mode-map ("C-d" . nil)))


				   ;; Avy
				   (use-package avy
				     :ensure t
				     :bind (("M-j" . avy-goto-char-timer)))


				   ;; Custom splash screen

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


				   ;; Duplicating lines and moving them around, vscode-style
				   (global-set-key (kbd "M-S-<up>") 'duplicate-line)
				   (global-set-key (kbd "M-S-<down>") (lambda ()
									(interactive)
									(let ((col (current-column)))
									  (duplicate-line)
									  (forward-line 1)
									  (move-to-column col))))
				   (global-set-key (kbd "M-<up>") 'swap-line-up)
				   (global-set-key (kbd "M-<down>") 'swap-line-down)
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

				   ;; Customizing M-DEL


				   ;; TABS!!!
				   ;; Use tabs for indentation globally
				   (setq-default indent-tabs-mode t)

				   ;; Set default tab width (in columns)
				   (setq-default tab-width 4)

				   ;; Make sure Emacs doesn’t convert tabs to spaces
				   (setq-default standard-indent 4)

				   ;; JavaScript / TypeScript
				   (setq js-indent-level 4
					 typescript-indent-level 4)

				   ;; Python
				   (setq python-indent-offset 4)

				   ;; C / C++
				   (setq c-basic-offset 4)

				   ;; Elisp
				   (setq lisp-indent-offset 4)

				   ;; glsl?
				   (setq glsl-indent-offset 4)

				   (add-hook 'prog-mode-hook (lambda () (setq indent-tabs-mode t)))
				   (add-hook 'prog-mode-hook 'electric-pair-local-mode)


				   ;; Hooks
				   (add-hook 'text-mode-hook 'visual-line-mode)
				   (add-hook 'prog-mode-hook 'display-line-numbers-mode)
				   (setq display-line-numbers-width-start t)
				   (setq display-line-numbers-grow-only t)
				   (add-hook 'window-setup-hook (lambda ()
								  (run-at-time "0.3 sec" nil #'bendomine/splash-screen)))

				   ;; Keybindings
				   (global-set-key (kbd "C-w") nil)
				   (global-set-key (kbd "C-<wheel-up>") nil)
				   (global-set-key (kbd "C-<wheel-down>") nil)

				   ;; Auctex
				   (setq TeX-auto-save t)
				   (setq TeX-parse-self t)
				   (setq-default TeX-master nil)

				   ;; My new commands
				   (defun bens-test ()
				     (interactive)
				     (message "Hello World!"))

				   (defun tex ()
				     (interactive)
				     (set-input-method "TeX"))

				   (defun bens-other-test ()
				     (interactive)
				     (self-insert-command "^")
				     (self-insert-command "b"))

				   (defun config ()
				     "Find the config file."
				     (interactive)
				     (find-file "~/.emacs.d/config.org"))

				   (defun compile-cmake ()
				     "Ask for target and compile current cmake project."
				     (interactive)
				     (let (
					   (target (read-string "Build target: "))
					   (build-directory "cmake-build-debug"))
				       (compile (format "cmake --build %s --target %s && ./%s/src/%s" build-directory target build-directory target)))
				     )

				   ;; Org Mode
				   (add-hook 'org-mode-hook 'visual-line-mode)
				   (add-hook 'org-mode-hook 'org-indent-mode)
				   (add-hook 'org-mode-hook #'turn-on-org-cdlatex)
				   ;; (add-hook 'org-mode-hook
				   ;;     (lambda ()
				   ;;         (setq-local yas/trigger-key [tab])
				   ;;         (define-key yas/keymap [tab] 'yas-next-field-or-maybe-expand)))

				   ;; YASnippet
				   ;;(keymap-global-set "C-<tab>" 'yas-expand)

				   ;; Just for fun
				   (require 'zone)
				   (zone-when-idle 100)

;;; .emacs ends here.

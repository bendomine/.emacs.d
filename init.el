;; Fullscreen
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(set-frame-parameter nil 'ns-appearance 'dark)

(setq custom-file "~/.emacs.d/custom.el") ;; Make customize variables go somewhere else
(load custom-file 'noerror)

;; PACKAGES

(require 'package)

;; From https://www.shaneikennedy.xyz/blog/emacs-intro
;; Nice macro for updating lists in place.
(defmacro append-to-list (target suffix)
	"Append SUFFIX to TARGET in place."
	`(setq ,target (append ,target ,suffix)))

;; Set up emacs package archives with 'package
(append-to-list package-archives
    '(("melpa" . "http://melpa.org/packages/") ;; Main package archive
         ("melpa-stable" . "http://stable.melpa.org/packages/") ;; Some packages might only do stable releases?
         ("org-elpa" . "https://orgmode.org/elpa/"))) ;; Org packages, I don't use org but seems like a harmless default

(package-initialize)

;; Ensure use-package is present. From here on out, all packages are loaded
;; with use-package, a macro for importing and installing packages. Also, refresh the package archive on load so we can pull the latest packages.
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(require 'use-package)
(setq
	use-package-always-ensure t ;; Makes sure to download new packages if they aren't already downloaded
	use-package-verbose t) ;; Package install logging. Packages break, it's nice to know why.

;; Slurp environment variables from the shell.
;; a.k.a. The Most Asked Question On r/emacs
(use-package exec-path-from-shell
	:config
	(exec-path-from-shell-initialize))


;; Themes
;; https://github.com/doomemacs/themes/tree/screenshots
(use-package doom-themes
	:init
	(load-theme 'doom-vibrant))


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


;; Flycheck is the newer version of flymake and is needed to make lsp-mode not freak out.
(use-package flycheck
	:config
	(add-hook 'prog-mode-hook 'flycheck-mode) ;; always lint my code
	(add-hook 'after-init-hook #'global-flycheck-mode))

;; Tree!
(use-package treemacs
	:ensure t
	:defer t
	:config
	;; Integrate with project.el
	(setq treemacs-project-follow-cleanup t))
;; (setq treemacs-sorting 'alphabetic-case-insensitive)


;; Snippets and LaTeX
(add-to-list 'load-path
    "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)

;; Tab bar
(use-package all-the-icons
	:if (display-graphic-p))

(defun centaur-tabs-buffer-groups ()
	"`centaur-tabs-buffer-groups' control buffers' group rules.

Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
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

(use-package centaur-tabs
	:demand
	:bind
	(("s-t" . centaur-tabs--create-new-tab)
		("s-w" . kill-this-buffer))
	:init
	(setq centaur-tabs-style "bar"

		;; Show icons
		centaur-tabs-set-icons t
		centaur-tabs-icon-type 'all-the-icons

		;; Modified marker
		centaur-tabs-set-modified-marker t

		;; Bar position: 'over, 'under, nil
		centaur-tabs-set-bar 'left

		;; Show the "+" button for new tabs
		centaur-tabs-show-new-tab-button t

		;; Cycle only through tabs, not groups
		centaur-tabs-cycle-scope 'tabs

		;; Grouping
		centaur-tabs-buffer-groups-function 'centaur-tabs-buffer-groups
		)
	:config
	;; Enable tabs
	;;(setq centaur-tabs-bar-height 50)
	(set-face-attribute 'centaur-tabs-default nil :height 5)
	(centaur-tabs-mode t)

	;; Style options: "bar", "slant", "wave"
	)

;; Multiple Cursors
;; https://github.com/magnars/multiple-cursors.el?tab=readme-ov-file#command-overview
(require 'multiple-cursors)
(global-set-key (kbd "C-d") 'mc/mark-next-like-this-word)
(global-set-key (kbd "C-S-d") 'mc/unmark-next-like-this)
(add-hook 'multiple-cursors-mode-hook (lambda () (setq delete-selection-mode nil)))
(add-hook 'multiple-cursors-mode-disabled-hook (lambda () (setq delete-selection-mode t)))
(define-key mc/keymap (kbd "<return>") nil)

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
										; (defun bens-)

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

(add-hook 'prog-mode-hook (lambda () (setq indent-tabs-mode t)))



;; Hooks
(add-hook 'text-mode-hook 'visual-line-mode)

;; Keybindings
(global-set-key (kbd "C-w") nil)

;; Auctex
(setq TeX-auto-save t)
0(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; My new commands
(defun bens-test ()
	(interactive)
	(mark-whole-buffer))

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
	(find-file "~/.emacs"))

;; Org Mode
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook
    (lambda ()
        (setq-local yas/trigger-key [tab])
        (define-key yas/keymap [tab] 'yas-next-field-or-maybe-expand)))

;; YASnippet
(keymap-global-set "C-<tab>" 'yas-expand)

;;; .emacs ends here.

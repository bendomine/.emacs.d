(require 'widget)

(eval-when-compile
    (require 'wid-edit))

(defun show-cmake-build-buffer ()
	"Display a buffer for building a CMake project."
    (interactive)
    (switch-to-buffer "*Build CMake Project*")
    (kill-all-local-variables)
    (let ((inhibit-read-only t))
        (erase-buffer))
    (remove-overlays)
    (widget-insert (propertize "Build CMake Project" 'face '(:height 1.3 :weight bold)))
	(widget-insert "\n\n")
	
	(widget-create 'menu-choice
		:value (propertize "Option1" 'face '(:height 1.3))
		:tag "Target"
		'(choice-item "Option1")
		'(choice-item "Option2")
		'(choice-item "Option3"))
	
    (use-local-map widget-keymap)
    (widget-setup))


(defun  🤥 ()
	(interactive)
	(xwidget-webkit-browse-url "https://www.youtube.com/watch?v=dQw4w9WgXcQ"))

(global-set-key (kbd "🤥") '🤥)

(defun my/delete ()
  "Delete one character, or all whitespace if only indentation is present."
  (interactive)
  (if (eq (current-indentation) (current-column))
      (while (not (eq 0 (current-indentation)))
	(backward-delete-char-untabify 1)))
  (backward-delete-char-untabify 1))

(define-key prog-mode-map (kbd "DEL") 'my/delete)
;; (define-key mhtml-mode-map (kbd "DEL") 'my/delete)

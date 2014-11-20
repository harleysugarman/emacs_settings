; Set backup directory
(setq backup-directory-alist `(("." . "~/.emacs_saves")))

; clear the eshell buffer by typing 'clear'
(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

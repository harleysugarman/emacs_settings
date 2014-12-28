; Set backup directory
(setq backup-directory-alist `(("." . "~/.emacs_saves")))

; Require package manager 
(require 'package)
(package-initialize)

; Add Marmalade package repo to package manager
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

; Hide menubar
(menu-bar-mode -1)

; Clear the eshell buffer by typing 'clear'
(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

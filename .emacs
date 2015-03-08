; Set backup directory
(setq backup-directory-alist `(("." . "~/.emacs_saves")))

; Require package manager 
(require 'package)
(package-initialize)

; Add Marmalade package repo to package manager
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

; Require IDO
(require 'ido)
(ido-mode t)

; Set default indent size to 2
(setq standard-indent 2)

; Set indentation to spaces, not tabs
(setq-default indent-tabs-mode nil)

; Enable auto-indentation
(define-key global-map (kbd "RET") 'newline-and-indent)

; Hide menubar
(menu-bar-mode -1)

; Enable line numbers (and put one space in the margin)
(global-linum-mode 1)
(setq linum-format "%d ")

; Inhibit splash screen and start with *scratch* buffer
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

; Clear the eshell buffer by typing 'clear'
(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

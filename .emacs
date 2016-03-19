;;; .emacs --- Harley Sugarman's emacs Configuration

;;; Commentary:

;;; On a fresh install of emacs:
;;; - Install from the package manager:
;;; -- autocomplete
;;; -- clojure-mode
;;; -- cider
;;; -- color-theme-monokai
;;; -- fill-column-indicator

;;; Code:

;;; Set backup directory
(setq backup-directory-alist `(("." . "~/.emacs_saves")))

;;; Require package manager 
(require 'package)
(package-initialize)

;;; Add Marmalade package repo to package manager
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;;; Require IDO
(require 'ido)
(ido-mode t)

;;; Enable autocomplete
(require 'auto-complete-config)
(ac-config-default)

;;; Enable flycheck
(global-flycheck-mode)

; Set default indent size to 4
(setq standard-indent 4)

;;; Set indentation to spaces, not tabs
(setq-default indent-tabs-mode nil)

;;; Enable auto-indentation
(define-key global-map (kbd "RET") 'newline-and-indent)

;;; Hide menubar
(menu-bar-mode -1)

;;; Enable line numbers (and put one space in the margin)
(global-linum-mode 1)
(setq linum-format "%d ")

;;; Add ruler at column 80 and display by default in prog-mode
(require 'fill-column-indicator)
(add-hook 'prog-mode-hook (lambda ()
                            (fci-mode 1)
                            (setq fci-rule-column 80)
                            ))

;;; Inhibit splash screen and start with *scratch* buffer
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;;; Clear the eshell buffer by typing 'clear'
(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;;; Add Monokai color theme
(load "~/.emacs.d/elpa/color-theme-monokai-0.0.5/color-theme-monokai.el")
(color-theme-monokai)

;;; Set window size (90*50)
(setq default-frame-alist
      '(
        (width . 92) ; character
        (height . 52) ; lines
        ))

;;; Set orgmode keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(provide `.emacs)
;;; .emacs ends here

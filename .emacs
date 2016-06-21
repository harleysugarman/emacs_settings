;;; .emacs --- Harley Sugarman's emacs Configuration

;;; Comments:

;;; On a fresh install of Emacs (24.x, MacOSX):
;;; - Install from the package manager (MELPA first, then Marmalade):
;;; -- autocomplete
;;; -- spacegray-theme
;;; -- fill-column-indicator
;;; -- web-mode
;;; -- multiple-cursors

;;; Code:

;;; Set backup directory
(setq backup-directory-alist `(("." . "~/.emacs_saves")))

;;; Require package manager 
(require 'package)
(package-initialize)

;;; Add Marmalade/MELPA package repos to package manager
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;;; Require IDO
(require 'ido)
(ido-mode t)

;;; Enable web-mode for web development

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))


;;; Enable autocomplete
(require 'auto-complete-config)
(ac-config-default)

;;; Enable flycheck
(global-flycheck-mode)

; Set default indent size to 4
(setq standard-indent 4)

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

;;; Add Spacegray color theme
(load-theme 'spacegray t)

;;; Highlight the current line
(global-hl-line-mode 1)

;;; Set font (Inconsolata, 14pt)
(set-frame-font "Inconsolata-14")

;;; Set orgmode keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;;; Enable multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)

;;; Set window size to be maximized (this must be done last)
(toggle-frame-maximized)

(provide `.emacs)
;;; .emacs ends here

;;; .emacs --- Harley Sugarman's emacs Configuration

;;;; Commentary:

;;; This file loads all necessary plugins for a fresh Emacs install and
;;; configures the editor to run with the correct settings.

;;;; Code:

;;; Require Common Lisp
(require 'cl-lib)

;;; Require package manager
(require 'package)
(package-initialize)

;;; Add MELPA package repo to package manager
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)


;;; Packages to be installed at launch
(defvar emacs-packages
  '(auto-package-update
    auto-complete
    fill-column-indicator
    flycheck
    multiple-cursors
    powerline
    spacegray-theme
    web-mode
    whitespace))

;;; Predicate to check if all correct packages installed
(defun packages-installed-p ()
  "Loop through all packages and check if installed."
  (cl-loop for p in emacs-packages
	   when (not (package-installed-p p)) do (cl-return nil)
	   finally (cl-return t)))

;;; Loop through all necessary packages and installed any that are not found
(unless (packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p emacs-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;;; Set backup directory
(setq backup-directory-alist `(("." . "~/.emacs_saves")))

;;; Require IDO
(require 'ido)
(ido-mode t)

;;; Require Powerline
(powerline-default-theme)

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

;;; Set default indent size to 4
(setq-default tab-width 4)
(setq-default indent-tabs-mode t)

;;; Enable auto-indentation
(define-key global-map (kbd "RET") 'newline-and-indent)

;;; Hide menubar
(menu-bar-mode nil)

;;; Enable line numbers (and put space in the margin)
(global-linum-mode t)
(setq-default right-fringe-width  0)
(setq-default left-fringe-width  10)
(setq linum-format "%4d\u2502")

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
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;;; Add Spacegray color theme
(load-theme 'spacegray t)

;;; Highlight the current line
(global-hl-line-mode 1)

;;; Show whitespace
(global-whitespace-mode t)

(custom-set-faces
 '(whitespace-space ((t (:bold t :background nil :foreground "gray25"))))
 '(whitespace-empty ((t (:bold t :background nil :foreground "gray25"))))
 '(whitespace-hspace ((t (:bold t :background nil :foreground "gray25"))))
 '(whitespace-indentation ((t (:bold t :background nil :foreground "gray25"))))
 '(whitespace-newline ((t (:bold t :background nil :foreground "gray25"))))
 '(whitespace-space-after-tab ((t (:bold t :background nil :foreground "gray25"))))
 '(whitespace-space-before-tab ((t (:bold t :background nil :foreground "gray25"))))
 '(whitespace-tab ((t (:bold t :background nil :foreground "gray25"))))
 '(whitespace-trailing ((t (:bold t :background nil :foreground "gray25"))))
 )

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
(setq initial-frame-alist
      '(
        (width . 100) ; character
        (height . 40) ; lines
        ))

(provide `.emacs)

;;; .emacs ends here

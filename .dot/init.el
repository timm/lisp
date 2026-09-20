;;; init.el --- minimal: catppuccin-mocha + sly -*- lexical-binding: t -*-

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(setq use-package-always-ensure t)   ; auto-install on first run

;; less chrome, fewer beeps
(if (display-graphic-p)
    (progn (tool-bar-mode -1) (scroll-bar-mode -1))
  (xterm-mouse-mode 1)    ; tty: mouse events
  (menu-bar-mode -1)      ; no stray menu pops; F10 on demand
  (setq interprogram-cut-function     ; M-w/C-w -> macOS
        (lambda (text)                ; clipboard, via pbcopy
          (let ((p (make-process :name "pbcopy"
                     :command '("pbcopy")
                     :connection-type 'pipe)))
            (process-send-string p text)
            (process-send-eof p)))
        interprogram-paste-function   ; C-y pulls clipboard
        (lambda () (let ((s (shell-command-to-string "pbpaste")))
                     (unless (string= s "") s)))))
(setq inhibit-startup-screen t
      ring-bell-function 'ignore
      make-backup-files nil)

(use-package catppuccin-theme
  :custom (catppuccin-flavor 'mocha)
  :config (load-theme 'catppuccin :no-confirm))

(use-package sly
  :custom (inferior-lisp-program "/opt/homebrew/bin/sbcl"))

(unless (package-installed-p 'ghostel) ; terminal on libghostty
  (package-vc-install
   '(ghostel :url "https://github.com/dakra/ghostel"
             :lisp-dir "lisp")))
(use-package ghostel :ensure nil :commands (ghostel))

;; keep customize noise out of this file
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error)

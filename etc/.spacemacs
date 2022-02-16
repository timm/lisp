(setq column-number-mode t)
;(menu-bar-mode 0)

(progn
  (setq inhibit-startup-message t)
  (setq make-backup-files nil)
  (global-font-lock-mode t)
  (xterm-mouse-mode t)
  (show-paren-mode t)
  (transient-mark-mode t)
  (setq scroll-step 1)
  (global-hl-line-mode 1)
  (setq-default indent-tabs-mode nil)
  (setq-default fill-column 52))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-theme
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "78e6be576f4a526d212d5f9a8798e5706990216e9be10174e3f3b015b8662e27" default))
 '(package-selected-packages
   '(highlight-function-calls smudge sly-repl-ansi-color spacemacs-theme monokai-theme afternoon-theme sly slime ## slime-volleyball)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq inferior-lisp-program "/opt/homebrew/bin/sbcl")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;(load-theme 'monokai t)
(load-theme 'spacemacs-dark t)

(require 'recentf)
(recentf-mode 1)
(display-time-mode 1)
(column-mode 1)
;(require 'highlight-function-calls)


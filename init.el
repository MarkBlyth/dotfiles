;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Installing packages from MELPA
;; (also some emacs auto-generated stuff?)
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "No SSL connection for package installation - insecure!"))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(company-auto-complete nil)
 '(custom-enabled-themes (quote (doom-molokai)))
 '(custom-safe-themes
   (quote
    ("be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "774aa2e67af37a26625f8b8c86f4557edb0bac5426ae061991a7a1a4b1c7e375" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "229c5cf9c9bd4012be621d271320036c69a14758f70e60385e87880b46d60780" "7f791f743870983b9bb90c8285e1e0ba1bf1ea6e9c9a02c60335899ba20f3c94" "7b50dc95a32cadd584bda3f40577e135c392cd7fb286a468ba4236787d295f4b" "c520bbbddca1d7362d046635c5cc023b5f151b250ac9f8d6ce763afa804b7d1d" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f"))))
 '(org-agenda-files
   (quote
    ("~/OrgFiles/phd.org" "~/OrgFiles/personal.org" "~/OrgFiles/food.org")))
 '(org-export-backends (quote (ascii beamer html icalendar latex md odt org)))
 '(package-selected-packages
   (quote
    (hl-todo general elpy doom-themes evil-magit magit flycheck blacken python-black auto-complete pdf-tools org-bullets dashboard evil-visual-mark-mode spacemacs-theme which-key org-agenda-property))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;  Make use-package do its thing
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Make emacs install packages automatically
(require 'use-package-ensure)
(setq use-package-always-ensure t)


;; Use octave-mode for all .m files
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))


;; Autocomplete
(use-package auto-complete-config
  :ensure auto-complete
  :init
  (ac-config-default)
  (global-auto-complete-mode t)
)



;; which-key
(use-package which-key
  :init
  (setq whick-key-idle-delay 0.05) ; time between pressing a key and bringing up display
  :config
  (which-key-mode)
)


;; doom themes
(use-package doom-themes
  :defer t
)

;; evil
(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode t)
)


;; helm
(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x f" . helm-recentf)
         ("C-x b" . helm-buffers-list))
  :config (progn
	    (setq helm-buffers-fuzzy-matching t)
            (helm-mode 1))
)


;; dashboard
(use-package dashboard
  :init
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  :config
  (dashboard-setup-startup-hook)
)


;; nicer org bullet points
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
)



;; blacken, to nicely format python code
(use-package blacken)
(use-package python-black
  :demand t
  :after python
)


;; elpy (python ide stuff)
(use-package elpy
  :init
  (elpy-enable)
  ;; Automatically run Black on buffer save
;;  (add-hook 'elpy-mode-hook
;;          '(lambda ()
;;             (when (eq major-mode 'python-mode)
;;               (add-hook 'before-save-hook 'elpy-black-fix-code))))
)




;; flycheck (on-the-fly syntax checking)
(use-package flycheck
  :config
  (when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
  :after elpy
)


;; magit (git interface)
(use-package magit)

;; make evil keybindings work with magit
(use-package evil-magit)


;; Highlight TODOs
(use-package hl-todo
  :config
  (global-hl-todo-mode)
)

;; Nice key binds
(use-package general)
(general-evil-define-key 'normal 'global
 :prefix "SPC"
 "" nil
 "r" 'helm-recentf
 "f" 'helm-find-files
 "b" 'helm-buffers-list
 "g" 'magit
 "p" 'hl-todo-previous
 "n" 'hl-todo-next
 "o" 'hl-todo-occur
 "i" 'hl-todo-insert
 "3" 'split-window-right
 "0" 'delete-window
) 

;;;;;;;;;;;;;;;;;;;;;;
;; Config
;;;;;;;;;;;;;;;;;;;;;;

;; Misc
(setq delete-old-versions -1 )		; delete excess backup versions silently
(setq vc-make-backup-files t )		; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
(setq vc-follow-symlinks t )	    ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq ring-bell-function 'ignore )	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 )	; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(menu-bar-mode -1)
(global-linum-mode t)
(global-visual-line-mode t)

;; org hotkeys
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)


;; Fix RET doing weird stuff sometimes
(let ((x (key-binding "\C-j")))
  (local-set-key "\C-m" x))


;; Org mode TODO types
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "PENDING(p)" "|" "CLAIMED")
        (sequence "WRITE(W)" "WRITING(w)" "REWRITE(R)" "|" "COMPLETED(c)"))
)


;; Open shells in the currently activated window
(add-to-list 'display-buffer-alist
             `(,(regexp-quote "*shell") display-buffer-same-window))

;; Always scroll to the bottom of python shells
(add-hook 'inferior-python-mode-hook
          (lambda ()
            (setq comint-move-point-for-output t)))

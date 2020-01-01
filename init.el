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
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(org-agenda-files
   (quote
    ("~/OrgFiles/phd.org" "~/OrgFiles/personal.org" "~/OrgFiles/food.org")))
 '(org-export-backends (quote (ascii beamer html icalendar latex md odt org)))
 '(package-selected-packages
   (quote
    (flycheck blacken elpy python-black auto-complete pdf-tools org-bullets dashboard evil-visual-mark-mode spacemacs-theme which-key org-agenda-property))))
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


;; spacemacs theme
(use-package spacemacs-theme
  :defer t
  :init (load-theme 'spacemacs-dark t)
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
  :bind (("M-a" . helm-M-x)
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
  :config
  (elpy-enable)
  ;; Automatically run Black on buffer save
  (add-hook 'elpy-mode-hook
          '(lambda ()
             (when (eq major-mode 'python-mode)
               (add-hook 'before-save-hook 'elpy-black-fix-code))))
)




;; flycheck (on-the-fly syntax checking)
(use-package flycheck
  :config
  (when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
  :after elpy
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

;; org hotkeys
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Org mode TODO types
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "PENDING(p)" "|" "CLAIMED")
        (sequence "WRITE(W)" "WRITING(w)" "REWRITE(R)" "|" "COMPLETED(c)"))
)


;; Open shells in the currently activated window
(add-to-list 'display-buffer-alist
             `(,(regexp-quote "*shell") display-buffer-same-window))

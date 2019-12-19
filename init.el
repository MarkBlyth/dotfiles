;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Codes required for installing packages from MELPA

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


;; Autocomplete
(ac-config-default)
(global-auto-complete-mode t)


;; Open shells in the currently activated window
(add-to-list 'display-buffer-alist
             `(,(regexp-quote "*shell") display-buffer-same-window))

;; which-key
(setq whick-key-idle-delay 0.05) ; time between pressing a key and bringing up display
(require 'which-key)
(which-key-mode)

;; spacemacs theme
(load-theme 'spacemacs-dark)


;; evil
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode t)


;; helm
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "C-x C-f") 'helm-find-files)


;; dashboard
(require 'dashboard)
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
;;
(dashboard-setup-startup-hook)
;; For use-package...
;;(use-package dashboard
;;             :ensure t
;;             :config
;;             (dashboard-setup-startup-hook))


;; nicer org bullet points
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))


;; elpy (python ide stuff)
(elpy-enable)


;; Automatically run Black on buffer save
; M-x package-install blacken
(add-hook 'elpy-mode-hook
          '(lambda ()
             (when (eq major-mode 'python-mode)
               (add-hook 'before-save-hook 'elpy-black-fix-code))))


;; flycheck (on-the-fly syntax checking)
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))


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

;; TODOs
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "PENDING(p)" "|" "CLAIMED")
        (sequence "WRITE(W)" "WRITING(w)" "REWRITE(R)" "|" "COMPLETED(c)"))
)


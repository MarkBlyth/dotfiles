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
 '(company-auto-commit nil)
 '(company-auto-complete nil)
 '(company-insertion-on-trigger nil)
 '(custom-enabled-themes '(doom-molokai))
 '(custom-safe-themes
   '("0466adb5554ea3055d0353d363832446cd8be7b799c39839f387abb631ea0995" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "774aa2e67af37a26625f8b8c86f4557edb0bac5426ae061991a7a1a4b1c7e375" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "229c5cf9c9bd4012be621d271320036c69a14758f70e60385e87880b46d60780" "7f791f743870983b9bb90c8285e1e0ba1bf1ea6e9c9a02c60335899ba20f3c94" "7b50dc95a32cadd584bda3f40577e135c392cd7fb286a468ba4236787d295f4b" "c520bbbddca1d7362d046635c5cc023b5f151b250ac9f8d6ce763afa804b7d1d" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(hl-todo-keyword-faces
   '(("TODO" . "#f2241f")
     ("CHECK" . "#f2241f")
     ("NEXT" . "#4f97d7")
     ("FINISH" . "#4f97d7")
     ("DONE" . "#86dc2f")
     ("OKAY" . "#86dc2f")
     ("TEMP" . "#b1951d")
     ("FIX" . "#dc752f")))
 '(org-agenda-files nil)
 '(org-export-backends '(ascii beamer html icalendar latex md odt org))
 '(org-latex-classes
   '(("beamer" "\\documentclass[presentation]{beamer}
[DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]
"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("article" "\\documentclass[11pt]{article}
\\usepackage[usenames,dvipsnames,svgnames,table]{xcolor}
\\newenvironment{note}{\\color{red}\\bfseries ZZZ}


[DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]
"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("springer" "\\documentclass{svjour3}
\\usepackage[usenames,dvipsnames,svgnames,table]{xcolor}
\\renewenvironment{note}{\\color{red}\\bfseries ZZZ}


[DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]
"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("report" "\\documentclass[11pt]{report}
\\usepackage[usenames,dvipsnames,svgnames,table]{xcolor}
\\newenvironment{note}{\\color{red}\\bfseries ZZZ}


[DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]
"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("book" "\\documentclass[11pt]{book}
[DEFAULT-PACKAGES]
\\usepackage[usenames,dvipsnames,svgnames,table]{xcolor}
\\newenvironment{note}{\\color{red}\\bfseries ZZZ}


[PACKAGES]
[EXTRA]
"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
 '(org-structure-template-alist
   '(("c" . "center")
     ("C" . "comment")
     ("e" . "example")
     ("E" . "export")
     ("h" . "export html")
     ("l" . "export latex")
     ("q" . "quote")
     ("s" . "src")
     ("n" . "note")
     ("d" . "definition")
     ("t" . "theorem")
     ("a" . "abstract")
     ("" . "")))
 '(package-selected-packages
   '(lsp-ui lsp-mode rustic evil-collection company-tabnine use-package company-quickhelp company rainbow-delimiters markdown-mode helm-bibtex org-ref hl-todo general doom-themes evil-magit magit flycheck blacken python-black pdf-tools org-bullets dashboard evil-visual-mark-mode spacemacs-theme which-key org-agenda-property)))
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


;; Fast insertion of structure templates
(require 'org-tempo)


(use-package rainbow-delimiters
  :config
  (add-hook 'python-mode-hook 'org-mode-hook #'rainbow-delimiters-mode)
)


(use-package company
  :init
  (setq company-idle-delay 0)
  (setq company-show-numbers t)
  ;; :config
  ;; (add-hook 'python-mode-hook 'rustic-mode-hook #'company-mode)
  :hook (prog-mode . company-mode)
)

(use-package company-tabnine :ensure t)
(add-to-list 'company-backends #'company-tabnine)

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode)
)

;; Use octave-mode for all .m files
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))


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


;; undo-tree; gets undo and redo to work in evil
(use-package undo-tree
  :ensure t
  :after evil
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))

;; evil
(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-keybinding nil)
;;  (evil-define-key 'normal evil-jumper-mode-map (kbd "TAB") nil)
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
(use-package python-black
  :demand t
  :after python
)



;; flycheck (on-the-fly syntax checking)
(use-package flycheck
  :init (global-flycheck-mode)
  :config
;;  (setq flycheck-global-modes '(not org-mode))
  (add-hook 'after-init-hook #'global-flycheck-mode)
)

(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
        (id (one-or-more (not (any " "))))
        (message) line-end))
  :modes (text-mode markdown-mode gfm-mode org-mode))
(add-to-list 'flycheck-checkers 'proselint)


;; magit (git interface)
(use-package magit)


;; Get evil in magit
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init 'magit))


;; Highlight TODOs
(use-package hl-todo
  :init
  (global-hl-todo-mode 1)
)


;; Use org-ref as a bibliography manager
(use-package org-ref
    :config
    (setq org-ref-default-bibliography '("~/PhD/OrgFiles/refs/references.bib"))
)
;; Use helm-bibtex to find refs
(use-package helm-bibtex
  :config
;;    (setq org-latex-pdf-process (quote ("texi2dvi -p -b -V %f")))
    (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
    (setq bibtex-completion-bibliography "~/Files/PhD/OrgFiles/refs/references.bib")
    (setq reftex-default-bibliography '("~/Files/PhD/OrgFiles/refs/references.bib"))
    (helm-delete-action-from-source "Insert citation" helm-source-bibtex)
    (helm-add-action-to-source "Insert citation" 'helm-bibtex-insert-citation helm-source-bibtex 0)
)


(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


;; Rust setup
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;; Rust setup
(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; Rust setup
(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))



;; Nice key binds
(use-package general)
(general-evil-define-key 'normal 'global
 :prefix "SPC"
 "" nil
 ;; files
 "r" 'helm-recentf
 "f" 'helm-find-files
 ;; windows
 "b" 'helm-buffers-list
 "3" 'split-window-right
 "0" 'delete-window
 ;; TODOs
 "tp" 'hl-todo-previous
 "tn" 'hl-todo-next
 "to" 'hl-todo-occur
 "ti" 'hl-todo-insert
 ;; others
 "g" 'magit
 "o" 'other-window
 "c" 'flycheck-list-errors
 "s" 'flyspell-auto-correct-word
 "d" 'org-time-stamp-inactive
)

(general-evil-define-key 'normal 'global
    :keymaps 'org-mode-map
    :prefix "SPC"
    ;; org specific
    "ae" 'org-latex-export-to-pdf
    "ab" 'org-beamer-export-to-latex
    "al" 'org-toggle-latex-fragment
    "ap" 'org-set-property
    "at" 'org-todo
    "ac" 'helm-bibtex
)
(general-evil-define-key 'normal 'global
  :keymaps 'python-mode-map
  :prefix "SPC"
  "pb" 'python-black-buffer
)


;;;;;;;;;;;;;;;;;;;;;;
;; Config
;;;;;;;;;;;;;;;;;;;;;;

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

;; Handy key definition
(define-key global-map "\C-\M-Q" 'unfill-region)


;; Misc

 (defun flyspell-on-for-buffer-type ()
      "Enable Flyspell appropriately for the major mode of the current
buffer. Uses `flyspell-prog-mode' for modes derived from `prog-mode',
so only strings and comments get checked. All other buffers get
`flyspell-mode' to check all text. If flyspell is already enabled,
does nothing."
      (interactive)
      (if (not (symbol-value flyspell-mode)) ; if not already on
	(progn
	  (if (derived-mode-p 'prog-mode)
	    (progn
	      (message "Flyspell on (code)")
	      (flyspell-prog-mode))
	    ;; else
	    (progn
	      (message "Flyspell on (text)")
	      (flyspell-mode 1)))
	  ;; I tried putting (flyspell-buffer) here but it didn't seem to work
	  )))
    
    (defun flyspell-toggle ()
      "Turn Flyspell on if it is off, or off if it is on. When turning
on, it uses `flyspell-on-for-buffer-type' so code-vs-text is handled
appropriately."
      (interactive)
      (if (symbol-value flyspell-mode)
	  (progn ; flyspell is on, turn it off
	    (message "Flyspell off")
	    (flyspell-mode -1))
	  ; else - flyspell is off, turn it on
	  (flyspell-on-for-buffer-type)))
(add-hook 'find-file-hook 'flyspell-on-for-buffer-type)
(add-hook 'org-mode-hook 'turn-on-flyspell)
;;(add-hook 'org-mode-hook 'flyspell-buffer)
;;(add-hook 'after-change-major-mode-hook 'flyspell-on-for-buffer-type)


(setq ispell-dictionary "british")
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

;; Remap c-u to c-f, and set c-u as evil scroll
(define-key global-map (kbd "C-f") 'universal-argument)
(define-key universal-argument-map (kbd "C-u") nil)
(define-key universal-argument-map (kbd "C-f") 'universal-argument-more)
(define-key global-map (kbd "C-u") 'kill-whole-line)
(eval-after-load 'evil-maps
  '(progn
     (define-key evil-motion-state-map (kbd "C-f") nil)
     (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)))


;; Org mode TODO types
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
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

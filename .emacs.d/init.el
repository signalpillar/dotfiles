(setq-default inhibit-startup-screen t)
;; update "PATH" variable that is used from non-login shell
(let (
      (mypaths
       (list 
         "/home/spillar/bin"
         (getenv "PATH")
         )
       ))

  (setenv "PATH" (mapconcat 'identity mypaths ":") )
  (setq exec-path (append mypaths (list "." exec-directory)) )) 

(require 'package)
;; declare repositories that will be used by package.el
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
	     '("gnu" . "http://elpa.gnu.org/packages/"))

;; load packages installed with package manager
(package-initialize)
;; Add in your own as you wish:
(defvar my-packages '(evil
                      paredit
                      auto-complete
                      ac-slime slime-fuzzy
                           elscreen
			   fill-column-indicator
			   ;clojure related
			   clojure-mode clojure-test-mode nrepl
			   )
  "A list of packages to ensure are installed at launch.")
(dolist (p my-packages)
      (when (not (package-installed-p p))
              (package-install p)))

; gui
(tool-bar-mode 0)

; global editor settings
(setq-default indent-tabs-mode nil)        ; use only spaces (no tabs at all)
(set-frame-font "Monaco-10")               ; change font
(load-theme 'tango-dark)
(column-number-mode t)
(size-indication-mode t)                   ; show file size
(global-hl-line-mode -1)                   ; disable current line hightlighting
;; show right margin (80symb)
(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda () (fci-mode 1)))
(global-fci-mode 1)
(setq-default fill-column 79)
; show line numbers always
(global-linum-mode 1)
; show paren mode
(show-paren-mode 1)
(setq show-paren-style 'expression)
(set-face-background 'show-paren-match-face "#4F5152")
;;; enable evil-mode
(require 'evil)
(evil-mode 1)

;; autocomplete
(require 'auto-complete-config)
;; show menu after 0.5 seconds
(setq ac-auto-show-menu 0.5)
(setq ac-show-menu-immediately-on-auto-complete t)
(setq ac-use-fuzzy t)
;; ignore case
(setq ac-ignore-case t)
(ac-config-default)
;; ac-slime
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

      
;; paredit
(autoload 'paredit-mode "paredit" t)
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))

;; eldoc
(require 'eldoc)
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

;;ido
(custom-set-variables
 '(ido-enable-last-directory-history t)
 '(ido-record-commands t)
 '(ido-max-work-file-list 10)
 '(ido-max-work-directory-list 5))

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; nrepl
;; enable eldoc in clojure buffers
(add-hook 'nrepl-interaction-mode-hook
	  'nrepl-turn-on-eldoc-mode)
;; stop the error buffer from popping up while working in the REPL
(setq nrepl-popup-stacktraces nil)


;; recentf
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)
(global-set-key [(meta f12)] 'recentf-open-files)

;; ibuffer configuration
(setq ibuffer-shrink-to-minimum-size t)
(setq ibuffer-always-show-last-buffer nil)
(setq ibuffer-sorting-mode 'recency)
(setq ibuffer-use-header-line t)
(global-set-key [(f12)] 'ibuffer)

; clojure 
(setq inferior-lisp-program "lein repl")
(add-hook 'clojure-mode-hook
          (lambda ()
            ((turn-on-paredit)
             )))

; elscreen
(load "elscreen" "ElScreen" t)
(setq elscreen-prefix-key (kbd "C-c c")
      elscreen-display-tab nil)
(global-unset-key elscreen-prefix-key)
(global-set-key elscreen-prefix-key elscreen-map)
(elscreen-start)

;; used resources
; http://www.xsteve.at/prg/emacs/power-user-tips.html

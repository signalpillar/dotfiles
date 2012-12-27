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

(evil-mode 1)

; gui
(tool-bar-mode 0)
(menu-bar-mode t)
;(load-theme 'wombat)

; disable creation of backup files
(setq make-backup-files nil)
; global editor settings
(setq-default indent-tabs-mode nil)        ; use only spaces (no tabs at all)
(set-frame-font "Inconsolata-11")               ; change font
(column-number-mode t)
(size-indication-mode t)                   ; show file size
;; (global-hl-line-mode -1)                   ; disable current line hightlighting
;; show right margin (80symb)
;; (define-globalized-minor-mode global-fci-mode fci-mode
;;   (lambda () (fci-mode 1)))
;; (global-fci-mode 1)
(setq-default fill-column 79)

; show line numbers always
(global-linum-mode -1)

; show paren mode
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)
(set-face-background 'show-paren-match-face "#bAA")


;; paredit
;; (autoload 'paredit-mode "paredit" t);
;; (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
;; (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
;; (add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
;; (add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))


;; recentf
;; (recentf-mode 1)
;; (setq recentf-max-saved-items 500)
;; (setq recentf-max-menu-items 60)
;; (global-set-key [(meta f12)] 'recentf-open-files)


; clojure 
(setq inferior-lisp-program "lein repl")
(defun compile-clj-buffer ()
  "Compile current clojure buffer"
  )

(defun enable-paredit ()
  (paredit-mode 1))
(add-hook 'clojure-mode-hook 'enable-paredit)
;; (add-hook 'clojure-mode-hook
;;           '(lambda ()
;;              (add-hook 'after-save-hook 'slime-compile-and-load-file)))


(global-set-key [(f2)] 'sr-speedbar-toggle)
(global-set-key [(f4)] 'sr-speedbar-select-window)
(global-set-key [(f5)] 'rever-buffer)

; (require 'graphene)

; (setq graphene-linum-auto nil)

(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))

(ido-mode t)


; enable bindings to move among windows (Shift + Arrow Key)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))


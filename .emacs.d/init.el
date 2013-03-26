(setq-default inhibit-startup-screen t)
;; update "PATH" variable that is used from non-login shell
(let (
      (mypaths
       (list 
         "/usr/bin/"
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
;; (tool-bar-mode 0)
(menu-bar-mode t)
;(load-theme 'wombat)

; disable creation of backup files
(setq make-backup-files nil)
; global editor settings
(setq-default indent-tabs-mode nil)        ; use only spaces (no tabs at all)
;(set-frame-font "Inconsolata-12")               ; change font
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
; (add-hook 'clojure-mode-hook '(lambda () ;(add-hook 'after-save-hook 'slime-compile-and-load-file)))


(global-set-key [(f2)] 'sr-speedbar-toggle)
(global-set-key [(f4)] 'sr-speedbar-select-window)
(global-set-key [(f5)] 'rever-buffer)

(require 'graphene)
(setq graphene-linum-auto nil)

(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))

(ido-mode t)
;; show recently opened buffers but closed in ido
(setq ido-use-virtual-buffers t)

;; minimal comfort
(fset 'yes-or-no-p 'y-or-n-p)

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(ac-config-default)

(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)

;; dirty fix for having AC everywhere
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()                       
                       (if (not (minibufferp (current-buffer)))                           
                           (auto-complete-mode 1))))
(real-global-auto-complete-mode t)

(defun jao-toggle-selective-display ()
  (interactive)
  (set-selective-display (if selective-display nil 1)))

(global-set-key [f3] 'jao-toggle-selective-display)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil))



(defun collect-regexp-results (regex)
  ;;; collects all the matches of regex in a buffer called *collect-result*
  ;;; then switches to that buffer
  ;;; TODO refactor this to take the region as a parameter
  (interactive "Mregex to search for: ")
  (let ((curmin (region-or-buffer-beginning))
        (curmax (region-or-buffer-end)))
    (save-excursion
      (goto-char curmin)
      ;; (goto-char (region-or-buffer-beginning))
      (while (re-search-forward regex curmax t)
        (let ((retval (match-string-no-properties 0)))
          (with-current-buffer (get-buffer-create "*collect results*")
            (insert retval)
            (insert "\n"))))
      (switch-to-buffer "*collect results*"))))
 
(defun collect-ip-addresses ()
  (interactive)
  (collect-regexp-results "[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+"))

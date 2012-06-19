(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; enable evil-mode
(require 'evil)
(evil-mode 1)


;; clojure-mode
(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook
          (lambda ()
            (turn-on-paredit)
            (turn-on-eldoc-mode)
            (show-paren-mode t)
            (set-up-slime-ac)))

;; render line numbers
(global-linum-mode)

;; multi-scratch
(add-to-list 'load-path "~/.emacs.d/misc")
(require 'multi-scratch)
(global-set-key (kbd "C-<f11>") 'multi-scratch-new)

;;tabbar mode
(tabbar-mode 1)


;; auto-complete
(require 'auto-complete-config)
(ac-config-default)

(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

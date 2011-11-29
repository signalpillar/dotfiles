;;; Emacs load path
(setq load-path (cons "~/.emacs.d" load-path))
(require 'dirtree)
;;; {{{ Python mode
(add-to-list 'load-path "~/.emacs.d/python-mode")
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;;; }}}
;;; {{{ Scala mode
(add-to-list 'load-path "~/.emacs.d/scala-mode")
(require 'scala-mode-auto)
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))
;;; }}}
;;; {{{ Ensime
(add-to-list 'load-path "~/.emacs.d/ensime/elisp")
(require 'ensime)
;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
;;; }}}


;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)
;; Display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; show line numbers
(autoload 'linum-mode "linum" "toggle line numbers on/off" t)
(global-set-key (kbd "C-<f5>") 'linum-mode)

; shortcuts
; M-g g => go to line
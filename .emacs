;;; Emacs load path
(setq load-path (cons "~/.emacs.d" load-path))
(require 'dirtree)
;;; {{{ Python mode
(add-to-list 'load-path "~/.emacs.d/python-mode")
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;;; }}}
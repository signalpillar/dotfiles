(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/auto-install/")
(require 'auto-install)

;; allow selection deletion
(delete-selection-mode t)

;;; {{{ Scala mode
(add-to-list 'load-path "~/.emacs.d/scala-mode")
(require 'scala-mode-auto)
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))
;;; }}}
;;; {{{ Ensime
(add-to-list 'load-path "~/.emacs.d/ensime/elisp")
(require 'ensime)

;; free of trailing whitespace and to use 80-column width, standard indentation
(setq whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 80)

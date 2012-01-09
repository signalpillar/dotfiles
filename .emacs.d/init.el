(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/auto-install/")
(require 'auto-install)

;; allow selection deletion
(delete-selection-mode t)

;;; extensible vi layer
(add-to-list 'load-path "~/.emacs.d/evil-evil")
(require 'evil)
(evil-mode 1)


(hl-line-mode)

;; auto-complete configuration
(add-to-list 'load-path "~/.emacs.d/auto-complete/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete//ac-dict")
(ac-config-default)

;; scala-mode
(require 'scala-mode)
(add-hook 'scala-mode-hook
          '(lambda ()
             (yas/minor-mode-on)
             ))

;; enable color theme solarized
(color-theme-solarized-light)


;; recent mode
(recentf-mode 1)
(global-set-key "\C-cf" 'recentf-open-files)

;; enable menu bar
(menu-bar-mode 1)
;; show column number along with row
(column-number-mode t)
;; font configuration
(set-default-font "DejaVu Sans Mono-11")

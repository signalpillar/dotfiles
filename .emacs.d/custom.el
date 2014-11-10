;;; init.el --- Custom configuration used as part of Prelude
;;
;; Copyright (c) 2014 Volodymyr Vitvitskyi
;;
;; Version: 1.0.0
;; Keywords: evil, python, haskell, clojure

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(menu-bar-mode 0)
; disable creation of backup files
(setq make-backup-files nil)
(disable-theme 'zenburn)
(setq prelude-whitespace nil)
(setq prelude-flyspell nil)

; global editor settings
(setq-default indent-tabs-mode nil)        ; use only spaces (no tabs at all)
; (column-number-mode nil)
(size-indication-mode t)                   ; show file size
; (setq-default fill-column 79)

; show paren mode
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)
(set-face-background 'show-paren-match-face "#bAA")

;; show recently opened buffers but closed in ido
(ido-vertical-mode 1)

;; minimal comfort
(fset 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode -1)

(defun jao-toggle-selective-display ()
  (interactive)
  (set-selective-display (if selective-display nil 1)))

(global-set-key [f3] 'jao-toggle-selective-display)

;; show (in left margin) marker for empty lines
;; (setq-default indicate-empty-lines nil)

(prelude-require-package 'edit-server)
(edit-server-start)

(elpy-enable)
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
(define-key global-map (kbd "C-c O") 'iedit-mode)

;; jump to the top location, jump back in the navigation history
(define-key global-map (kbd "M-[") 'pop-global-mark)


;; evil-model configuration
;; https://github.com/bradleywright/emacs.d/blob/master/setup-evil.el
(setq
 ;; this stops evil from overwriting the cursor color
 evil-default-cursor t
 evil-default-state 'normal
 ;; Don't move back the cursor one position when exiting insert mode
 evil-move-cursor-back nil
 )

(global-evil-search-highlight-persist t)

;; Make C-g work like <esc>
(define-key evil-normal-state-map "\C-g" 'evil-normal-state)
(define-key evil-visual-state-map "\C-g" 'evil-normal-state)
(define-key evil-insert-state-map "\C-g" 'evil-normal-state)
(define-key evil-normal-state-map (kbd ",f") 'projectile-find-file)
(define-key evil-normal-state-map (kbd ",p") 'helm-projectile)
(define-key evil-normal-state-map (kbd ",,") 'evil-buffer)
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
;; fix C-u in evil mode - it has to scroll up by screen
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(evil-define-keymap evil-insert-state-modes "<tab>" 'evil-jump-forward)

;; evil-surround - Emacs version of surround.vim
;; https://github.com/timcharper/evil-surround
(require 'evil-surround)

;; trim spaces before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; open recent file when emacs starts
(recentf-mode 1)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(electric-indent-mode)
;; python configuration
(add-hook 'python-mode-hook #'lambda-mode 1)

(yas-global-mode 1)

;; fixing a keybinding bugs in elpy
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
(define-key global-map (kbd "C-c o") 'iedit-mode)

(setq prelude-whitespace nil)
(setq prelude-guru nil)

(elpy-use-ipython)

;; configure clojure model
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)

(setq-default cursort-type 'bar)

(add-hook 'hy-mode-hook 'paredit-mode)

; from http://juanjoalvarez.net/es/detail/2014/sep/19/vim-emacsevil-chaotic-migration-guide/
; configure powerline like in vim (shows current mode)
(require 'powerline)
(powerline-evil-vim-color-theme)
(display-time-mode)

; smooth scrolling
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

; start maximazed
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized))))) ;; start maximized

; coding style and spaces instead of tabs
(setq-default tab-width 4
              indent-tabs-mode nil)

; determine indentation style of opened file
(dtrt-indent-mode 1)
; auto-indent with Return key
(define-key global-map (kbd "RET")
  'newline-and-indent)

(define-key python-mode-map (kbd "M-b") '(elpy-goto-definition))
(add-hook 'python-mode-hook (lambda ()
                              ; fill-column-indicator
                              ; (fci-mode)
                              (color-identifiers-mode)
                              ; (set-fill-column 79)
                              ))

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional

; don't create backup files
(setq make-backup-files nil)

; disable scroll bars
(scroll-bar-mode -1)

(global-linum-mode -1)

(helm-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)


; haskell mode configuration
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;; configure to load "github" theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'github t)
(color-theme-approximate-on)
(message "evaluated from custom.el")
;;; custom.el ends here

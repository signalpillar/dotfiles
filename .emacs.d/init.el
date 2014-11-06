;;; init.el --- Prelude's configuration entry point.
;;
;; Copyright (c) 2011 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://batsov.com/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file simply sets up the default load path and requires
;; the various modules defined within Emacs Prelude.

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
(defvar current-user
      (getenv
       (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Prelude is powering up... Be patient, Master %s!" current-user)

(when (version< emacs-version "24.1")
  (error "Prelude requires at least GNU Emacs 24.1, but you're running %s" emacs-version))

;; Always load newest byte code
(setq load-prefer-newer t)

(defvar prelude-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Prelude distribution.")
(defvar prelude-core-dir (expand-file-name "core" prelude-dir)
  "The home of Prelude's core functionality.")
(defvar prelude-modules-dir (expand-file-name  "modules" prelude-dir)
  "This directory houses all of the built-in Prelude modules.")
(defvar prelude-personal-dir (expand-file-name "personal" prelude-dir)
  "This directory is for your personal configuration.

Users of Emacs Prelude are encouraged to keep their personal configuration
changes in this directory.  All Emacs Lisp files there are loaded automatically
by Prelude.")
(defvar prelude-personal-preload-dir (expand-file-name "preload" prelude-personal-dir)
  "This directory is for your personal configuration, that you want loaded before Prelude.")
(defvar prelude-vendor-dir (expand-file-name "vendor" prelude-dir)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
(defvar prelude-savefile-dir (expand-file-name "savefile" prelude-dir)
  "This folder stores all the automatically generated save/history-files.")
(defvar prelude-modules-file (expand-file-name "prelude-modules.el" prelude-dir)
  "This files contains a list of modules that will be loaded by Prelude.")

(unless (file-exists-p prelude-savefile-dir)
  (make-directory prelude-savefile-dir))

(defun prelude-add-subfolders-to-load-path (parent-dir)
 "Add all level PARENT-DIR subdirs to the `load-path'."
 (dolist (f (directory-files parent-dir))
   (let ((name (expand-file-name f parent-dir)))
     (when (and (file-directory-p name)
                (not (string-prefix-p "." f)))
       (add-to-list 'load-path name)
       (prelude-add-subfolders-to-load-path name)))))

;; add Prelude's directories to Emacs's `load-path'
(add-to-list 'load-path prelude-core-dir)
(add-to-list 'load-path prelude-modules-dir)
(add-to-list 'load-path prelude-vendor-dir)
(prelude-add-subfolders-to-load-path prelude-vendor-dir)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; preload the personal settings from `prelude-personal-preload-dir'
(when (file-exists-p prelude-personal-preload-dir)
  (message "Loading personal configuration files in %s..." prelude-personal-preload-dir)
  (mapc 'load (directory-files prelude-personal-preload-dir 't "^[^#].*el$")))

(message "Loading Prelude's core...")

;; the core stuff
(require 'prelude-packages)
(require 'prelude-ui)
(require 'prelude-custom)  ;; Needs to be loaded before core and editor
(require 'prelude-core)
(require 'prelude-mode)
(require 'prelude-editor)
(require 'prelude-global-keybindings)

;; OSX specific settings
(when (eq system-type 'darwin)
  (require 'prelude-osx))

(message "Loading Prelude's modules...")

;; the modules
(when (file-exists-p prelude-modules-file)
  (load prelude-modules-file))

;; config changes made through the customize UI will be store here
(setq custom-file (expand-file-name "custom.el" prelude-personal-dir))

;; load the personal settings (this includes `custom-file')
(when (file-exists-p prelude-personal-dir)
  (message "Loading personal configuration files in %s..." prelude-personal-dir)
  (mapc 'load (directory-files prelude-personal-dir 't "^[^#].*el$")))

(message "Prelude is ready to do thy bidding, Master %s!" current-user)

(prelude-eval-after-init
 ;; greet the use with some useful tip
 (run-at-time 5 nil 'prelude-tip-of-the-day))

(menu-bar-mode 0)
; disable creation of backup files
(setq make-backup-files nil)
(disable-theme 'zenburn)
(setq prelude-whitespace nil)
(setq prelude-flyspell nil)
(global-linum-mode -1)

; global editor settings
(setq-default indent-tabs-mode nil)        ; use only spaces (no tabs at all)
(column-number-mode t)
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
(setq-default indicate-empty-lines t)

(prelude-require-package 'edit-server)
(edit-server-start)

(elpy-enable)
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
(define-key global-map (kbd "C-c O") 'iedit-mode)

;; jump to the top location, jump back in the navigation history
(define-key global-map (kbd "M-[") 'pop-global-mark)

;;; init.el ends here

;; evil-model configuration
; Don't move back the cursor one position when exiting insert mode
(setq evil-move-cursor-back nil)

(define-key evil-normal-state-map (kbd ",f") 'projectile-find-file)
(define-key evil-normal-state-map (kbd ",p") 'helm-projectile)
(define-key evil-normal-state-map (kbd ",,") 'evil-buffer)
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
;; fix C-u in evil mode - it has to scroll up by screen
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

(evil-define-keymap evil-insert-state-modes "<tab>" 'evil-jump-forward)

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

(color-theme-approximate-on)
(global-evil-search-highlight-persist t)
; (evil-leader/set-key "SPC" 'evil-search-highlight-persist-remove-all)

(helm-mode 1)

(global-set-key (kbd "M-x") 'helm-M-x)
; (load-theme 'misterioso t)
(load-theme 'tango t)


; haskell mode configuration
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

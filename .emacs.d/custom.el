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

(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(disable-theme 'zenburn)

; disable menu bar
(menu-bar-mode 0)
(tool-bar-mode 0)
; disable creation of backup files
(setq make-backup-files nil)
(setq prelude-whitespace nil)
(setq prelude-flyspell nil)

; global editor settings
(setq-default indent-tabs-mode nil)        ; use only spaces (no tabs at all)
; (column-number-mode nil)
; (size-indication-mode -1)                   ; show file size
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


;; trim spaces before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; open recent file when emacs starts
(recentf-mode 1)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(electric-indent-mode)
;; python configuration
(add-hook 'python-mode-hook #'pretty-lambda-mode 1)

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

; smooth scrolling
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

; coding style and spaces instead of tabs
(setq-default tab-width 4
              indent-tabs-mode nil)

; determine indentation style of opened file
(dtrt-indent-mode 1)
; auto-indent with Return key
; (define-key global-map (kbd "RET")
;  'newline-and-indent)

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
(color-theme-approximate-on)
(message "evaluated from custom.el")

;; evil configuration
;; evil-model configuration
;; https://github.com/bradleywright/emacs.d/blob/master/setup-evil.el
(setq
 ;; this stops evil from overwriting the cursor color
 evil-default-cursor t
 evil-default-state 'normal
 ;; Don't move back the cursor one position when exiting insert mode
 evil-move-cursor-back nil
 )

; from http://juanjoalvarez.net/es/detail/2014/sep/19/vim-emacsevil-chaotic-migration-guide/
; configure powerline like in vim (shows current mode)
(require 'powerline)
(powerline-evil-vim-color-theme)
(display-time-mode -1)

;; (message (format "%s" mode-line-format))


(global-evil-search-highlight-persist -1)

;; Make C-g work like <esc>
(define-key evil-normal-state-map "\C-g" 'evil-normal-state)
(define-key evil-visual-state-map "\C-g" 'evil-normal-state)
(define-key evil-insert-state-map "\C-g" 'evil-normal-state)
(define-key evil-normal-state-map (kbd ",f") 'projectile-find-file)
(define-key evil-normal-state-map (kbd ",gt") 'helm-etags-select)
(define-key evil-normal-state-map (kbd ",p") 'helm-projectile)
(define-key evil-normal-state-map (kbd ",o") 'elpy-goto-definition)
(define-key evil-normal-state-map (kbd ",,") 'evil-buffer)
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
;; fix C-u in evil mode - it has to scroll up by screen
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(evil-define-keymap evil-insert-state-modes "<tab>" 'evil-jump-forward)

(global-evil-leader-mode)
; to make it work in all modes
(setq evil-leader/in-all-states 1)
(evil-leader/set-leader ",")


;; https://github.com/ShingoFukuyama/helm-swoop
(global-set-key (kbd "M-i") 'helm-swoop)
;; When doing evil-search, hand the word over to helm-swoop
(define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)
;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)
;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)
;; Go to the opposite side of line from the end or beginning of line
(setq helm-swoop-move-to-line-cycle t)

(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)


(evil-leader/set-key
  "s" 'tw/helm-occur-python
  "d" 'dash-at-point
  "D" 'dash-at-point-with-docset
  "<SPC>" 'company-yasnippet

  ;; neotree
  "e" 'neotree-toggle

  ;; extended commands
  "xt" 'create-tags

  ;; helm
  "gg" 'helm-git-grep
  "gt" 'helm-etags-select
  "hc" 'helm-flycheck
  "h#" 'helm-themes
  "hgg" 'helm-ag
  "hgd" 'helm-do-ag

  ;; shell
  "ts" 'shell
  "te" 'eshell
  "tt" 'ansi-term
  "tp" 'shell-pwd

  ;; projectile/perspective
  "pf" 'projectile-find-file
  "ps" 'projectile-switch-project
  "pg" 'projectile-ag
  "pd" 'projectile-dired
  "ph" 'persp-prev
  "pl" 'persp-next
  "pr" 'persp-rename
  "pw" 'persp-switch
  "pk" 'persp-kill

  ;; evil nerd commenter
  "''" 'evilnc-comment-or-uncomment-lines
  "'l" 'evilnc-comment-or-uncomment-to-the-line
  "'c" 'evilnc-copy-and-comment-lines
  "'p" 'evilnc-comment-or-uncomment-paragraphs
  "'r" 'comment-or-uncomment-region

  ;; magit
  "ms" 'magit-status
  "mc" 'magit-checkout
  "mg" 'magit-run-gitk
  "ml" 'magit-log
  "mf" 'magit-fetch
  "mr" 'magit-reflog
  "mb" 'magit-blame-mode)

(evil-leader/set-key-for-mode 'python-mode
  "<" 'python-indent-shift-left
  ">" 'python-indent-shift-right

  "ow" 'pyvenv-workon
  "oe" 'pyvenv-deactivate
  "or" 'run-python
  "od" 'elpy-doc
  "ob" 'elpy-goto-definition
  "os" 'elpy-shell-switch-to-shell

  ;; pony
  "opb" 'pony-browser
  "ops" 'pony-shell
  "opd" 'pony-db-shell
  "opf" 'pony-fabric
  "opm" 'pony-manage
  "opr" 'pony-runserver
  "opt" 'pony-test
  "opgs" 'pony-goto-settings
  "opgt" 'pony-goto-template)


(defun rebuild-python-tags ()
  "Rebuild TAGS for the current projectile project and chosen venv.

  Dependencies: projectile, elpy, etags (shell command)."
  (interactive)
  (call-interactively 'pyvenv-workon)
  (let ((python-files-locations (list (projectile-project-root) pyvenv-virtual-env))
        (output-file-path (concat (projectile-project-root) "TAGS")))
    (message (format "Remove initial TAGS file: %s" output-file-path))
    (shell-command (format "rm %s| true" output-file-path))
    (loop for base-dir in python-files-locations collect
          (let ((cmd (format
                      "find %s -type f -name '*.py' | xargs etags --append --language=python --output=%s"
                      base-dir output-file-path)))
            (message cmd)
            (shell-command cmd)))
    (message "Regenerated %s!" output-file-path)))


(setq projectile-switch-project-action
      (lambda ()
        (neotree-projectile-action)))

(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "o") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))


(load-theme 'monokai t)

;; Share clipboard with OSX
;; http://blog.lathi.net/articles/2007/11/07/sharing-the-mac-clipboard-with-emacs
;; (defun copy-from-osx ()
;;   (shell-command-to-string "pbpaste"))

;; (defun paste-to-osx (text &optional push)
;;   (let ((process-connection-type nil))
;;     (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
;;       (process-send-string proc text)
;;       (process-send-eof proc))))

;; (setq interprogram-cut-function 'paste-to-osx)
;; (setq interprogram-paste-function 'copy-from-osx)

;;; custom.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("03ec0a33794a2f1b74103e5d63b1646ddb2a0cf38b3b447df6d6e6ba68c5b3af" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "fa942713c74b5ad27893e72ed8dccf791c9d39e5e7336e52d76e7125bfa51d4c" "c4e6fe8f5728a5d5fd0e92538f68c3b4e8b218bcfb5e07d8afff8731cc5f3df0" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

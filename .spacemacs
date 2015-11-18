;; Configuration layers
;; --------------------

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-delete-orphan-packages t
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layers
   '(
     ansible
     (auto-completion :variables
                      ;; auto-completion-enable-help-tooltip t
                      auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-enable-sort-by-usage t
                      :disabled-for org)
     clojure
     colors
     deft
     dockerfile
     emacs-lisp
     ;; eyebrowse
     git
     github
     go
     haskell
     markdown
     (org :variables
          org-enable-github-support t)
     ocaml
     osx

     (python :variables
             python-fill-docstring-style 'pep-257-nn
             fill-column 100
             python-test-runner 'pytest)
     (shell :variables
            shell-default-shell 'shell)
     (syntax-checking :variables
                      syntax-checking-enable-by-default nil)
     themes-megapack
     (version-control :variables
                      git-magit-status-fullscreen t)
     yaml)

   dotspacemacs-additional-packages `()
   dotspacemacs-excluded-packages `()))

(defun dotspacemacs/init ()
  "Initialization function.
   This function is called at the very startup of Spacemacs initialization
   before layers configuration."

  (setq-default

   dotspacemacs-editing-style 'vim

   dotspacemacs-themes '(
                         leuven
                         molokai
                         subatomic
                         spacegray
                         material
                         tangotango
                         fogus
                         zenburn
                         twilight)

   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.15)

   dotspacemacs-leader-key ","
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key "SPC"
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   dotspacemacs-command-key ":"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-helm-resize t
   ;; dotspacemacs-helm-no-header t
   dotspacemacs-which-key-delay 1.0
   dotspacemacs-smooth-scrolling t
   dotspacemacs-search-tools '("ag" "grep")

   ))

(defun dotspacemacs/user-init ()
  (setq-default

   ;; Miscellaneous
   require-final-newline t
   x-select-enable-clipboard nil

   ;; Backups
   backup-directory-alist `((".*" . ,temporary-file-directory))
   auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
   backup-by-copying t
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   make-backup-files nil

   ;; Evil
   evil-shift-round nil

   ;; Whitespace mode
   whitespace-style '(face tabs tab-mark newline-mark)
   whitespace-display-mappings
   '((newline-mark 10 [172 10])
     (tab-mark 9 [9655 9]))

   ;; Smartparens
   sp-highlight-pair-overlay nil
   sp-highlight-wrap-overlay nil
   sp-highlight-wrap-tag-overlay nil

   ;; Flycheck
   flycheck-check-syntax-automatically '(save mode-enabled)

   ;; Deft
   deft-extension "rst"
   deft-text-mode 'rst-mode
   deft-directory "~/Documents/Wuala/MdNotes/"
   deft-use-filename-as-title t

   ;; Avy
   avy-all-windows 'all-frames
   ))

(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."

  (defun jao-toggle-selective-display ()
    (interactive)
    (set-selective-display (if selective-display nil 1)))
  (global-set-key [f3] 'jao-toggle-selective-display)

  (setq-default
   powerline-default-separator 'alternate
   guide-key/popup-window-position :right
   )

  (set-fill-column 100)
  (global-set-key (kbd "C-=") 'zoom-frm-in)
  (global-set-key (kbd "C--") 'zoom-frm-out)
  (evil-leader/set-key "of" 'neotree-find)
  (setq clojure-enable-fancify-symbols t)

  ;; Miscellaneous
  (add-hook 'text-mode-hook 'auto-fill-mode)
  (add-hook 'makefile-mode-hook 'whitespace-mode)

  ;; Disable smartparens highlighting
  (with-eval-after-load 'smartparens
    (show-smartparens-global-mode -1))
  )

(setq markdown-open-command "~/bin/open_md")



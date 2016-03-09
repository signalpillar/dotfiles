;; Configuration layers
;; --------------------

(defun dotspacemacs/layers ()
  (setq-default

   dotspacemacs-delete-orphan-packages t
   dotspacemacs-distribution 'spacemacs

   dotspacemacs-configuration-layers

   '(ansible
     (auto-completion :variables
                      ;; auto-completion-enable-help-tooltip t
                      auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-enable-sort-by-usage t
                      :disabled-for org)
     (c-c++ :variables
            c-c++--enable-clang-support t)
     clojure
     colors
     deft
     dockerfile
     emacs-lisp
     ;; eyebrowse
     games
     git
     github
     go
     haskell
     html
     lua
     markdown
     org
     ocaml
     osx

     (python :variables
             python-fill-docstring-style 'pep-257-nn
             python-fill-column 100
             python-test-runner 'pytest)
     ranger
     semantic
     (shell :variables
            shell-default-shell 'shell)
     (syntax-checking :variables
                      syntax-checking-enable-by-default nil)
     themes-megapack
     (theming :variables
              theming-headings-inherit-from-default 'all
              theming-headings-same-size 'all
              theming-headings-bold 'all)
     (version-control :variables
                      version-control-diff-tool 'diff-hl
                      git-magit-status-fullscreen t)
     yaml)

   dotspacemacs-additional-packages `(
                                      virtualenvwrapper
                                      nix-mode
                                      ;; python2
                                      ;; python3
                                      ;; bash
                                      ;; c
                                      ;; c++
                                      ;; cmake
                                      ;; emacs lisp
                                      helm-dash)
   dotspacemacs-excluded-packages `()))

(defun dotspacemacs/init ()
  "Initialization function.
   This function is called at the very startup of Spacemacs initialization
   before layers configuration."

  (setq-default
   dotspacemacs-elpa-https t
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
	 dotspacemacs-check-for-update nil
   dotspacemacs-editing-style 'vim

   dotspacemacs-themes '(
                         ;; light theemes
                         ;; spacemacs-light
                         ;; twilight-bright
                         ;; tao-yang
                         ;; mccarthy
                         ;; ritchie
                         ;; stekene-light
                         ;; organic-green
                         ;; occidental
                         ;; monochrome-bright
                         ;; minimal-light
                         ;; dark themes
                         ;; material
                         ;; molokai
                         colorsarenice-dark
                         zenburn
                         twilight)

   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t

   dotspacemacs-default-font '("Source Code Pro"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 0.75)

   dotspacemacs-leader-key ","
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key "SPC"
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   dotspacemacs-command-key ":"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-use-ido nil
   dotspacemacs-helm-resize t
   dotspacemacs-helm-position 'bottom
   dotspacemacs-which-key-delay 0.4
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
   evil-want-C-i-jump nil

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
  (setq flycheck-display-errors-function 'flycheck-display-error-messages-unless-error-list)

  ;; Miscellaneous
  (add-hook 'text-mode-hook 'auto-fill-mode)
  (add-hook 'makefile-mode-hook 'whitespace-mode)

  ;; Org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (makefile . t)
     (ocaml . t)
     (org . t)
     (perl . t)
     (python . t)
     (shell . t)
     (sql . t)
     (sqlite . t)))

  ;; Disable smartparens highlighting
  (with-eval-after-load 'smartparens
    (show-smartparens-global-mode -1))
  )

(setq markdown-open-command "~/bin/open_md")



(defun sp/path/parent-dir (dir)
  (file-name-directory (directory-file-name dir)))

(defun sp/tox/find-tox-dirs-in-project (project-root-dir)

  (defun -find-toxenv-bin-dirs (root)
    (projectile-files-via-ext-command
     (format "find %s -type d -name 'bin' -path '*/.tox/*/bin' -print0" root)))

  (-map (lambda (file) (sp/path/parent-dir (sp/path/parent-dir file)))
        (-find-toxenv-bin-dirs project-root-dir)))

(defun sp/tox/activate-current-project-tox-env ()
  (interactive)
  (let* (
         (venv-dirs (sp/tox/find-tox-dirs-in-project (projectile-project-root)))
         (venv-dirs-length (length venv-dirs)))
    (progn
      (venv-set-location
       (if (> venv-dirs-length 1)
           (helm-comp-read "Choose tox directory to workon" venv-dirs)
         (if (= venv-dirs-length 0)
             (error "The project doesn't have created tox virtual environments.")
           (car venv-dirs))))
      (venv-workon))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-capture-templates
   (quote
    (("ort/checkitem" "Org Repo Checklist Item" checkitem
      (file+headline
       (ort/todo-file)
       "Checklist"))
     ("ort/todo" "Org Repo Todo" entry
      (file+headline
       (ort/todo-file)
       "Todos")
      "* TODO  %?			%T
 %i
 Link: %l
")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(font-latex-sectioning-0-face ((t (:inherit default :height 1.0 :weight bold))))
 '(font-latex-sectioning-1-face ((t (:inherit default :height 1.0 :weight bold))))
 '(font-latex-sectioning-2-face ((t (:inherit default :height 1.0 :weight bold))))
 '(font-latex-sectioning-3-face ((t (:inherit default :height 1.0 :weight bold))))
 '(font-latex-sectioning-4-face ((t (:inherit default :height 1.0 :weight bold))))
 '(font-latex-sectioning-5-face ((t (:inherit default :height 1.0 :weight bold))))
 '(font-latex-slide-title-face ((t (:inherit default :height 1.0 :weight bold))))
 '(info-title-1 ((t (:inherit default :height 1.0 :weight bold))))
 '(info-title-2 ((t (:inherit default :height 1.0 :weight bold))))
 '(info-title-3 ((t (:inherit default :height 1.0 :weight bold))))
 '(info-title-4 ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-1 ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-2 ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-3 ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-4 ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-5 ((t (:inherit default :height 1.0 :weight bold))))
 '(markdown-header-face-6 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-document-title ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-1 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-2 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-3 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-4 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-5 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-6 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-7 ((t (:inherit default :height 1.0 :weight bold))))
 '(org-level-8 ((t (:inherit default :height 1.0 :weight bold)))))

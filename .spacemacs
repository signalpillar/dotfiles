;; Configuration layers
;; --------------------

(setq-default
 dotspacemacs-configuration-layers
 '(
   ansible
   (python :variables
           python-fill-docstring-style 'pep-257-nn
           fill-column 100
           python-test-runner 'pytest
           )
   clojure
   git
   go
   (org :variables
        org-enable-github-support t)
   (version-control :variables
                    git-magit-status-fullscreen t)
   dockerfile
   markdown
   deft
   haskell
   emacs-lisp
   themes-megapack
   (auto-completion :variables
                    auto-completion-enable-help-tooltip t
                    auto-completion-enable-sort-by-usage t
                    )
   syntax-checking
   ;; experimental
   puppet
   osx
   slime
   colors
   prelude)
    dotspacemacs-smooth-scrolling t
    dotspacemacs-leader-key ","
    dotspacemacs-major-mode-leader-key "SPC"
    ;; light theme - leuven
    ;; dotspacemacs-themes '(material)
    dotspacemacs-themes '(
                          leuven
                          subatomic
                          spacegray
                          material
                          tangotango
                          fogus
                          zenburn
                          twilight)
 )

(setq dotspacemacs-additional-packages '(groovy-mode)
      x-select-enable-clipboard nil)

(defun dotspacemacs/config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  (set-fill-column 100)
  (global-set-key (kbd "C-=") 'zoom-frm-in)
  (global-set-key (kbd "C--") 'zoom-frm-out)
  (evil-leader/set-key "of" 'neotree-find)
  (setq clojure-enable-fancify-symbols t)
  (setq-default
   ;; Powerline with arrows as separator
   powerline-default-separator 'arrow
   deft-extension "rst"
   deft-text-mode 'rst-mode
   deft-directory "~/Documents/Wuala/MdNotes/"
   deft-use-filename-as-title t
   guide-key/popup-window-position :right
   )
  )

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  (setq-default

   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   )

  ;; User initialization goes here
  (add-hook 'python-mode-hook 'eldoc-mode)

)

(defun jao-toggle-selective-display ()
  (interactive)
  (set-selective-display (if selective-display nil 1)))

(global-set-key [f3] 'jao-toggle-selective-display)

(setq markdown-open-command "~/bin/open_md")

(setq-default dotspacemacs-default-font '("Source Code Pro"
                                          :size 14
                                          :weight normal
                                          :width normal
                                          :powerline-scale 1.3))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-safe-themes
   (quote
    ("eafda598b275a9d68cc1fbe1689925f503cab719ee16be23b10a9f2cc5872069" default)))
 '(paradox-github-token t)
 '(ring-bell-function (quote ignore) t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

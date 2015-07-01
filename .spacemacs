;; Configuration layers
;; --------------------
(defun dotspacemacs/config ()
  "This is were you can ultimately override default Spacemacs configuration.
This function is called at the very end of Spacemacs initialization."
  (setq powerline-default-separator 'alternate)
  )

;; Configuration layers
;; --------------------

(setq-default
 dotspacemacs-configuration-layers '(
                                     (python :variables
                                             python-fill-docstring-style 'pep-257-nn
                                             fill-column 100)
                                     clojure
                                     (git :variables
                                          git-magit-status-fullscreen t)
                                     groovy
                                     dockerfile
                                     markdown
                                     deft
                                     haskell
                                     emacs-lisp
                                     erlang-elixir
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
    dotspacemacs-themes '(leuven)
 )


(defun dotspacemacs/config ()
  (set-fill-column 100)
  (setq-default
    deft-extension "rst"
    deft-text-mode 'rst-mode
    deft-directory "~/Documents/Wuala/MdNotes/"
    deft-use-filename-as-title t
    guide-key/popup-window-position :right
   )
)

(defun jao-toggle-selective-display ()
  (interactive)
  (set-selective-display (if selective-display nil 1)))

(global-set-key [f3] 'jao-toggle-selective-display)

(setq markdown-open-command "~/bin/open_md")

(setq-default dotspacemacs-default-font '("Source Code Pro"
                                          :size 12
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
 '(paradox-github-token t)
 '(ring-bell-function (quote ignore) t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

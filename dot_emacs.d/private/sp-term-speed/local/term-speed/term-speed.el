;;; term-speed.el --- Speed up Emacs terminal buffers -*- lexical-binding: t -*-

;; Copyright (c) 2025 James Cherti
;; Copyright (c) 2026 Volodymyr Vitvitskyi
;;
;; Author: James Cherti
;; Maintainer: Volodymyr Vitvitskyi
;; License: MIT
;; URL: https://www.jamescherti.com/emacs-terminal-performance-vterm-eat-ansi-term-ghostel/
;;
;; Adapted for a Spacemacs private layer. Logic follows James Cherti's
;; MIT snippet. Spacemacs-specific minor modes are extra.

;;; Commentary:

;; Apply display, I/O, and minor-mode cuts in vterm, eat, ghostel, term,
;; and ansi-term buffers. Goal: less freeze on large stdout.

;;; Code:

(defgroup term-speed nil
  "Speed up Emacs terminal emulator buffers."
  :group 'convenience
  :prefix "term-speed-")

(defcustom term-speed-vterm-timer-delay 0.01
  "vterm output batch delay in seconds."
  :type 'number
  :group 'term-speed)

(defcustom term-speed-ghostel-timer-delay 0.01
  "ghostel output batch delay in seconds."
  :type 'number
  :group 'term-speed)

(defcustom term-speed-eat-minimum-latency 0.007
  "eat minimum output latency in seconds."
  :type 'number
  :group 'term-speed)

(defcustom term-speed-eat-maximum-latency 0.05
  "eat maximum output latency in seconds."
  :type 'number
  :group 'term-speed)

(defcustom term-speed-vterm-max-scrollback 500
  "vterm scrollback in lines."
  :type 'integer
  :group 'term-speed)

(defcustom term-speed-ghostel-max-scrollback (* 1024 1024)
  "ghostel scrollback in bytes."
  :type 'integer
  :group 'term-speed)

(defcustom term-speed-eat-term-scrollback-size (* 64 1024)
  "eat scrollback in characters."
  :type 'integer
  :group 'term-speed)

(defcustom term-speed-eat-enable-shell-prompt-annotation nil
  "When non-nil, keep eat shell prompt annotations."
  :type 'boolean
  :group 'term-speed)

(defcustom term-speed-use-system-libvterm t
  "When non-nil, compile vterm against the system libvterm."
  :type 'boolean
  :group 'term-speed)

(defcustom term-speed-hide-mode-line t
  "When non-nil, hide the mode line in terminal buffers except ghostel."
  :type 'boolean
  :group 'term-speed)

(defcustom term-speed-disable-scroll-bars nil
  "When non-nil, hide scroll bars in terminal buffers."
  :type 'boolean
  :group 'term-speed)

(defcustom term-speed-disabled-modes
  '(electric-pair-local-mode
    electric-indent-local-mode
    display-line-numbers-mode
    display-fill-column-indicator-mode
    hl-line-mode
    show-paren-local-mode
    flymake-mode
    flycheck-mode
    evil-surround-mode
    evil-snipe-local-mode
    yas-minor-mode
    company-mode
    corfu-mode
    vi-tilde-fringe-mode
    highlight-parentheses-mode
    smartparens-mode
    rainbow-delimiters-mode
    highlight-numbers-mode
    indent-guide-mode
    golden-ratio-mode
    centered-cursor-mode
    volatile-highlights-mode
    spacemacs-whitespace-cleanup-mode
    evil-goggles-mode
    hl-todo-mode
    column-enforce-mode)
  "Minor modes to turn off in terminal buffers."
  :type '(repeat symbol)
  :group 'term-speed)

(defvar term-speed--hooks
  '(term-mode-hook vterm-mode-hook eat-mode-hook ghostel-mode-hook)
  "Major-mode hooks that receive `term-speed-setup-buffer'.")

;; Package variables. Set before those packages load.
(defvar vterm-timer-delay)
(defvar vterm-max-scrollback)
(defvar vterm-module-cmake-args)
(defvar ghostel-timer-delay)
(defvar ghostel-max-scrollback)
(defvar eat-minimum-latency)
(defvar eat-maximum-latency)
(defvar eat-term-scrollback-size)
(defvar eat-enable-shell-prompt-annotation)

(defun term-speed-apply-package-defaults ()
  "Set package variables used by vterm, eat, and ghostel.
Set these before those packages compile or load."
  (setq vterm-timer-delay term-speed-vterm-timer-delay)
  (setq ghostel-timer-delay term-speed-ghostel-timer-delay)
  (setq eat-minimum-latency term-speed-eat-minimum-latency)
  (setq eat-maximum-latency term-speed-eat-maximum-latency)
  (setq vterm-max-scrollback term-speed-vterm-max-scrollback)
  (setq ghostel-max-scrollback term-speed-ghostel-max-scrollback)
  (setq eat-term-scrollback-size term-speed-eat-term-scrollback-size)
  (setq eat-enable-shell-prompt-annotation
        term-speed-eat-enable-shell-prompt-annotation)
  (setq vterm-module-cmake-args
        (concat "-DCMAKE_BUILD_TYPE=Release "
                "-DCMAKE_C_FLAGS='-O3 -march=native -mtune=native' "
                "-DCMAKE_SHARED_LINKER_FLAGS='-Wl,-O2 -Wl,--as-needed'"
                (if term-speed-use-system-libvterm
                    " -DUSE_SYSTEM_LIBVTERM=yes"
                  ""))))

(defun term-speed-setup-buffer ()
  "Reduce display and minor-mode cost in the current terminal buffer."
  (let ((ghostel-buffer (derived-mode-p 'ghostel-mode)))
    (setq-local font-lock-defaults '(nil t))
    (setq-local scroll-conservatively most-positive-fixnum)
    (setq-local hscroll-margin 0)
    (setq-local scroll-margin 0)
    (setq-local auto-hscroll-mode nil)
    (when term-speed-disable-scroll-bars
      (setq-local vertical-scroll-bar nil)
      (setq-local horizontal-scroll-bar nil))
    (setq-local truncate-lines t)
    (setq-local nobreak-char-display nil)
    (setq-local bidi-paragraph-direction 'left-to-right)
    (setq-local bidi-inhibit-bpa t)
    (unless ghostel-buffer
      (setq-local line-spacing 0)
      (when term-speed-hide-mode-line
        (setq-local mode-line-format nil)))
    (setq-local process-adaptive-read-buffering nil)
    (let ((output-max (* 1024 1024)))
      (when (< read-process-output-max output-max)
        (setq-local read-process-output-max output-max)))
    (buffer-disable-undo)
    (remove-hook 'pre-command-hook 'evil--jump-hook t)
    (remove-hook 'post-command-hook 'evil--jump-handle-buffer-crossing t)
    (let ((inhibit-redisplay t)
          (inhibit-message t)
          (modes (copy-sequence term-speed-disabled-modes)))
      (unless ghostel-buffer
        (push 'eldoc-mode modes)
        (push 'auto-composition-mode modes))
      (dolist (mode modes)
        (when (fboundp mode)
          (ignore-errors
            (funcall mode -1)))))))

;;;###autoload
(defun term-speed-enable ()
  "Apply package defaults and hook terminal major modes."
  (term-speed-apply-package-defaults)
  (dolist (hook term-speed--hooks)
    (add-hook hook #'term-speed-setup-buffer t)))

;;;###autoload
(defun term-speed-disable ()
  "Remove terminal major-mode hooks.
Open buffers keep the last local settings until you recreate them."
  (dolist (hook term-speed--hooks)
    (remove-hook hook #'term-speed-setup-buffer t)))

(provide 'term-speed)

;;; term-speed.el ends here

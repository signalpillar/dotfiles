;;; packages.el --- sp-codetutor layer packages file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2026 Volodymyr Vitvitskyi
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; CodeTutor package and Spacemacs keys. This layer is off unless you add
;; `sp-codetutor' to `dotspacemacs-configuration-layers'.

;;; Code:

(defconst sp-codetutor-packages
  '((codetutor :location (recipe :fetcher github :repo "jaketothepast/codetutor")))
  "Lisp packages required by the sp-codetutor layer.")

(defun sp-codetutor/init-codetutor ()
  (use-package codetutor
    :commands (codetutor-mode codetutor-open codetutor-what-next codetutor-ask
                              codetutor-follow-up codetutor-refresh-architecture-memory
                              codetutor-new-spec codetutor-open-spec codetutor-scratch
                              codetutor-inline-tips codetutor-clear-inline-tips)
    :init
    (setq codetutor-backend 'auto
          codetutor-review-on-save nil)
    (spacemacs/declare-prefix "ot" "codetutor")
    (spacemacs/set-leader-keys
      "ot o" #'codetutor-open
      "ot n" #'codetutor-what-next
      "ot a" #'codetutor-ask
      "ot f" #'codetutor-follow-up
      "ot m" #'codetutor-refresh-architecture-memory
      "ot s" #'codetutor-new-spec
      "ot S" #'codetutor-open-spec
      "ot t" #'codetutor-scratch
      "ot i" #'codetutor-inline-tips)
    :config
    (codetutor-mode 1)
    ;; Upstream bug: the pi.dev backend command never sets :stdin (only the
    ;; codex backend does), so `codetutor--request' never calls
    ;; `process-send-eof' and the `pi' subprocess hangs forever waiting for
    ;; stdin to close. Force EOF by giving that backend an empty :stdin;
    ;; the actual prompt already travels via the `@promptfile' argument.
    (advice-add 'codetutor--backend-command :filter-return
                (lambda (backend)
                  (if (equal (plist-get backend :name) "pi.dev")
                      (plist-put backend :stdin "")
                    backend)))))

;;; packages.el ends here

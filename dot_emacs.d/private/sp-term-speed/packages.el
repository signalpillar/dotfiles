;;; packages.el --- sp-term-speed layer packages file for Spacemacs.
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

;; Load the local `term-speed' package. That package applies James Cherti's
;; terminal performance settings to vterm, eat, ghostel, term, and ansi-term.

;;; Code:

(defconst sp-term-speed-packages
  '((term-speed :location local))
  "Lisp packages required by the sp-term-speed layer.")

(defun sp-term-speed/init-term-speed ()
  (use-package term-speed
    :demand t
    :config
    (term-speed-enable)))

;;; packages.el ends here

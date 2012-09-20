(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

                                        ; TOOLBAR
(menu-bar-mode 1)
                                        ; ELSCREEN

(add-to-list 'load-path "~/.emacs.d/escreen/")
(load "escreen")
(escreen-install)
(setq escreen-prefix-char "\C-a") ;; http://www.macs.hw.ac.uk/~hwloidl/cool-el.html
(global-set-key escreen-prefix-char 'escreen-prefix)
;; add C-\ l to list screens with emphase for current one

                                        ; EDITOR CONFIGURATION
; disable wordwrap by default
(setq-default truncate-lines nil)
(setq lisp-indent-offset 2)
;; Use only spaces (no tabs at all).
(setq-default indent-tabs-mode nil)
;; Always show column numbers.
(setq-default column-number-mode t)
;; Display full pathname for files.
(add-hook 'find-file-hooks
          '(lambda ()
             (setq mode-line-buffer-identification 'buffer-file-truename)))

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; enable evil-mode
(require 'evil)
(evil-mode 1)

(defun turn-on-paredit () (paredit-mode 1))

;; -----------------------------------------------------
;; draw fill-column line
; (add-to-list 'load-path "/path/to/highlight-80+")
; (require 'highlight-80+)
;; -----------------------------------------------------

;; clojure-mode
(add-hook 'clojure-mode-hook
          (lambda ()
            (turn-on-paredit)
            (turn-on-eldoc-mode)
            (show-paren-mode t)
            (set-up-slime-ac)
            (fci-mode)))

;; clojure-script mode
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))


;; render line numbers
(global-linum-mode)

;; multi-scratch
(add-to-list 'load-path "~/.emacs.d/misc")
(require 'multi-scratch)
(global-set-key (kbd "C-<f11>") 'multi-scratch-new)

(load-theme 'tsdh-dark)

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)

(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))


;; comment-or-uncomment
;; (global-set-key (kbd "C-S /") 'comment-or-uncomment-region)


(set-default 'indicate-empty-lines t)

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-buffer))

(global-set-key (kbd "C-c n") 'cleanup-buffer)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-safe-themes (quote ("71efabb175ea1cf5c9768f10dad62bb2606f41d110152f4ace675325d28df8bd" default)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 98 :width normal)))))


                                        ; eproject
(add-to-list 'load-path "~/.emacs.d/eproject")
(require 'eproject)
(require 'eproject-extras)
                                        ; projects type configuration

(define-project-type clojure (generic)
  (look-for ".scalaproject")
  :relevant-files ("\.java$" "\.scala$"))

(define-project-type scala (generic)
  (look-for ".scalaproject")
  :relevant-files ("\.java$" "\.scala$"))

(define-project-type pydevproject (generic)
  (look-for ".pydevproject")
  :relevant-files ("\.py$" "\.xml$" "\.md$" "\.properties$"))


                                        ; jython stuff

(defun compose-cmd-line (jython-home python-path python-ext-path script)
  "Compose command line for jython script execution")

(defun file-read-content (file)
  "Read the contents of a file and return as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun file-write-content (file content)
   (with-temp-buffer
     (insert content)
     (when (file-writable-p file)
       (write-region (point-min)
                     (point-max)
                     file))))

(defun parse-pydevproject (content))

(defun run-jython-tests ()
  "Execute jython tests in currect buffer"
  (interactive)
  ; take project root folder and read .pydevproject file 
  (let ((project-file (concat (eproject-root) "/.pydevproject"))
         (run-file (concat (eproject-root) "/.pydevrun"))
         (one-hour-in-sec (* 60 60)))
    ; check for the stored file .pydevrun
    (when (or (not (file-exists-p run-file))
            (progn (let
                     (attrs (file-attributes run-file))
                     (modification-time (float-time (elt attrs 5)))
                     (current-time (float-time))
                     ; compute difference 1 HOUR
                     (> (- current-time modification-time) one-hour-in-sec))))
      
      ; get content of .pydevproject file
      (setq pydevproject (parse-pydevproject (file-read-content project-file)))
      (file-write-content project-run-file
        (compose-cmd-line
          jython-home
          ; pythonpath and python ext path (libs)
          (elt pydevproject 0) (elt pydevproject 1))))

    ; execute & redirect output to temporary buffer
    (show-in-temporary-buffer
      (execute-shell-cmd
        (string (get-file-content run-file) (current-buffer))))))
    
    
    ;; (with-temp-buffer
    ;;   (insert-file-contents-literally project-file)
    ;;   (setq content (read (current-buffer))))


                                        ; make anythin.el to work with eproject
(require 'cl)
(defun anything-eproject-get-files ()
  (let ((matcher (format "\\(?:%s\\)"
                         (reduce (lambda (a b) (concat a "\\|" b))
                                 (mapcar (lambda (f) (format "\\(?:%s\\)" f))
                                         (eproject-get-project-metadatum
                                          (eproject-type) :relevant-files))))))
    (eproject--search-directory-tree (eproject-root) matcher "$^")))

(defvar anything-eproject-source
  '((name . "Eproject")
    (init . (lambda ()
              (setq anything-eproject-last-buffer (current-buffer))))
    (type . file)
    (candidates . (lambda ()
                    (with-current-buffer anything-eproject-last-buffer
                      (anything-eproject-get-files))))))

(defun any-eproject ()
  "helps anything to use eproject to find a file"
  (interactive)
  (let ((anything-sources '(anything-eproject-source)) anything-samewindow)
    (anything nil nil nil nil nil "eproject")))


(global-set-key (kbd "M-p") 'any-eproject)


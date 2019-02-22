;;; init.el --- emacs initialization -*- lexical-binding: t; -*-
;;; Commentary:
;; This file was generated. do not edit. changes may be overwritten
;;; Code:

(setq-default gc-cons-threshold 104857600)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
       'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

(with-eval-after-load "org"
  (defun mkyle/rebuild-init-file ()
    "Rebuild init file if it's changed since the last time it was built."
    (interactive)
    (let ((source-file    (expand-file-name "readme.org" user-emacs-directory))
          (generated-file (expand-file-name "init.el" user-emacs-directory)))
      (when (org-file-newer-than-p source-file
                                   (file-attribute-modification-time
                                    (file-attributes generated-file)))
        (org-babel-tangle-file source-file generated-file "emacs-lisp")
        (byte-compile-file generated-file)
        t))))

(use-package no-littering)

(with-eval-after-load "no-littering"
  (setq-default custom-file (expand-file-name "custom.el" no-littering-etc-directory))
  (when (file-exists-p custom-file)
    (load custom-file t)))

(use-package diminish :defer t)

(use-package anzu
  :diminish anzu-mode
  :config (global-anzu-mode))

(size-indication-mode t)

(line-number-mode t)
(column-number-mode t)

(set-frame-font "xos4 Terminus 12")

(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

(setq inhibit-startup-screen  t
      initial-scratch-message nil)

(setq-default large-file-warning-threshold 104857600)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode +1))

(defadvice server-visit-files (before parse-numbers-in-lines (files proc &optional nowait) activate)
  "Open file with emacsclient with cursors positioned on requested line.
Most of console-based utilities prints filename in format
'filename:linenumber'.  So you may wish to open filename in that format.
Just call:

  emacsclient filename:linenumber

and file 'filename' will be opened and cursor set on line 'linenumber'"
  (ad-set-arg 0
              (mapcar (lambda (fn)
                        (let ((name (car fn)))
                          (if (string-match
                               "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$"
                               name)
                              (cons
                                (match-string 1 name)
                                (cons (string-to-number (match-string 2 name))
                                      (string-to-number
                                       (or (match-string 3 name)
                                           ""))))
                            fn)))
                      files)))

(setq frame-title-format
      '("" invocation-name " - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style   'forward
        uniquify-separator           "/"
        ;; rename after killing uniquified
        uniquify-after-kill-buffer-p t
        ;; ignore system buffers
        uniquify-ignore-buffers-re   "^\\*"))

(global-auto-revert-mode t)

(when (require 'savehist nil t)
  (setq savehist-additional-variables
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60)
        (savehist-mode +1))

;; smex, remember recently and most frequently used commands
(with-eval-after-load "ido"
  (use-package smex
    :config (progn
              (smex-initialize)
              (global-set-key (kbd "M-x") 'smex)
              (global-set-key (kbd "M-X") 'smex-major-mode-commands))))

(defun mkyle/split-window (&optional window)
  "Split window more senibly.  WINDOW."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (with-selected-window window
               (split-window-right)))
        (and (window-splittable-p window)
             ;; Split window vertically.
             (with-selected-window window
               (split-window-below)))
        (and (eq window (frame-root-window (window-frame window)))
             (not (window-minibuffer-p window))
             ;; If WINDOW is the only window on its frame and is not the
             ;; minibuffer window, try to split it horizontally disregarding
             ;; the value of `split-width-threshold'.
             (let ((split-width-threshold 0))
               (when (window-splittable-p window t)
                 (with-selected-window window
                   (split-window-right))))))))

(setq-default split-window-preferred-function #'mkyle/split-window)

(use-package beacon
  :diminish beacon-mode
  :config (beacon-mode +1))

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(with-eval-after-load "ibuffer"
  (setq ibuffer-formats
  '((mark modified read-only " "
     (name 40 40 :left :elide) " " ;; 40 40 is the column width
     (size 9 -1 :right) " "
     (mode 8 8 :left :elide) " "
     filename-and-process)
    (mark " " (name 16 -1) " " filename))))

(with-eval-after-load "ibuffer"
  (setq ibuffer-show-empty-filter-groups nil)

  (with-eval-after-load "straight"
    (use-package ibuffer-dynamic-groups
      :straight (ibuffer-dynamic-groups :type git
      :host github
      :repo "mitch-kyle/ibuffer-dynamic-groups")
      :config (progn
                (ibuffer-dynamic-groups-add
                 (lambda (groups)
                   (append groups
                           '(("System" (name . "^\\*.*\\*$")))))
                 '((name . system-group)))
                (ibuffer-dynamic-groups t)))))

(use-package ido
  :config
  (progn
    (setq ido-enable-prefix                      nil
          ido-enable-flex-matching               t
          ido-create-new-buffer                  'always
          ido-use-filename-at-point              'guess
          ido-max-prospects                      10
          ido-default-file-method                'selected-window
          ido-auto-merge-work-directories-length -1)
    (ido-mode +1)

    (use-package ido-completing-read+
      :config (ido-ubiquitous-mode +1))

    ;; smarter fuzzy matching for ido
    (use-package flx-ido
      :config (progn (flx-ido-mode +1)
                     ;; disable ido faces to see flx highlights
                     (setq ido-use-faces nil)))))

(if window-system
    (progn
      (global-set-key [s-left]  'windmove-left)
      (global-set-key [s-right] 'windmove-right)
      (global-set-key [s-up]    'windmove-up)
      (global-set-key [s-down]  'windmove-down))
  (progn
    (global-set-key (kbd "s-x <left>")  'windmove-left)
    (global-set-key (kbd "s-x <right>") 'windmove-right)
    (global-set-key (kbd "s-x <up>")    'windmove-up)
    (global-set-key (kbd "s-x <down>")  'windmove-down)))

(use-package projectile
  :defer t
  :config (progn
            (projectile-mode t)
            (global-set-key (kbd "C-c p") projectile-command-map)))

(defadvice vc-mode (after enable-projectile (&optional arg) activate)
  (projectile-mode arg))

(with-eval-after-load "projectile"
  (with-eval-after-load "ibuffer-dynamic-groups"
    (use-package ibuffer-projectile
      :config
      (progn
        (setq ibuffer-projectile-prefix "- ")
        (ibuffer-dynamic-groups-add
         (lambda (groups)
           (append (ibuffer-projectile-generate-filter-groups)
                   groups))
         '((name . projectile-groups)
           (depth . -50)))))))

(with-eval-after-load "tramp"
  (setq tramp-default-method "ssh"))

(cua-mode t)

(use-package rainbow-delimiters
  :hook ((prog-mode) . rainbow-delimiters-mode))

(global-linum-mode t)

(setq scroll-margin                   0
      scroll-conservatively           100000
      scroll-preserve-screen-position 1)

(setq-default indent-tabs-mode  nil
              tab-width         4
              tab-always-indent 'complete)

(add-hook 'before-save-hook #'whitespace-cleanup)

(use-package company
  :diminish company-mode
  :config
  (progn
    (setq company-idle-delay 0.5
          company-show-numbers t
          company-tooltip-limit 10
          company-minimum-prefix-length 2
          company-tooltip-align-annotations t
          ;; invert the navigation direction if the the completion popup-isearch-match
          ;; is displayed on top (happens near the bottom of windows)
          company-tooltip-flip-when-above t)
    (global-company-mode 1)))

(setq-default search-highlight t
              query-replace-highlight t)

(show-paren-mode t)
(set-face-foreground 'show-paren-match "DimGrey")

(use-package rainbow-mode
  :defer t
  :diminish rainbow-mode)

(use-package monokai-theme
  :if window-system
  :config (progn (load-theme 'monokai t)
                 (set-face-foreground 'show-paren-match "DimGrey")))

(use-package spaceline
  :if window-system
  :config (progn (setq powerline-default-separator 'contour)
                 (spaceline-emacs-theme)))

(when window-system
  (setq-default window-divider-default-right-width 1)
  (window-divider-mode t))

(when window-system
  (defun mkyle/toggle-transparency ()
    "Toggle off window transparency"
    (interactive)
    (set-frame-parameter nil 'alpha
      (if (eql (car (frame-parameter nil 'alpha))
               100)
          '(95 . 95)
        '(100 . 100))))

  (set-frame-parameter nil 'alpha '(95 . 95)))

(use-package git)
(when (require 'git nil t)
  (defun org-git-version ()
    "The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
    (let ((git-repo (expand-file-name "straight/repos/org/"
                                      user-emacs-directory)))
      (string-trim
       (git-run "describe"
                "--match=release\*"
                "--abbrev=6"
                "HEAD"))))

  (defun org-release ()
    "The release version of org-mode.
Inserted by installing org-mode or when a release is made."
    (let ((git-repo (expand-file-name "straight/repos/org/"
                                      user-emacs-directory)))
      (string-trim
       (string-remove-prefix
        "release_"
        (git-run "describe"
                 "--match=release\*"
                 "--abbrev=0"
                 "HEAD")))))

  (provide 'org-version))

(use-package org
  :mode ("\\.org\\'" . org-mode))

(with-eval-after-load "org"
  (use-package toc-org
    :hook ((org-mode) . toc-org-mode)))

(use-package magit
  :defer t
  :config (global-set-key (kbd "C-c m") 'magit-status))

(use-package git-modes
  :defer t)

(with-eval-after-load "ediff"
  ;; TODO this fails when ediff complains about a buffer already open for a file being merged
  (defun mkyle/ediff-janitor ()
    "Delete buffers and restore window on ediff exit."
    (let* ((ctl-buf ediff-control-buffer)
           (ctl-win (ediff-get-visible-buffer-window ctl-buf))
           (ctl-frm ediff-control-frame)
           (main-frame (cond ((window-live-p ediff-window-A)
                              (window-frame ediff-window-A))
                             ((window-live-p ediff-window-B)
                              (window-frame ediff-window-B)))))
      (ediff-kill-buffer-carefully ediff-diff-buffer)
      (ediff-kill-buffer-carefully ediff-custom-diff-buffer)
      (ediff-kill-buffer-carefully ediff-fine-diff-buffer)
      (ediff-kill-buffer-carefully ediff-tmp-buffer)
      (ediff-kill-buffer-carefully ediff-error-buffer)
      (ediff-kill-buffer-carefully ediff-msg-buffer)
      (ediff-kill-buffer-carefully ediff-debug-buffer)
      (when (boundp 'ediff-patch-diagnostics)
        (ediff-kill-buffer-carefully ediff-patch-diagnostics))
      (cond ((and (ediff-window-display-p)
                  (frame-live-p ctl-frm))
             (delete-frame ctl-frm))
            ((window-live-p ctl-win)
             (delete-window ctl-win)))
      (unless (ediff-multiframe-setup-p)
        (ediff-kill-bottom-toolbar))
      (ediff-kill-buffer-carefully ctl-buf)
      (when (frame-live-p main-frame)
        (select-frame main-frame)))
    (ediff-janitor nil nil))

  (add-hook 'ediff-cleanup-hook 'mkyle/ediff-janitor))

;; Technically a window management suite but it'll do to return the
;; window to normal after an ediff session
(use-package winner
  :config (progn (winner-mode +1)
                 (with-eval-after-load "ediff"
                   (add-hook 'ediff-cleanup-hook 'winner-undo))))

(with-eval-after-load "erc"
  (setq erc-query-display 'buffer
        erc-interpret-mirc-color t
        erc-server-coding-system '(utf-8 . utf-8)
        erc-save-buffer-on-part t
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477"))

  (erc-truncate-mode +1)
  (erc-track-mode t)

  (when (require 'erc-log nil t)
    (unless (file-exists-p erc-log-channels-directory)
      (mkdir erc-log-channels-directory t)))

  (when (require 'erc-spelling nil t)
    (erc-spelling-mode 1)))

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(with-eval-after-load "eldoc"
  (diminish 'eldoc-mode))

(use-package auto-compile
  :config
  (progn
    (setq auto-compile-display-buffer    nil
          auto-compile-mode-line-counter t)
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode)))

(defun mkyle/elisp-recompile-elc-on-save ()
  "Recompile your elc when saving an elisp file."
  (add-hook 'after-save-hook
    (lambda ()
      (when (and (string-prefix-p user-emacs-directory
                                  (file-truename buffer-file-name))
                 (file-exists-p (byte-compile-dest-file buffer-file-name)))
        (emacs-lisp-byte-compile)))
        nil
        t))

(add-hook 'emacs-lisp-mode-hook 'mkyle/elisp-recompile-elc-on-save)

(mapc (lambda (filename-regex)
        (add-to-list 'auto-mode-alist `(,filename-regex . conf-mode)))
      (list "\\.conf\\'"
            "\\.desktop\\'"
            "\\.service\\'"))

(use-package clojure-mode
  :mode ("\\.edn\\'" "\\.clj\\'")
  :config (add-hook 'clojure-mode-hook 'subword-mode))

(use-package cider
  :defer t
  :config (progn
            (setq nrepl-log-messages t)
            (add-hook 'cider-mode-hook #'eldoc-mode)
            (add-hook 'cider-repl-mode-hook #'subword-mode)
            (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
            (add-hook 'cider-repl-mode-hook #'company-mode)
            (add-hook 'cider-mode-hook #'company-mode)

            (with-eval-after-load "ibuffer-dynamic-groups"
              (ibuffer-dynamic-groups-add
               (lambda (groups)
                 (append '(("Cider" (or (name . "^\\*nrepl-.*\\*$")
                                        (name . "^\\*cider-.*\\*$"))))
                         groups))
               '((name . cider-group)
                 (depth . -1))))))

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package js2-mode
  :mode ("\\.js\\'" "\\.pac\\'")
  :interpreter "node")

(use-package json-mode
  :mode "\\.json\\'")

(use-package scheme
  :mode ("\\.scm\\'" . scheme-mode))

(use-package geiser
  :defer t
  :config (setq geiser-mode-start-repl-p t))

(use-package groovy-mode
  :mode ("\\.groovy\\'" "JenkinsFile\\'"))

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package yaml-mode
  :mode ("\\.yaml\\'" "\\.yml\\'"))

(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'"))

(use-package lua-mode
  :mode "\\.lua\\'")

(let ((zsh-files '("zlogin" "zlogin" "zlogout" "zpreztorc"
                   "zprofile" "zshenv" "zshrc" ".zsh")))
  (add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))
  (mapc (lambda (file)
          (add-to-list 'auto-mode-alist
                       `(,(format "\\%s\\'" file) . sh-mode)))
        zsh-files)
  (add-hook 'sh-mode-hook
            (lambda ()
              (when
               (and buffer-file-name
                    (member (file-name-nondirectory buffer-file-name)
                            zsh-files))
               (sh-set-shell "zsh")))))

(use-package terraform-mode
  :mode ("\\.tf\\'" "\\.tvars\\'"))

(use-package exwm
  ;; TODO find test for emacs on root window to put here
  :if window-system
  :defer t
  :config
  (org-babel-load-file
   (expand-file-name "window-manager.org"
                     user-emacs-directory)
   t))

;; init.el ends here

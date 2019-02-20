;;; init.el --- emacs initialization -*- lexical-binding: t; -*-
;;; Commentary:
;; This file was generated. do not edit. changes may be overwritten
;;; Code:

(setq-default gc-cons-threshold 104857600)

(server-start)

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

(when (require 'org nil t)
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

(when (and (functionp 'mkyle/rebuild-init-file)
           (mkyle/rebuild-init-file))
  (load (expand-file-name "init.elc" user-emacs-directory))
  (error (concat "Loaded from dirty config. "
                 "This isn't likely to cause problems and should be "
                 "fix when emacs is restarted. "
                 "Thought you aught to know.")))

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

(global-set-key [s-left]  'windmove-left)
(global-set-key [s-right] 'windmove-right)
(global-set-key [s-up]    'windmove-up)
(global-set-key [s-down]  'windmove-down)

(use-package projectile
  :defer t
  :config (progn
            (projectile-mode t)
            (global-set-key (kbd "C-c p") projectile-command-map)))

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

(defmacro wm/define-launcher (fun-name command-and-args)
      "Define an interactive function that invokes the shell command given"
      `(defun ,fun-name ()
   (interactive)
   (start-process-shell-command "" nil ,command-and-args)))

(use-package exwm
  ;; TODO find test for emacs on root window to put here
  :if window-system
  :defer t
  :config
  (progn
    (require 'exwm-config)
    (require 'exwm-randr)
    (require 'exwm-systemtray)

    (with-eval-after-load "ibuffer-dynamic-groups"
  (ibuffer-dynamic-groups-add (lambda (groups)
              (append '(("X Windows" (mode . exwm-mode)))
                groups))
            '((name . exwm-group)
              (depth . -10))))

    ;; Dialog boxes do not work with exwm
    (setq use-dialog-box nil)
    (setq display-time-day-and-date t)
    (setq exwm-workspace-show-all-buffers t)
    (setq exwm-layout-show-all-buffers t)

    (defvar wm/tmux-session-name "0")

    (defun wm/no-op ()
      "Used to suppress warnings for shortcut keys that already work in hardware"
      (interactive))

    ;; Launchers

    (defun wm/run-sh-async (command)
      "Interactive prompt to run a shell command in a child process which may or may not spawn an x window"
      (interactive (list (read-shell-command "$ ")))
      (start-process-shell-command "" nil command))

    (defun wm/run-tmux (command)
      "Run a command in a new window of the tmux session"
      (interactive (list (read-shell-command "[tmux]$ ")))
      (start-process-shell-command ""
     nil
     (concat "terminator -e 'tmux new-session -AD -c $HOME -s "
       wm/tmux-session-name
       "\\; new-window -c $(pwd) \""
       command
       "\"'")))


    (wm/define-launcher wm/browser (or (getenv "X_BROWSER")
         "firefox"))
    (wm/define-launcher wm/tmux-shell-here (concat "terminator -e 'tmux new-session -AD -c $HOME -s \""
         wm/tmux-session-name
         "\" \\; new-window -c $(pwd) /usr/bin/zsh'"))
    (wm/define-launcher wm/term (concat "terminator -e 'tmux new-session -AD -c $HOME -s \""
    wm/tmux-session-name
    "\"'"))
    (wm/define-launcher wm/volume-manager "terminator --title Volume -e 'pulsemixer || alsamixer'")
    (wm/define-launcher wm/volume-up "amixer set Master 5%+")
    (wm/define-launcher wm/volume-down "amixer set Master 5%-")
    (wm/define-launcher wm/mute-toggle "amixer set Master toggle")
    (wm/define-launcher wm/mute-mic "amixer set Mic toggle")
    (wm/define-launcher wm/scrot "scrot --select --exec 'mv $f ~/Pictures/screenshots'")
    (wm/define-launcher wm/lock "dm-tool lock")
    (wm/define-launcher wm/music-toggle "mpc toggle")
    (wm/define-launcher wm/music-next "mpc next")
    (wm/define-launcher wm/music-prev "mpc prev")
    (wm/define-launcher wm/music-manager "terminator -e 'ncmpcpp -s playlist -S visualizer'")

    ;; TODO get windmove integration working better
    (when (require 'windmove nil t)
      (use-package framemove
  :config (progn ;; windmove with framemove integration
      (defun wm/frame-move (dir)
        (pcase dir
          ('up (fm-up-frame))
          ('down (fm-down-frame))
          ('left (fm-left-frame))
          ('right (fm-right-frame))))

      (defun wm/do-window-select (dir &optional arg window)
        "Move to the window at direction DIR.
DIR, ARG, and WINDOW are handled as by `windmove-other-window-loc'.
If no window is at direction DIR, an error is signaled."
        (let ((other-window (windmove-find-other-window dir arg window)))
          (cond ((null other-window)
           (wm/frame-move dir))
          ((and (window-minibuffer-p other-window)
          (not (minibuffer-window-active-p other-window)))
           (wm/frame-move dir))
          (t
           (select-window other-window)))))

      (defun wm/move-left (&optional arg)
  (interactive "P")
  (wm/do-window-select 'left arg))

      (defun wm/move-up (&optional arg)
  (interactive "P")
  (wm/do-window-select 'up arg))

      (defun wm/move-right (&optional arg)
  (interactive "P")
  (wm/do-window-select 'right arg))

      (defun wm/move-down (&optional arg)
  (interactive "P")
  (wm/do-window-select 'down arg))

      (exwm-input-set-key (kbd "s-<left>") #'wm/move-left)
      (exwm-input-set-key (kbd "s-<right>") #'wm/move-right)
      (exwm-input-set-key (kbd "s-<up>") #'wm/move-up)
      (exwm-input-set-key (kbd "s-<down>") #'wm/move-down))))


    (defun wm/insert (string)
      "Send `string' to clipboard and then send C-v to application to hopefully
trigger the paste operation, `string' will be inserted into the application."
      (if (derived-mode-p 'exwm-mode)
    (progn
      (kill-new string)
      (dolist (key (string-to-list (kbd "\C-v")))
  (exwm-input--fake-key key))
      (setq kill-ring (cdr kill-ring)))
  (insert string)))

    (defun wm/xrandr-update-outputs ()
      (let ((connected-monitors (car
   (read-from-string
    ;; TODO write in el
    (shell-command-to-string
     "xrandr | awk 'BEGIN {print \"(\"}
    / connected/ {print \"\\\"\" $1 \"\\\"\"}
    END {print \")\"}'"))))
      (i -1))
  (setq exwm-randr-workspace-monitor-plist (cl-reduce (lambda (acc s)
        (setq i (+ i 1))
        (append acc (list i s)))
      connected-monitors
      :initial-value '()))
  (setq i (+ i 1))
  (while (> i (exwm-workspace--count))
    (exwm-workspace-add))
  (while (< i (exwm-workspace--count))
    (exwm-workspace-delete (- (exwm-workspace--count) 1)))))

    (defun wm/rename-buffer ()
      (interactive)
      (exwm-workspace-rename-buffer

       (concat exwm-class-name ": "
   (if (<= (length exwm-title) 50)
       exwm-title
     (concat (substring exwm-title 0 49) "...")))))

    (add-hook 'exwm-update-class-hook 'wm/rename-buffer)
    (add-hook 'exwm-update-title-hook 'wm/rename-buffer)

    (defun wm/xrandr-init ()
      (add-hook 'exwm-randr-screen-change-hook 'wm/xrandr-update-outputs)
      (wm/xrandr-update-outputs)
      (exwm-randr--init))

    (defun wm/xrandr-exit ()
      (remove-hook 'exwm-randr-screen-change-hook 'wm/xrandr-update-outputs)
      (exwm-randr--exit))

    (add-hook 'exwm-init-hook #'wm/xrandr-init)
    (add-hook 'exwm-exit-hook #'wm/xrandr-exit)

    (if (require 'ido nil t)
        (progn (exwm-input-set-key (kbd "s-x b") #'ido-switch-buffer)
               (exwm-config-ido))
      (exwm-config-default))

    (exwm-input-set-key (kbd "s-SPC") #'exwm-input-toggle-keyboard)

    ;; Do stuff
    (exwm-input-set-key (kbd "s-`") #'wm/run-sh-async)
    (exwm-input-set-key (kbd "s-!") #'wm/run-tmux)
    (exwm-input-set-key (kbd "s-x s-x") #'execute-extended-command)

    ;; Navigation
    (exwm-input-set-key (kbd "M-<tab>") #'previous-buffer)
    (exwm-input-set-key (kbd "M-<iso-lefttab>") #'next-buffer)
    (exwm-input-set-key (kbd "M-<left>") #'previous-buffer)
    (exwm-input-set-key (kbd "M-<right>") #'next-buffer)

    ;; Cheating
    (exwm-input-set-key (kbd "s-x s-b") #'ibuffer)

    ;; Apps
    (exwm-input-set-key (kbd "s-x i") #'wm/browser)
    (exwm-input-set-key (kbd "s-x <return>") #'wm/tmux-shell-here)
    (exwm-input-set-key (kbd "s-x v") #'wm/volume-manager)
    (exwm-input-set-key (kbd "s-x l") #'wm/lock)
    (exwm-input-set-key (kbd "s-l") #'wm/lock)
    (exwm-input-set-key (kbd "s-<return>") #'wm/term)

    (exwm-input-set-key (kbd "s-x m") #'wm/music-manager)
    (exwm-input-set-key (kbd "s-x <down>") #'wm/music-toggle)
    (exwm-input-set-key (kbd "s-x <left>") #'wm/music-prev)
    (exwm-input-set-key (kbd "s-x <right>") #'wm/music-next)

    (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") #'wm/volume-up)
    (exwm-input-set-key (kbd "<XF86AudioLowerVolume>") #'wm/volume-down)
    (exwm-input-set-key (kbd "<XF86AudioMute>") #'wm/mute-toggle)
    (exwm-input-set-key (kbd "<XF86AudioMicMute>") #'wm/mute-mic)
    (exwm-input-set-key (kbd "<XF86AudioPlay>") #'wm/music-toggle)
    (exwm-input-set-key (kbd "<XF86AudioNext>") #'wm/music-next)
    (exwm-input-set-key (kbd "<XF86AudioPrev>") #'wm/music-prev)
    (exwm-input-set-key (kbd "<XF86Launch1>") #'wm/scrot)
    (exwm-input-set-key (kbd "<XF86ScreenSaver>") #'wm/lock)
    (exwm-input-set-key (kbd "<XF86LaunchA>") #'wm/music-toggle)
    (exwm-input-set-key (kbd "<XF86Search>") #'wm/music-prev)
    (exwm-input-set-key (kbd "<XF86Explorer>") #'wm/music-next)

    ;; These work in hardware so don't need warning about undefined
    (exwm-input-set-key (kbd "<XF86MonBrightnessDown>") #'wm/no-op)
    (exwm-input-set-key (kbd "<XF86MonBrightnessUp>") #'wm/no-op)
    (exwm-input-set-key (kbd "<XF86Sleep>") #'wm/no-op)
    (exwm-input-set-key (kbd "<XF86WLAN>") #'wm/no-op)

    ;; Keybind to send emacs bound keys to x window while in line mode
    (exwm-input-set-key (kbd "C-q") #'exwm-input-send-next-key)

    (display-time-mode t)
    (display-battery-mode t)
    (exwm-systemtray-enable)
    (exwm-enable)))

;; init.el ends here

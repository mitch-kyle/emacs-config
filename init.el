;;; init.el --- emacs initialization -*- lexical-binding: t; -*-
;;; Commentary:
;; Generated file. do not edit. changes may be overwritten
;;; Code:

(defvar bootstrap-version)
(defvar straight-repository-branch "develop")
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
(require 'bind-key)

(use-package org-plus-contrib
  :mode ("\\.org\\'" . org-mode)
  :init (progn (setq org-directory                    "~/org"
                     org-adapt-indentation            nil
                     org-edit-src-content-indentation 0)
               (mkdir org-directory t))
  :config
  (progn
    (require 'org-eldoc)
    (org-eldoc-load)
    (advice-add 'org-eldoc-documentation-function :before-until
                (lambda (&rest _)
                  (when-let (link
                             (org-element-property :raw-link
                                                   (org-element-context)))
                    (concat "Link: "
                            (propertize link
                                        'face 'org-link))))
                '((name  . mkyle/show-link-in-minibuffer)
                  (depth . 100)))
    (add-hook 'org-mode-hook 'flyspell-mode)
    (add-hook 'org-mode-hook 'yas-minor-mode)))

(use-package toc-org
  :after org
  :hook ((org-mode) . toc-org-mode))

(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-arguments   nil)
  (exec-path-from-shell-shell-name "/bin/sh")
  :config
  (exec-path-from-shell-initialize))

(use-package no-littering
  :ensure t)

(with-eval-after-load 'no-littering
  (let ((base-custom-file (expand-file-name "custom" no-littering-etc-directory)))
    (setq-default custom-file (concat base-custom-file ".el"))
    (load base-custom-file t)))

(defadvice custom-save-all (after mkyle/recompile-custom-file-on-save () activate)
  "Recompile custom files after saving to it"
  (byte-compile-file custom-file))

(defvar mkyle/after-enable-theme-hook nil
  "Hook to run after a theme is enabled.")

(advice-add 'enable-theme :after
            (lambda (theme)
              (unless (eq theme 'user)
                (run-hooks 'mkyle/after-enable-theme-hook)))
            '((name . mkyle/after-enable-theme-hook)))

(use-package monokai-theme
  :straight (monokai-theme :type git
                           :host github
                           :repo "mitch-kyle/monokai-emacs")
  :ensure t
  :config (load-theme 'monokai t))

(use-package diminish :defer t)

(use-package anzu
  :diminish anzu-mode
  :config (global-anzu-mode t))

(size-indication-mode t)

(line-number-mode t)
(column-number-mode t)

(use-package spaceline
  :config
  (progn
    (require 'spaceline)
    (require 'spaceline-segments)

    (setq-default anzu-cons-mode-line-p           nil
                  powerline-default-separator     'contour
                  spaceline-minor-modes-separator " ")

    ;; Projectile doesn't really fit with the other minor modes
    ;; but the menu might be useful. let's move it to it's own
    ;; segment
    (spaceline-define-segment mkyle/projectile
      "Display project name with projectile menu"
      (when (and (boundp projectile-project-root)
                 (projectile-project-root))
        (propertize (projectile-project-name)
                    'local-map (let ((map (make-sparse-keymap)))
                                 (define-key map [mode-line down-mouse-1]
                                   projectile-mode-menu)
                                 map)
                    'mouse-face 'mode-line-highlight)))


    (defun mkyle/spaceline-reset ()
      (spaceline-compile)
      (setq-default mode-line-format
                    '("%e" (:eval (spaceline-ml-main)))))

    (defun mkyle/spaceline-theme (&rest additional-segments)
      "Spaceline emacs theme with some tweaks"
      (spaceline-compile
        `((((((persp-name :fallback workspace-number) window-number)
             :separator "•")
            buffer-modified
            buffer-size)
           :face highlight-face
           :priority 100)
          (anzu :priority 95)
          auto-compile
          ((buffer-id remote-host)
           :priority 98)
          (major-mode :priority 79)
          (process :when active)
          ((flycheck-error flycheck-warning flycheck-info)
           :when active
           :priority 89)
          (minor-modes :when active
                       :priority 9)
          (mu4e-alert-segment :when active)
          (erc-track :when active)
          (version-control :when active
                           :priority 78)
          (mkyle/projectile :priority 20)
          (org-pomodoro :when active)
          (org-clock :when active)
          nyan-cat)
        `(which-function
          (python-pyvenv :fallback python-pyenv)
          (purpose :priority 94)
          (battery :when active)
          (selection-info :priority 95)
          input-method
          ((point-position line-column)
           :separator " • "
           :priority 96)
          ((buffer-encoding-abbrev)
           :priority 9)
          (global :when active)
          ,@additional-segments
          (buffer-position :priority 99)
          (hud :priority 99)))
      (setq-default mode-line-format
                    '("%e" (:eval (spaceline-ml-main)))))

    (mkyle/spaceline-theme)
    (add-hook 'mkyle/after-enable-theme-hook 'mkyle/spaceline-reset)))

(when (member "Terminus" (font-family-list))
  (set-frame-font "Terminus 12" nil (frame-list)))

(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

(use-package emojify
  :init (defvar emojify-display-style 'unicode))

(setq inhibit-startup-screen            t
      initial-scratch-message           nil)

;; Display a fortune instead of the welcome to emacs message
(defun display-startup-echo-area-message ()
  (when (executable-find "fortune")
    (message "%s" (substring (shell-command-to-string "fortune -sa") 0 -1))))

(setq-default large-file-warning-threshold 104857600)

(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode +1))

(defadvice server-visit-files (before parse-numbers-in-lines
                                      (files proc &optional nowait)
                                      activate)
  "Open file with emacsclient with cursors positioned on requested line.
Most of console-based utilities prints filename in format
'filename:linenumber'.  So you may wish to open filename in that format.
Just call:

  emacsclient filename:linenumber

to open 'filename' and set the cursor on line 'linenumber'."
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

(use-package savehist
  :config (savehist-mode 1))

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

(global-unset-key (kbd "C-x C-z"))

(setq scroll-margin                   0
      scroll-conservatively           100000
      scroll-preserve-screen-position 1)

(global-linum-mode t)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(with-eval-after-load 'ibuffer
  (add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode))

(use-package ibuffer
  :hook
  ;; Update list when ibuffer gets focus
  (ibuffer-mode-hook ibuffer-auto-mode)
  :commands (ibuffer)
  :config
  ;; increase buffer name column width
  (setq ibuffer-formats '((mark modified read-only " "
                                ;; 40 40 is the column width
                                (name 40 40 :left :elide) " "
                                (size 9 -1 :right) " "
                                (mode 8 8 :left :elide) " "
                                filename-and-process)
                          (mark " " (name 16 -1) " " filename))))

(use-package ibuffer-dynamic-groups
  :after ibuffer
  :straight (ibuffer-dynamic-groups :type git
                                    :host github
                                    :repo "mitch-kyle/ibuffer-dynamic-groups")
  :config (progn
            (setq ibuffer-show-empty-filter-groups nil)
            (ibuffer-dynamic-groups-add
             (lambda (groups)
               (append groups
                       '(("System" (name . "^\\*.*\\*$")))))
             '((name . system-group)))
            (ibuffer-dynamic-groups t)))

(use-package smex) ;; Completion History

(use-package ivy
  :straight (swiper :type git
                    :host github
                    :repo "abo-abo/swiper"
                    :files (:defaults (:exclude "ivy-hydra.el")))
  :ensure t
  :after smex
  :init (setq ivy-use-virtual-buffers       nil
              enable-recursive-minibuffers  t
              ;; Enable fuzzy matching except in swiper
              ivy-re-builders-alist         '((t      . ivy--regex-ignore-order)
                                              (swiper . ivy--regex-plus))
              ivy-wrap                      t
              ivy-use-selectable-prompt     t
              projectile-completion-system  'ivy
              ivy-initial-inputs-alist      nil
              ivy-count-format              "%d/%d ")
  :bind (:map ivy-mode-map
              ("C-c C-r" . ivy-resume)
              :map global-map
              ("C-s" . swiper)
              :map ivy-minibuffer-map
              ("<return>"   . ivy-alt-done) ;; Complete directory like ido
              ("M-<return>" . ivy-done))
  :config
  ;; Remove the org-mode face that messes up formatting
  (setq ivy-switch-buffer-faces-alist
        (assq-delete-all 'org-mode ivy-switch-buffer-faces-alist)))

;; Add info to ivy buffer
(use-package ivy-rich
  :ensure t
  :after ivy
  :config (ivy-rich-mode +1))

;; Something is enabling ido at startup and I can't find it
;; so hack is off after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (counsel-mode +1)
            (ivy-mode +1)
            (diminish 'counsel-mode)
            (diminish 'ivy-mode)
            (ido-mode -1)))

(windmove-default-keybindings)

(progn
  (global-set-key [s-left]  'windmove-left)
  (global-set-key [s-right] 'windmove-right)
  (global-set-key [s-up]    'windmove-up)
  (global-set-key [s-down]  'windmove-down))

(unless window-system
  (global-set-key (kbd "C-c <left>")  'windmove-left)
  (global-set-key (kbd "C-c <right>") 'windmove-right)
  (global-set-key (kbd "C-c <up>")    'windmove-up)
  (global-set-key (kbd "C-c <down>")  'windmove-down))

(setq-default create-lockfiles nil)

(with-eval-after-load 'no-littering
  (setq-default auto-save-file-name-transforms
                `((".*" ,no-littering-var-directory t))))

(use-package projectile
  :diminish projectile-mode
  :config (progn
            (global-set-key (kbd "C-c p") projectile-command-map)
            (projectile-mode t)))

(use-package ibuffer-projectile
  :after (:all projectile ibuffer-dynamic-groups)
  :config
  (progn
    (setq ibuffer-projectile-prefix "- ")
    (with-eval-after-load 'ibuffer-dynamic-groups
      (ibuffer-dynamic-groups-add
       (lambda (groups)
         (append (ibuffer-projectile-generate-filter-groups)
                 groups))
       '((name . projectile-groups)
         (depth . -50))))))

(cua-mode t)

(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode)
  :hook ((prog-mode) . rainbow-delimiters-mode))

(setq-default indent-tabs-mode  nil
              tab-width         4
              tab-always-indent 'complete)

(add-hook 'before-save-hook 'whitespace-cleanup)

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

(use-package rainbow-mode
  :defer t
  :commands rainbow-mode
  :diminish rainbow-mode)

(use-package highlight-symbol
  :hook ((prog-mode) . highlight-symbol-mode)
  :diminish highlight-symbol-mode
  :config (set-face-attribute 'highlight-symbol-face nil
                              :background nil
                              :underline t))

(use-package flyspell
  :commands flyspell-mode
  :config
  (setq-default flyspell-issue-welcome-flag nil
                flyspell-issue-message-flag nil
                ispell-program-name         "/usr/bin/aspell"
                ispell-list-command         "list"))

(use-package yasnippet
  :bind (:map yas-minor-mode-map
              ("C-`" . yas-expand)
              ("C-/" . yas-insert-snippet))
  :commands yas-minor-mode)

(use-package yasnippet-snippets
  :after yasnippets)

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
  (set-frame-parameter nil 'alpha '(95 . 95))
  ;; Make new frame transparent because we don't always inherit
  (add-to-list 'after-make-frame-functions
               (lambda (&rest _)
                 (set-frame-parameter nil 'alpha '(95 . 95)))))

(use-package eldoc
  :diminish eldoc-mode
  :hook ((emacs-lisp-mode) . eldoc-mode)
  :config (global-eldoc-mode +1))

(use-package auto-compile
  :config
  (progn
    (setq auto-compile-display-buffer    nil
          auto-compile-mode-line-counter t)
    (auto-compile-on-load-mode +1)
    (auto-compile-on-save-mode +1)))

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
            (setq nrepl-log-messages                   t
                  cider-inject-dependencies-at-jack-in t)
            (add-hook 'cider-mode-hook      'eldoc-mode)
            (add-hook 'cider-repl-mode-hook 'subword-mode)
            (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

            (define-key cider-mode-map (kbd "C-c f") 'cider-find-var)

            (with-eval-after-load 'ibuffer-dynamic-groups
              (ibuffer-dynamic-groups-add
               (lambda (groups)
                 (append '(("Cider" (or (name . "^\\*nrepl-.*\\*$")
                                        (name . "^\\*cider-.*\\*$"))))
                         groups))
               '((name . cider-group)
                 (depth . -1))))))

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  :config (add-hook 'cmake-mode-hook 'yas-minor-mode))

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
  :mode ("\\.md\\'" "\\.markdown\\'")
  :config (progn (add-hook 'markdown-mode-hook 'flyspell-mode)
                 (add-hook 'markdown-mode-hook 'yas-minor-mode)))

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package sh-script
  :ensure t
  :config
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
                  (sh-set-shell "zsh"))))))

(use-package terraform-mode
  :mode ("\\.tf\\'" "\\.tvars\\'"))

(use-package aurel
  :when (executable-find "makepkg")
  :defer t)

(use-package guix
  :when (executable-find "guix")
  :commands (guix)
  :bind (("s-x p" . guix)))

(use-package magit
  :defer t
  :bind (:map global-map
              ("C-x g" . magit-status)))

(use-package git-modes
  :defer t)

(use-package ediff
  :defer t
  :config
  (progn
    (defun mkyle/ediff-write-merge-buffer ()
      (let ((file ediff-merge-store-file))
        (set-buffer ediff-buffer-C)
        (write-region (point-min) (point-max) file)
        (message "Merge buffer saved in: %s" file)
        (set-buffer-modified-p nil)
        (sit-for 1)))
    (add-hook 'ediff-quit-merge-hook 'mkyle/ediff-write-merge-buffer)

    (defvar mkyle/ediff-last-windows nil)

    (defun mkyle/store-pre-ediff-winconfig ()
      (setq mkyle/ediff-last-windows (current-window-configuration)))
    (add-hook 'ediff-before-setup-hook 'mkyle/store-pre-ediff-winconfig)

    (defun mkyle/restore-pre-ediff-winconfig ()
      (dolist (buf (list ediff-buffer-A
                         ediff-buffer-B
                         ediff-buffer-C
                         "*Ediff Control Panel*"
                         "*ediff-errors*"
                         "*ediff-diff*"
                         "*Ediff Registry*"
                         "*ediff-fine-diff*"))
        (set-window-configuration mkyle/ediff-last-windows)
        (condition-case nil
            (let ((buf (get-buffer buf)))
              (when buf (kill-buffer buf)))
          (error nil))))

    (add-hook 'ediff-quit-hook 'mkyle/restore-pre-ediff-winconfig)

    (setq-default ediff-keep-variants nil)

    ;; Don't start a new frame
    (setq-default ediff-window-setup-function 'ediff-setup-windows-plain)))

(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode))

(use-package erc
  :defer t
  :commands (erc)
  :config
  (progn
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
      (erc-spelling-mode 1))))

(use-package persistent-scratch
  :ensure t
  :diminish persistent-scratch-mode
  :config
  (progn
    (persistent-scratch-setup-default)
    (persistent-scratch-autosave-mode +1)))

(with-current-buffer "*scratch*"
  (persistent-scratch-mode +1))

(defun mkyle/scratch ()
  "Get or create the scratch buffer"
  (interactive)
  (unless (get-buffer "*scratch*")
    (persistent-scratch-restore)
    (with-current-buffer "*scratch*"
      (persistent-scratch-mode +1)))
  (let ((buf (get-buffer "*scratch*")))
    (if (eq buf (current-buffer))
        (bury-buffer)
      (switch-to-buffer buf))))

(global-set-key (kbd "s-x s") 'mkyle/scratch)

(use-package vterm
  :commands (vterm)
  :init (setq vterm-always-compile-module t
              vterm-buffer-name-string    "*vterm* %s"
              vterm-copy-mode-map
              (let ((map (make-sparse-keymap)))
                (define-key map (kbd "q")       'vterm-copy-mode-done)
                (define-key map (kbd "c")       'vterm-copy-mode-done)
                (define-key map (kbd "C-c C-c") 'vterm-copy-mode-done)
                (define-key map [return]        'vterm-copy-mode-done)
                (define-key map (kbd "RET")     'vterm-copy-mode-done)
                (define-key map (kbd "r")       'vterm-reset-cursor-point)
                (define-key map (kbd "a")       'vterm-beginning-of-line)
                (define-key map (kbd "e")       'vterm-end-of-line)
                (define-key map (kbd "n")       'vterm-next-prompt)
                (define-key map (kbd "p")       'vterm-previous-prompt)
                map))
  :config
  (progn
    ;; even with this hack it doesn't handle cua-mode very well
    (add-hook 'vterm-mode-hook (lambda ()
                                 (linum-mode -1)
                                 (setq-local cua-enable-cua-keys nil)
                                 (local-set-key (kbd "C-v") 'vterm-yank)
                                 (local-set-key (kbd "C-z") 'vterm-undo)))

    (with-eval-after-load 'ibuffer-dynamic-groups
      (ibuffer-dynamic-groups-add (lambda (groups)
                                    (append '(("Terminals" (mode . vterm-mode)))
                                            groups))
                                  '((name . vterm-group)
                                    (depth . -9))))))

(defun mkyle/vterm-execute (command)
  "Start a vterm session with the given command"
  (interactive (list (read-shell-command "$ ")))
  (let ((vterm-shell command))
    (vterm)))

(use-package vtplex
  :straight (vtplex :type   git
                    :host   github
                    :repo   "mitch-kyle/vtplex"
                    :branch "main")
  :after vterm
  :commands (vtplex vtplex-mode vtplex-execute)
  :bind (:map global-map
              ("s-<return>" . vtplex)
              ("s-!" .        vtplex-execute))
  :config
  (progn (require 'vtplex-spaceline)
         (vtplex-spaceline-enable 'mkyle/projectile)))

(defun mkyle/run-sh-async (&optional command)
  "Interactive prompt to run a shell command in a child process which
may or may not spawn an x window"
  (interactive (list (read-shell-command "$ ")))
  (when command
    (start-process-shell-command "" nil command)))

(global-set-key (kbd "s-`") #'mkyle/run-sh-async)

(defvar mkyle/labeled-buffers (make-hash-table :weakness 'value))

(defun mkyle/labeled-buffer (label create-new)
  "switch to labeled buffer if buffer does not exist create it by
invoking `create-new'."
  (let ((buf (gethash label mkyle/labeled-buffers)))
    (if (and buf (buffer-live-p buf))
        (switch-to-buffer buf)
      (funcall create-new)
      (puthash label (current-buffer) mkyle/labeled-buffers)
      nil)))

(defun mkyle/volume ()
  (interactive)
  (mkyle/labeled-buffer 'mkyle/volume
                        (lambda ()
                          (let ((vterm-shell (if (executable-find "pulsemixer")
                                                 "pulsemixer"
                                               "alsamixer"))
                                (vterm-buffer-name-string "*volume* - %s"))
                            (vterm)))))

(defun mkyle/volume-down ()
  (interactive)
  (start-process-shell-command "" nil "amixer set Master 5%-"))

(defun mkyle/volume-up ()
  (interactive)
  (start-process-shell-command "" nil "amixer set Master 5%+"))

(defun mkyle/volume-mute ()
  (interactive)
  (start-process-shell-command "" nil "amixer set Master toggle"))

(defun mkyle/volume-mute-mic ()
  (interactive)
  (start-process-shell-command "" nil "amixer set Mic toggle"))

(global-set-key (kbd "s-x v") 'mkyle/volume)

(defun mkyle/music ()
  (interactive)
  (mkyle/labeled-buffer 'mkyle/music
                        (lambda ()
                          (let ((vterm-shell "ncmpcpp -s playlist -S visualizer")
                                (vterm-buffer-name-string "*music* - %s"))
                            (vterm)))))

(defun mkyle/music-next ()
  (interactive)
  (start-process-shell-command "" nil "mpc next"))

(defun mkyle/music-prev ()
  (interactive)
  (start-process-shell-command "" nil "mpc prev"))

(defun mkyle/music-toggle ()
  (interactive)
  (start-process-shell-command "" nil "mpc toggle"))

(global-set-key (kbd "s-x m") 'mkyle/music)

(use-package exwm
  ;; TODO find test for emacs on root window to put here
  :if window-system
  :after no-littering
  :commands (exwm-init exwm-enable)
  :defer t
  :config
  (require 'window-manager
           (expand-file-name "window-manager.elc"
                             user-emacs-directory)
           t))

(defvar mkyle/user-lisp-directory
  (expand-file-name "lisp"
                    user-emacs-directory)
  "All emacs lisp files in this directory will be loaded during initialization. default is ~/.emacs.d/lisp.")

(use-package load-directory
  :config
  (when (file-directory-p mkyle/user-lisp-directory)
    (load-directory mkyle/user-lisp-directory t)))

;; init.el ends here

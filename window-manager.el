;;; window-manager.el --- exwm configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; This file was generated. do not edit. changes may be overwritten
;;; Code:

(require 'exwm)
(require 'exwm-config)
(require 'server)

(if (require 'ido nil t)
    (progn (exwm-input-set-key (kbd "s-x b") #'ido-switch-buffer)
           (exwm-config-ido))
  (exwm-config-default))

(unless (server-running-p)
  (server-start))

(exwm-enable)

(require 'exwm-randr)
(defun wm/xrandr-update-outputs ()
  (let* ((monitors   (let ((reg "^\\([^ ]*?\\) connected")
                           (str (shell-command-to-string "xrandr"))
                           (pos 0)
                           (res))
                       (while (setq pos (string-match reg str (1+ pos)))
                         (push (match-string 1 str) res))
                       (nreverse res)))
         (n-monitors (length monitors)))
    (let ((i 0)
          (result))
      (dolist (m monitors result)
        (setq result (cons m
                           (cons i
                                 result)))
        (setq i (1+ i)))
      (setq exwm-randr-workspace-monitor-plist (reverse result)))
    (while (> n-monitors (exwm-workspace--count))
      (exwm-workspace-add))
    (while (< n-monitors (exwm-workspace--count))
      (exwm-workspace-delete (- (exwm-workspace--count) 1)))))

(defun wm/xrandr-init ()
  (add-hook 'exwm-randr-screen-change-hook 'wm/xrandr-update-outputs)
  (wm/xrandr-update-outputs)
  (exwm-randr--init))

(defun wm/xrandr-exit ()
  (remove-hook 'exwm-randr-screen-change-hook 'wm/xrandr-update-outputs)
  (exwm-randr--exit))

(add-hook 'exwm-init-hook #'wm/xrandr-init)
(add-hook 'exwm-exit-hook #'wm/xrandr-exit)

(use-package framemove
  :after windmove
  :init (progn (require 'seq)
               (defalias 'remove-if-not 'seq-filter))
  :config
  (progn
    (require 'windmove)
    (defun mkyle/windmove-framemove-hook (f dir &optional arg window)
      "Hook windmove to framemove properly"
      (condition-case nil
        (funcall f dir arg window)
        ('error (fm-next-frame dir))))

    (advice-add 'windmove-do-window-select
                :around
                #'mkyle/windmove-framemove-hook)))

(exwm-input-set-key (kbd "s-<left>") #'windmove-left)
(exwm-input-set-key (kbd "s-<right>") #'windmove-right)
(exwm-input-set-key (kbd "s-<up>") #'windmove-up)
(exwm-input-set-key (kbd "s-<down>") #'windmove-down)

(setq exwm-workspace-show-all-buffers t
      exwm-layout-show-all-buffers    t)

(defun wm/rename-buffer ()
  (interactive)
  (exwm-workspace-rename-buffer
    (concat exwm-class-name ": "
            (if (<= (length exwm-title) 50)
                exwm-title
              (concat (substring exwm-title 0 49) "...")))))

(add-hook 'exwm-update-class-hook 'wm/rename-buffer)
(add-hook 'exwm-update-title-hook 'wm/rename-buffer)

(require 'exwm-systemtray)
(exwm-systemtray-enable)

(setq display-time-day-and-date t
      display-time-default-load-average nil)
(display-time-mode t)

(use-package fancy-battery
  :commands fancy-battery-mode
  :config (fancy-battery-mode t))

(with-eval-after-load "ibuffer-dynamic-groups"
  (ibuffer-dynamic-groups-add (lambda (groups)
                                (append '(("X Windows" (mode . exwm-mode)))
                                        groups))
                              '((name . exwm-group)
                                (depth . -10))))

(require 'seq)

(defun wm/browser ()
  (interactive)
  (let ((buf (seq-find (lambda (buffer)
                         (with-current-buffer buffer
                           (and (eq major-mode 'exwm-mode)
                                (string= exwm-class-name "firefox")
                                buffer)))
                       (buffer-list))))
    (if (and buf (buffer-live-p buf))
        (switch-to-buffer buf)
      (start-process-shell-command "" nil "firefox"))))

(defun wm/scrot ()
  (interactive)
  (start-process-shell-command "" nil
   "scrot --select --exec 'mv $f ~/Pictures/screenshots'"))

(defun wm/lock ()
  (interactive)
  (start-process-shell-command "" nil "dm-tool lock"))

;; Enable or disable other emacs keybindings in exwm windows
(exwm-input-set-key (kbd "s-SPC") 'exwm-input-toggle-keyboard)

;; Send the next key without it being captured by emacs
(exwm-input-set-key (kbd "C-q") 'exwm-input-send-next-key)

;; Floating Windows
(exwm-input-set-key (kbd "C-c f") 'exwm-floating-toggle-floating)

;; Do stuff
(exwm-input-set-key (kbd "s-`")        'mkyle/run-sh-async)
(exwm-input-set-key (kbd "s-!")        'vtermux-execute)
(exwm-input-set-key (kbd "s-<return>") 'vtermux)
(add-to-list 'exwm-input-prefix-keys 's-return) ;; vtermux

;; Apps
(exwm-input-set-key (kbd "s-x i")             'wm/browser)
(exwm-input-set-key (kbd "s-x v")             'mkyle/volume)
(exwm-input-set-key (kbd "s-x l")             'wm/lock)
(exwm-input-set-key (kbd "<XF86Launch1>")     'wm/scrot)
(exwm-input-set-key (kbd "<XF86ScreenSaver>") 'wm/lock)

;; Music
(exwm-input-set-key (kbd "s-x m")           'mkyle/music)
(exwm-input-set-key (kbd "s-x M-<down>")    'mkyle/music-toggle)
(exwm-input-set-key (kbd "s-x M-<left>")    'mkyle/music-prev)
(exwm-input-set-key (kbd "s-x M-<right>")   'mkyle/music-next)
(exwm-input-set-key (kbd "<XF86AudioPlay>") 'mkyle/music-toggle)
(exwm-input-set-key (kbd "<XF86AudioNext>") 'mkyle/music-next)
(exwm-input-set-key (kbd "<XF86AudioPrev>") 'mkyle/music-prev)

;; Some laptops put playback symbols on other keys for some reason
(exwm-input-set-key (kbd "<XF86LaunchA>")   'mkyle/music-toggle)
(exwm-input-set-key (kbd "<XF86Search>")    'mkyle/music-prev)
(exwm-input-set-key (kbd "<XF86Explorer>")  'mkyle/music-next)

;; Audio Control
(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") 'mkyle/volume-up)
(exwm-input-set-key (kbd "<XF86AudioLowerVolume>") 'mkyle/volume-down)
(exwm-input-set-key (kbd "<XF86AudioMute>")        'mkyle/volume-mute)
(exwm-input-set-key (kbd "<XF86AudioMicMute>")     'mkyle/volume-mute-mic)

;; These work in outside the window manager so don't need warning about undefined
(let ((noop (lambda () (interactive))))
  (exwm-input-set-key (kbd "<XF86MonBrightnessDown>") noop)
  (exwm-input-set-key (kbd "<XF86MonBrightnessUp>")   noop)
  (exwm-input-set-key (kbd "<XF86Sleep>")             noop)
  (exwm-input-set-key (kbd "<XF86WLAN>")              noop))

(provide 'window-manager)
;;; window-manager.el ends here

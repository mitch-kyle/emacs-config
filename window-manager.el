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
(require 'seq)
(defun wm/xrandr-update-outputs ()
  (let* ((monitors   (car
                      (read-from-string
                        ;; TODO write in el
                       (shell-command-to-string
                        "xrandr | awk 'BEGIN {print \"(\"}
                                       / connected/ {print \"\\\"\" $1 \"\\\"\"}
                                       END {print \")\"}'"))))
         (n-monitors (length monitors)))
    (setq exwm-randr-workspace-monitor-plist
          (apply 'append
                 (seq-map-indexed (lambda (montr idx)
                                    (list idx montr))
                                  monitors)))
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
  :config (setq framemove-hook-into-windmove t))

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

(setq display-time-day-and-date t)
(display-time-mode t)

(display-battery-mode t)

(with-eval-after-load "ibuffer-dynamic-groups"
  (ibuffer-dynamic-groups-add (lambda (groups)
                                (append '(("X Windows" (mode . exwm-mode)))
                                        groups))
                              '((name . exwm-group)
                                (depth . -10))))

(defmacro wm/define-launcher (fun-name command-and-args)
  "Define an interactive function that invokes the shell command given"
  `(defun ,fun-name ()
     (interactive)
     (start-process-shell-command "" nil ,command-and-args)))

(defun wm/run-sh-async (command)
  "Interactive prompt to run a shell command in a child process which
may or may not spawn an x window"
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command "" nil command))

(defvar wm/terminal-emulator "terminator")
(defvar wm/preferred-shell "/usr/bin/zsh")

(defvar wm/tmux-session-name "0")

(defun wm/run-tmux (command)
  "Run a command in a new window of the tmux session"
  (interactive (list (read-shell-command "[tmux]$ ")))
  (start-process-shell-command
   "" nil
   (concat wm/terminal-emulator
           " -e 'tmux new-session -AD -c $HOME -s "
           wm/tmux-session-name
           "\\; new-window -c $(pwd) \""
           command
           "\"'")))

(wm/define-launcher wm/tmux-shell-here
                    (concat wm/terminal-emulator
                            " -e 'tmux new-session -AD -c $HOME -s \""
                            wm/tmux-session-name
                            "\" \\; new-window -c $(pwd) "
                            wm/preferred-shell "'"))

(wm/define-launcher wm/tmux (concat wm/terminal-emulator
                                    " -e 'tmux new-session -AD -c $HOME -s \""
                                    wm/tmux-session-name "\"'"))

(wm/define-launcher wm/browser (or (getenv "X_BROWSER") "firefox"))

(wm/define-launcher wm/volume-manager
                    (concat wm/terminal-emulator
                            " --title Volume -e 'pulsemixer || alsamixer'"))
(wm/define-launcher wm/volume-up "amixer set Master 5%+")
(wm/define-launcher wm/volume-down "amixer set Master 5%-")
(wm/define-launcher wm/mute-toggle "amixer set Master toggle")
(wm/define-launcher wm/mute-mic "amixer set Mic toggle")

(wm/define-launcher wm/music-toggle "mpc toggle")
(wm/define-launcher wm/music-next "mpc next")
(wm/define-launcher wm/music-prev "mpc prev")
(wm/define-launcher wm/music-manager
                    (concat wm/terminal-emulator
                            " -e 'ncmpcpp -s playlist -S visualizer'"))

(wm/define-launcher wm/scrot
                    "scrot --select --exec 'mv $f ~/Pictures/screenshots'")

(wm/define-launcher wm/lock "dm-tool lock")

;; Enable or disable other emacs keybindings in exwm windows
(exwm-input-set-key (kbd "s-SPC") 'exwm-input-toggle-keyboard)

;;Send the next key without it being captured by emacs
(exwm-input-set-key (kbd "C-q") 'exwm-input-send-next-key)

;; Do stuff
(exwm-input-set-key (kbd "s-`") 'wm/run-sh-async)
(exwm-input-set-key (kbd "s-!") 'wm/run-tmux)

;; Apps
(exwm-input-set-key (kbd "s-x i")             'wm/browser)
(exwm-input-set-key (kbd "s-x v")             'wm/volume-manager)
(exwm-input-set-key (kbd "s-x l")             'wm/lock)
(exwm-input-set-key (kbd "s-x <return>")      'wm/tmux-shell-here)
(exwm-input-set-key (kbd "s-<return>")        'wm/tmux)
(exwm-input-set-key (kbd "<XF86Launch1>")     'wm/scrot)
(exwm-input-set-key (kbd "<XF86ScreenSaver>") 'wm/lock)

;; Music
(exwm-input-set-key (kbd "s-x m")           'wm/music-manager)
(exwm-input-set-key (kbd "s-x M-<down>")    'wm/music-toggle)
(exwm-input-set-key (kbd "s-x M-<left>")    'wm/music-prev)
(exwm-input-set-key (kbd "s-x M-<right>")   'wm/music-next)
(exwm-input-set-key (kbd "<XF86AudioPlay>") 'wm/music-toggle)
(exwm-input-set-key (kbd "<XF86AudioNext>") 'wm/music-next)
(exwm-input-set-key (kbd "<XF86AudioPrev>") 'wm/music-prev)
;; Some laptops put playback symbols on other keys for some reason
(exwm-input-set-key (kbd "<XF86LaunchA>")   'wm/music-toggle)
(exwm-input-set-key (kbd "<XF86Search>")    'wm/music-prev)
(exwm-input-set-key (kbd "<XF86Explorer>")  'wm/music-next)

;; Audio Control
(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") 'wm/volume-up)
(exwm-input-set-key (kbd "<XF86AudioLowerVolume>") 'wm/volume-down)
(exwm-input-set-key (kbd "<XF86AudioMute>")        'wm/mute-toggle)
(exwm-input-set-key (kbd "<XF86AudioMicMute>")     'wm/mute-mic)

;; These work in hardware so don't need warning about undefined
(exwm-input-set-key (kbd "<XF86MonBrightnessDown>") (lambda () (interactive)))
(exwm-input-set-key (kbd "<XF86MonBrightnessUp>")   (lambda () (interactive)))
(exwm-input-set-key (kbd "<XF86Sleep>")             (lambda () (interactive)))
(exwm-input-set-key (kbd "<XF86WLAN>")              (lambda () (interactive)))

;;; window-manager.el ends here

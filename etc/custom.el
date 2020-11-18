(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-align-annotations t)
 '(company-tooltip-flip-when-above t)
 '(custom-safe-themes
   '("fd2fdc57880ec7f9e075325b7f792ddda6e913559e22dba6d423942d727f638d" "2925ed246fb757da0e8784ecf03b9523bccd8b7996464e587b081037e0e98001" default))
 '(exwm-floating-border-color "#f8f8f2")
 '(jdee-db-active-breakpoint-face-colors (cons "#131033" "#1ea8fc"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#131033" "#a7da1e"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#131033" "#546A90"))
 '(objed-cursor-color "#e61f44")
 '(pdf-view-midnight-colors (cons "#f2f3f7" "#0c0a20"))
 '(rustic-ansi-faces
   ["#0c0a20" "#e61f44" "#a7da1e" "#ffd400" "#1ea8fc" "#ff2afc" "#42c6ff" "#f2f3f7"])
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
           (lambda nil
             (dolist
                 (file
                  (org-babel-tangle-file
                   (buffer-file-name)
                   nil "emacs-lisp"))
               (byte-compile-file file)))
           1 t)
     (eval modify-syntax-entry 43 "'")
     (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(spaceline-flycheck-error ((t (:distant-foreground "#F70057" :foreground "#FA518D"))))
 '(spaceline-flycheck-info ((t (:distant-foreground "#40CAE4" :foreground "#92E7F7"))))
 '(spaceline-flycheck-warning ((t (:distant-foreground "#BEB244" :foreground "#FFF7A8"))))
 '(spaceline-highlight-face ((t (:background "#A6E22E" :foreground "#3E3D31" :inherit 'mode-line))))
 '(spaceline-modified ((t (:background "#92E7F7" :foreground "#3E3D31" :inherit 'mode-line))))
 '(spaceline-python-venv ((t (:distant-foreground "#FB35EA" :foreground "#FE8CF4"))))
 '(spaceline-read-only ((t (:background "#AE81FF" :foreground "#3E3D31" :inherit 'mode-line))))
 '(spaceline-unmodified ((t (:background "#a6e22e" :foreground "#3E3D31" :inherit 'mode-line)))))

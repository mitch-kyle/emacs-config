;;; init.el --- emacs early initialization -*- lexical-binding: t; -*-
;;; Commentary:
;; Generated file. do not edit. changes may be overwritten
;;; Code:

(setq gc-cons-threshold  most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold  33554432 ;32M
          gc-cons-percentage 0.1)))

(let ((saved-file-name-handler-alist file-name-handler-alist))
  (setq file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook
    (lambda ()
      (setq file-name-handler-alist (append saved-file-name-handler-alist file-name-handler-alist)))))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq mode-line-format nil)

(defalias 'yes-or-no-p 'y-or-n-p)

;; early-init.el ends here

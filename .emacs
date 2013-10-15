;; share clipboard with X
(setq x-select-enable-clipboard t
        x-select-enable-primary t
        save-interprogram-paste-before-kill t)

;; save recently open files = 'desktop'
(desktop-save-mode 1)
(defun my-desktop-save ()
    (interactive)
    ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
    (if (eq (desktop-owner) (emacs-pid))
        (desktop-save desktop-dirname)))
  (add-hook 'auto-save-hook 'my-desktop-save)

;; delete selection works
(delete-selection-mode 1)

;;
(ido-mode t)

;; append directory to buffer name when multiple files with same name are opened
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; save cursor location in file
(require 'saveplace)
(setq-default save-place t)

;; match parantheses
(show-paren-mode t)

;; matlab.el
;;(add-to-list 'load-path "~/matlab-emacs/matlab.el")
;;(require 'matlab-load)

;;  (setq matlab-indent-function-body t)  ; if you want function bodies indented
;;  (setq matlab-verify-on-save-flag nil) ; turn off auto-verify on save
;;  (defun my-matlab-mode-hook ()
;;    (setq fill-column 76))		; where auto-fill should wrap
;;(gloabl-font-lock-mode 1)
(setq x-select-enable-clipboard t)
(desktop-save-mode 1)
(delete-selection-mode 1)
(defun my-desktop-save ()
    (interactive)
    ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
    (if (eq (desktop-owner) (emacs-pid))
        (desktop-save desktop-dirname)))
  (add-hook 'auto-save-hook 'my-desktop-save)

;; matlab.el
;;(add-to-list 'load-path "~/matlab-emacs/matlab.el")
;;(require 'matlab-load)

;;  (setq matlab-indent-function-body t)  ; if you want function bodies indented
;;  (setq matlab-verify-on-save-flag nil) ; turn off auto-verify on save
;;  (defun my-matlab-mode-hook ()
;;    (setq fill-column 76))		; where auto-fill should wrap
;;(gloabl-font-lock-mode 1)
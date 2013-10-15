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

;;allows many operations (like buffer switching and file navigation) to be enhanced with instant feedback among the completion choices. 
(ido-mode t)

;; append directory to buffer name when multiple files with same name are opened
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; save cursor location in file
(require 'saveplace)
(setq-default save-place t)

;; match parantheses
(show-paren-mode t)

;; recentf stuff - recently used files
(require 'recentf)
;; uncomment below line if using tramp
;;(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x \C-r" 'recentf-open-files)

;; matlab.el
;;(add-to-list 'load-path "~/matlab-emacs/matlab.el")
;;(require 'matlab-load)

;;  (setq matlab-indent-function-body t)  ; if you want function bodies indented
;;  (setq matlab-verify-on-save-flag nil) ; turn off auto-verify on save
;;  (defun my-matlab-mode-hook ()
;;    (setq fill-column 76))		; where auto-fill should wrap
;;(gloabl-font-lock-mode 1)

;; org mode keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
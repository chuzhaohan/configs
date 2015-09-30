;;; Commentary
;; My matlab-mode configuration
;; - Deepak Cherian

;; matlab.el
(autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
;;(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)

(require 'matlab-load)
(setq matlab-indent-function-body t)  ; if you want function bodies indented
(setq matlab-verify-on-save-flag nil) ; turn off auto-verify on save

(add-to-list 'auto-mode-alist
             '("\\.m$" . matlab-mode))
;; mode hook
(defun my-matlab-mode-hook ()
  (local-set-key (kbd "\C-c\C-m") 'matlab-shell)
  (local-set-key (kbd "\C-c\C-a") 'matlab-shell-run-cell)
  (local-set-key (kbd "\C-c\C-a") 'matlab-shell-run-cell)
  (local-set-key (kbd "\C-c\C-d") 'comment-region)
)

;; shell hook
(defun my-matlab-shell-hook ()
  (setq fill-column 76) ; where auto-fill should wrap
  ;;(add-to-list 'completion-at-point-functions
  ;;             'semantic-completion-at-point-function)
  (setq comint-input-ring-file-name "~/.matlab/R2014b/history.m"))

(add-hook 'matlab-mode-hook 'my-matlab-mode-hook)
(add-hook 'matlab-shell-mode-hook 'my-matlab-shell-hook)

;; have  matlab-shell read command history
(comint-read-input-ring t)

;; get CEDET running
;; (matlab-cedet-setup)

;; prevent rendering of urls in output. hopefully, this fixes responsiveness
(add-hook 'matlab-shell-mode-hook
          (lambda () (remove-hook 'comint-output-filter-functions
                                  'matlab-shell-render-errors-as-anchor t)))

;; default options for starting matlab
(custom-set-variables
 '(matlab-shell-command-switches '("-nodesktop -nosplash")))

;; for GDB/debugging in general
(global-set-key (kbd "<f5>") 'gud-cont)
(global-set-key (kbd "<f11>") 'gud-step) ;; equiv matlab step in
(global-set-key (kbd "<f10>") 'gud-next) ;; equiv matlab step 1
(global-set-key (kbd "<f7>") 'gud-finish) ;; equiv matlab step out

(provide 'dc-matlab)
;;; dc-matlab ends here

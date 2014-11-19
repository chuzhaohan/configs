;;; Commentary
;; My matlab-mode configuration
;; - Deepak Cherian

;; matlab.el
(autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
;;(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)

;; keyboard-shortcut for matlab-shell
(global-set-key (kbd "C-c C-m") 'matlab-shell)

(require 'matlab-load)
  (setq matlab-indent-function-body t)  ; if you want function bodies indented
  (setq matlab-verify-on-save-flag nil) ; turn off auto-verify on save
  (defun my-matlab-mode-hook ()
    (setq fill-column 76) ; where auto-fill should wrap
    (add-to-list 'completion-at-point-functions
                 'semantic-completion-at-point-function))

;; have  matlab-shell read command history
(setq comint-input-ring-file-name "~/.matlab/R2014a/history.m")
(comint-read-input-ring t)

;; prevent rendering of urls in output. hopefully, this fixes responsiveness
(add-hook 'matlab-shell-mode-hook
          (lambda () (remove-hook 'comint-output-filter-functions
                                  'matlab-shell-render-errors-as-anchor t)))

;; default options for starting matlab
(custom-set-variables
 '(matlab-shell-command-switches '("-nodesktop -nosplash")))

;; attempt to get execute-cell to work
(defun matlab-inside-comment-p ()
  (save-excursion
    (beginning-of-line 1)
    (looking-at "^%")))

(defun matlab-select-cell ()
  (interactive)
  (goto-char
   (if (re-search-backward "^\\s-*%%[^%]" nil t)
       (match-end 0)
     (point-min)))
  (while (and (python-inside-comment-p)
              (eq 0 (forward-line 1)))
    nil)
  (set-mark (point))
  (goto-char
   (if (re-search-forward "^\\s-*\\(%%[^%]\\)" nil t)
       (- (match-beginning 1) 2)
     (point-max))))

;; for GDB/debugging in general
(global-set-key (kbd "<f5>") 'gud-cont)
(global-set-key (kbd "<f11>") 'gud-step) ;; equiv matlab step in
(global-set-key (kbd "<f10>") 'gud-next) ;; equiv matlab step 1
(global-set-key (kbd "<f7>") 'gud-finish) ;; equiv matlab step out

;;; Code:
;; prelude options
;; disable guru mode
(setq prelude-guru nil)

;; Disable whitespace mode
;;(setq prelude-whitespace nil)

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

;; disabled now because prelude has a better version apparently
;;allows many operations (like buffer switching and file navigation) to be enhanced with instant feedback among the completion choices.
;;(ido-mode t)

;; append directory to buffer name when multiple files with same name are opened
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; save cursor location in file
(require 'saveplace)
(setq-default save-place t)

;; match parentheses
(show-paren-mode t)

;; set solarized theme by default
(disable-theme 'zenburn)
(load-theme 'solarized-light t)

;; use spaces for TABs
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)

;; line numbers by default
(global-linum-mode 1)

;; recentf stuff - recently used files
(require 'recentf)
;; uncomment below line if using tramp
;;(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x \C-r" 'recentf-open-files)

;; LaTeX configuration
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)

(setq TeX-output-view-style
    (quote
     (("^pdf$" "." "evince -f %o")
      ("^html?$" "." "iceweasel %o"))))

;; Setting up writegood-mode
(require 'writegood-mode)
(global-set-key "\C-cg" 'writegood-mode)

;; which-function-mode
(which-function-mode)
(setq which-func-unknown "n/a")

;; electric indent mode enabled
(electric-indent-mode +1)

;; matlab.el
(add-to-list 'load-path "~/.emacs.d/personal/matlab-emacs/")
(autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)
(load-library "~/.emacs.d/personal/matlab-emacs/matlab-load.el")
(require 'matlab-load)
  (setq matlab-indent-function-body t)  ; if you want function bodies indented
  (setq matlab-verify-on-save-flag nil) ; turn off auto-verify on save
  (defun my-matlab-mode-hook ()
    (setq fill-column 76)); where auto-fill should wrap
;; default options for starting matlab
(custom-set-variables
 '(matlab-shell-command-switches '("-nodesktop -nosplash")))

;; for GDB/debugging in general
(global-set-key (kbd "<f5>") 'gud-cont)
(global-set-key (kbd "<f11>") 'gud-step) ;; equiv matlab step in
(global-set-key (kbd "<f10>") 'gud-next) ;; equiv matlab step 1
(global-set-key (kbd "<f7>") 'gud-finish) ;; equiv matlab step out

(global-font-lock-mode 1)

;; org mode keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)

;; setup marmalade
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
               '("marmalade" .
                 "http://marmalade-repo.org/packages/"))
  (package-initialize)
)
;; syntax checking with flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; jedi mode
(autoload 'jedi-setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi-setup)

;; auto-complete
(require 'auto-complete)
(global-auto-complete-mode t)
(provide '.emacs)

;;; .emacs ends here

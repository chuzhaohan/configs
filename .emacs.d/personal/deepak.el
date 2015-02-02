;;; Code:

;; include my other files - not needed with prelude
;;(load "dc-org.el")
;;(load "dc-latex.el")
;;(load "dc-comint.el")
;;(load "dc-matlab.el")

;; prelude options
;; disable guru mode
(setq prelude-guru nil)

;; Disable whitespace mode
(setq prelude-whitespace nil)

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

;; get wb-butler-global-mode to work with emacs --daemon
;;(defun run-settings-functions-which-require-client ()
;;  (ws-butler-global-mode t)
;;)
;;(add-hook 'server-visit-hook 'run-settings-functions-which-require-client)

;; delete selection works
(delete-selection-mode 1)

;; Return gives both newline and indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; move ace-window away from C-xo
(global-unset-key (kbd "\C-x o"))
(global-set-key (kbd "\M-p") 'ace-window)
(global-set-key (kbd "\C-x o") 'other-window)


;; disabled now because prelude has a better version apparently
;;allows many operations (like buffer switching and file navigation) to be enhanced with instant feedback among the completion choices.
;;(ido-mode t)

;; semantic / CEDET
(semantic-mode 1)
(setq semantic-default-submodes
      '(global-semantic-decoration-mode
        global-semanticdb-minor-mode
        global-semantic-idle-scheduler-mode
        global-semantic-idle-completions-mode
        global-semantic-highlight-func-mode
        global-semantic-stickyfunc-mode))

;; append directory to buffer name when multiple files with same name are opened
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; suggest to Emacs that it should split the frame vertically rather than horizontally
;; when Emacs has the choice (eg when bringing up help).
;; https://stackoverflow.com/questions/20167246/emacs-open-buffer-in-vertical-split-by-default
(setq split-height-threshold nil)
(setq split-width-threshold 160)

;; save cursor location in file
(require 'saveplace)
(setq-default save-place t)

;; match parentheses
(show-paren-mode t)

;; set solarized theme by default
(disable-theme 'zenburn)
(require 'color-theme)
(color-theme-initialize)
(color-theme-solarized-light)

;; use spaces for TABs
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

;; line numbers by default
(global-linum-mode 1)
(setq linum-format "%d ")

;; recentf stuff - recently used files
(require 'recentf)
;; uncomment below line if using tramp
;;(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\C-r" 'recentf-open-files)

;; pydoc-info
(add-to-list 'load-path "~/.emacs.d/modules/pydoc-info/")
(load-library "~/.emacs.d/modules/pydoc-info/pydoc-info.el")
(require 'pydoc-info)

;; Setting up writegood-mode
(require 'writegood-mode)
(global-set-key "\C-cg" 'writegood-mode)

;; which-function-mode
(which-function-mode)
(setq which-func-unknown "n/a")

;; electric indent mode enabled
(electric-indent-mode +1)

;; whitespace-butler mode
;;(ws-butler-mode)

(global-font-lock-mode 1)

;; setup marmalade
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
               '("marmalade" .
                 "http://marmalade-repo.org/packages/"))
  (package-initialize)
  )

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;; Page down/up move the point, not the screen.
;; In practice, this means that they can move the
;; point to the beginning or end of the buffer.
(global-set-key [next]
  (lambda () (interactive)
    (condition-case nil (scroll-up)
      (end-of-buffer (goto-char (point-max))))))

(global-set-key [prior]
  (lambda () (interactive)
    (condition-case nil (scroll-down)
      (beginning-of-buffer (goto-char (point-min))))))

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

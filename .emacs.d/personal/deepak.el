;;; Code:
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

;; whitespace butler on
;;(ws-butler-mode t)

;; set solarized theme by default
(disable-theme 'zenburn)
(require 'color-theme)
(color-theme-initialize)
(color-theme-solarized-light)
;;(load-theme 'solarized-light t)

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
(global-set-key "\C-x \C-r" 'recentf-open-files)

;; pydoc-info
(add-to-list 'load-path "~/.emacs.d/modules/pydoc-info/")
(load-library "~/.emacs.d/modules/pydoc-info/pydoc-info.el")
(require 'pydoc-info)

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

;; whitespace-butler mode
;;(ws-butler-mode)

;; matlab.el
;;(add-to-list 'load-path "~/.emacs.d/personal/matlab-emacs/")
(autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
;;(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)
;;(load-library "~/.emacs.d/personal/matlab-emacs/matlab-load.el")

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
;;;;;;;;;;;;;;;;;;;;;;;; END MATLAB STUFF ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;; COMINT STUFF ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; close comint Completion buffers by default
;; from https://snarfed.org/automatically_close_completions_in_emacs_shell_comint_mode
(defun comint-close-completions ()
    "Close the comint completions buffer.
Used in advice to various comint functions to automatically close
the completions buffer as soon as I'm done with it. Based on
Dmitriy Igrishin's patched version of comint.el."
    (if comint-dynamic-list-completions-config
        (progn
          (set-window-configuration comint-dynamic-list-completions-config)
          (setq comint-dynamic-list-completions-config nil))))

(defadvice comint-send-input (after close-completions activate)
  (comint-close-completions))

(defadvice comint-dynamic-complete-as-filename (after close-completions activate)
  (if ad-return-value (comint-close-completions)))

(defadvice comint-dynamic-simple-complete (after close-completions activate)
  (if (member ad-return-value '('sole 'shortest 'partial))
      (comint-close-completions)))

(defadvice comint-dynamic-list-completions (after close-completions activate)
  (comint-close-completions)
  (if (not unread-command-events)
      ;; comint's "Type space to flush" swallows space. put it back in.
      (setq unread-command-events (listify-key-sequence " "))))

;; other comint options
(custom-set-variables
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output t) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 '(comint-completion-autolist t)     ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; insert space/slash after file completion
 '(comint-buffer-maximum-size 5000)    ; max length of the buffer in lines
 '(comint-prompt-read-only nil)         ; if this is t, it breaks shell-command
 '(comint-get-old-input-default (lambda () "")) ; what to run when i press enter on a
                                        ; line above the current prompt
 '(comint-input-ring-size 5000)         ; max shell history size
 '(protect-buffer-bury-p nil)
 )

; interpret and use ansi color codes in shell output windows
 (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; ;; make shell output read only - Messes with matlab-shell :()
;; (defvar my-shells
;;     '("*shell0*" "*shell1*" "*MATLAB*"))
;; (defun make-my-shell-output-read-only (text)
;;   "Add to comint-output-filter-functions to make stdout read only in my shells."
;;   (if (member (buffer-name) my-shells)
;;       (let ((inhibit-read-only t)
;;             (output-end (process-mark (get-buffer-process (current-buffer)))))
;;         (put-text-property comint-last-output-start output-end 'read-only t))))
;; (add-hook 'comint-output-filter-functions 'make-my-shell-output-read-only)

;; truncate buffers continuously
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
;;;;;;;;;;;;;;;;;;;;;;;;;;; END COMINT STUFF ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

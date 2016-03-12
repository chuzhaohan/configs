;;; Commentary:
;; This contains my auctex/latex configuration
;; - Deepak Cherian

;;; Code:
(require 'auctex-latexmk)
(auctex-latexmk-setup)
(setq auctex-latexmk-inherit-TeX-PDF-mode t)

(setq-default TeX-PDF-mode t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

(setq TeX-source-correlate-method 'synctex)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/texlive/2014/bin/x86_64-linux/"))
(setq exec-path (append exec-path '("/usr/local/texlive/2014/bin/x86_64-linux/")))

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\(\\\\block\\)\\s-*{" 1 font-lock-warning-face t)))))

(setq TeX-output-view-style
      (quote
       (("^pdf$" "." "evince -f %o")
        ("^html?$" "." "iceweasel %o"))))

;; Following from:
;; https://stackoverflow.com/questions/3627574/emacs-changing-the-default-reftex-citation
(defvar reftex-last-citation nil)

(defadvice reftex-citation (after reftex-citation-and-remember-citation activate)
  "Save last citation to 'reftex-last-citation after running 'reftex-citation"
  (setq reftex-last-citation ad-return-value))

(defadvice reftex-get-bibkey-default (around reftex-just-return-last-citation activate)
  "If there is a 'reftex-last-citation then just return that instead of running 'reftex-get-bibkey-default"
  (if reftex-last-citation
      (setq ad-return-value reftex-last-citation)
    ad-do-it))

(provide 'dc-latex)
;;; dc-latex.el ends here

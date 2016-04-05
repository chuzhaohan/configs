;;; Commentary:
;; This contains my org-mode configuration
;; - Deepak Cherian

;;; Code:
;; org-mode class for my latex style
(require 'ox-latex)

(setq org-latex-listings t)
(setq org-startup-indented t)

(setq org-ellipsis "⤵")

;; org mode todo faces
;;(setq org-todo-keyword-faces
;;      '(("TODO" . org-warning) ("STARTED" . "yellow")
;;        ("CANCELED" . (:foreground "blue" :weight bold))
;;        ("DONE" . "green")))

;; org mode keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)
(setq org-catch-invisible-edits 'show)
(setq org-list-allow-alphabetical t)
(setq org-export-in-background nil)
(setq org-latex-prefer-user-labels t)
(setq org-latex-tables-booktabs t)

(defun headline-numbering-filter (data backend info)
  "No numbering in headlines that have a property :numbers: no"
  (let* ((beg (next-property-change 0 data))
         (headline (if beg (get-text-property beg :parent data))))
    (if (and (eq backend 'latex)
             (string= (org-element-property :NUMBERS headline) "no"))
        (replace-regexp-in-string
         "\\(part\\|chapter\\|\\(?:sub\\)*section\\|\\(?:sub\\)?paragraph\\)"
         "\\1*" data nil nil 1)
      data)))
(setq org-export-filter-headline-functions '(headline-numbering-filter))

;; Originally taken from Bruno Tavernier: http://thread.gmane.org/gmane.emacs.orgmode/31150/focus=31432
;; but adapted to use latexmk 4.20 or higher.
;; (defun my-auto-tex-cmd ()
;;   "When exporting from .org with latex, automatically run latex,
;;      pdflatex, or xelatex as appropriate, using latexmk."
;;   (let ((texcmd)))
;;   ;; default command: oldstyle latex via dvi
;;   (setq texcmd "latexmk -dvi -pdfps -quiet %f")
;;   ;; pdflatex -> .pdf
;;   (if (string-match "LATEX_CMD: pdflatex" (buffer-string))
;;       (setq texcmd "latexmk -pdf  -f %f"))
;;   ;; xelatex -> .pdf
;;   (if (string-match "LATEX_CMD: xelatex" (buffer-string))
;;       (setq texcmd "latexmk -pdflatex=\"xelatex -fmt=/media/data/Work/tools/latex/preamble.fmt\" -pdf %f"))
;;   ;; LaTeX compilation command
;;   (setq org-latex-pdf-process '(list texcmd)))

;;(setq texcmd "latexmk -pdflatex=\"xelatex -fmt=/media/data/Work/tools/latex/preamble.fmt\" -pdf %f")
;;(setq org-latex-to-pdf-process (list texcmd))

;;(add-hook 'org-export-latex-before-parsing-hook 'my-auto-tex-cmd)
(setq org-latex-create-formula-image-program 'dvipng)

(setq org-file-apps
      '((auto-mode . emacs)
       ("\\.pdf\\'" . "evince %s")))

;; org-latex-pdf-process is for org > 8.0
;; remove blank lines - so that I can use format statement in
;; #+LATEX_HEADER. set jobname so that it opens the pdf. %b command
;; found in ox-latex.el
(setq org-latex-pdf-process
      '("export BSTINPUTS=/usr/local/texlive/2014/texmf-dist/bibtex/bst/elsarticle/; tail -n +2 %f | sed '/./,$!d' > %f.nolines; mv %f.nolines %f; latexmk -gg %f; exiftool -overwrite_original -Producer=`git log -n 1 --pretty=%H` %b.pdf"))

;;(setq org-latex-pdf-process
;;'("PATH=/usr/local/texlive/2014/bin/x86_64-linux/:$PATH latexmk -pdflatex=xelatex ;;-jobname=%b -gg -pdf %f; exiftool -overwrite_original -Producer=`git log -n 1 ;;--pretty=%H` %b.pdf"))

(defun my-auto-tex-parameters ()
  "Automatically select the tex packages to include."
  ;; default packages for ordinary latex or pdflatex export
  (setq org-latex-default-packages-alist
        '((""     "inputenx" t)
          ("T1"   "fontenc"   t)
          ("sc"   "mathpazo"  t)
          (""     "fixltx2e"  nil)
          (""     "wrapfig"   nil)
          (""     "soul"      t)
          (""     "xfrac"     t)
          (""     "bigints"   t)
          (""     "textcomp"  t)
          (""     "marvosym"  t)
          (""     "wasysym"   t)
          (""     "latexsym"  t)
          (""     "mathtools" t)
          (""     "amssymb"   t)
          ("tight-spacing=true"     "siunitx"   nil)
          (""     "cleveref"  nil)
          (""     "graphicx"  nil)
          (""     "natbib"    nil)
          (""     "hyperref"  nil)))

  ;; Packages to include when xelatex is used
  (if (string-match "LATEX_CMD: xelatex" (buffer-string))
      (setq org-latex-default-packages-alist
            '(("" "fontspec" t)
              ("" "unicode-math" t)))))

;;(add-hook 'org-export-preprocess-before-selecting-backend-code-hook 'my-auto-tex-parameters)

;; my customized preamble
(add-to-list 'org-latex-classes
             '("dcarticle"
               "[NO-DEFAULT-PACKAGES]"
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                ("\\paragraph{%s}" . "\\paragraph*{%s}")
                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("ametsoc"
               "[NO-DEFAULT-PACKAGES]
\\documentclass{ametsoc}
\\usepackage{fixltx2e}
\\usepackage[mathletters]{ucs}
\\usepackage[utf8x]{inputenx}
\\usepackage[T1]{fontenc}
\\usepackage{array}
\\usepackage{booktabs}
\\usepackage{multirow}
\\usepackage{longtable}
\\usepackage{subfig}
\\usepackage{float}
\\usepackage[normalem]{ulem}
\\usepackage{etoolbox}
\\usepackage{parskip}
\\usepackage{paralist}
\\usepackage{mathtools}
\\usepackage{siunitx}
\\usepackage{xfrac}
\\usepackage{bigints}
\\usepackage[protrusion=true]{microtype}
\\sisetup{detect-all = true, separate-uncertainty = true, list-units=single, range-phrase = -- }
\\bibpunct{(}{)}{;}{a}{}{,}
[EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
               ))

(add-to-list 'org-latex-classes
             '("dcbeamer"
               "[NO-DEFAULT-PACKAGES]"
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
(provide 'dc-org)
;;; dc-org ends here

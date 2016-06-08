;;; Commentary:
;; This contains my org-mode configuration
;; - Deepak Cherian

;;; Code:
;; org-mode class for my latex style
(add-to-list 'load-path "~/.emacs.d/personal/orgmode/")
(require 'org)

(require 'ox-latex)

;; org mode keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-latex-listings t)
(setq org-startup-indented t)
(setq org-ellipsis "⤵")
(setq org-log-done t)
(setq org-catch-invisible-edits 'show)
(setq org-list-allow-alphabetical t)
(setq org-export-in-background nil)
(setq org-latex-prefer-user-labels t)
(setq org-latex-tables-booktabs t)
(setq org-hide-emphasis-markers t)
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(custom-set-faces
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.15))))
 )

(require 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)

(require 'org-ref)
(setq org-ref-notes-directory "~/Papers/notes/"
      org-ref-bibliography-notes "~/org/papers.org"
      org-ref-default-bibliography '("~/Papers/library.bib")
      org-ref-pdf-directory "~/Papers/")

(require 'helm-bibtex)
(setq bibtex-completion-bibliography "~/Papers/library.bib"
      bibtex-completion-library-path "~/Papers/"
      bibtex-completion-notes-path "~/org/papers.org")

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
(setq org-preview-latex-default-process 'dvipng)

(setq org-capture-templates
      '(("a"               ; key
         "Article"         ; name
         entry             ; type
         (file+headline "~/org/papers.org" "Article")  ; target
         "* %^{Title} %(org-set-tags)  :article: \n:PROPERTIES:\n:Created: %U\n:Linked: %a\n:INTERLEAVE_PDF:\n:END:\n%i\nBrief description:\n%?"  ; template
         :prepend t        ; properties
         :empty-lines 1    ; properties
         :created t        ; properties
         )
))

(setq org-file-apps
      '((auto-mode . emacs)
       ("\\.pdf\\'" . "evince %s")))

;; org-latex-pdf-process is for org > 8.0
;; remove blank lines - so that I can use format statement in
;; #+LATEX_HEADER. set jobname so that it opens the pdf. %b command
;; found in ox-latex.el
(setq org-latex-pdf-process
      '("export BSTINPUTS=/usr/local/texlive/2014/texmf-dist/bibtex/bst/elsarticle/; tail -n +3 %f | sed '/./,$!d' > %f.nolines; mv %f.nolines %f; latexmk %f; exiftool -overwrite_original -Producer=`git log -n 1 --pretty=%H` %b.pdf"))

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
             '("dcthesis"
               "[NO-DEFAULT-PACKAGES]"
               ("\\chapter{%s}" . "\\chapter*{%s}")
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

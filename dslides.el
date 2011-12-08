;; Copyright (C) 2011, Damien Cassou 

;; Permission is hereby granted, free of charge, to any person obtaining a 
;; copy of this software and associated documentation files (the "Software"), 
;; to deal in the Software without restriction, including without limitation 
;; the rights to use, copy, modify, merge, publish, distribute, sublicense, 
;; and/or sell copies of the Software, and to permit persons to whom the 
;; Software is furnished to do so, subject to the following conditions: 

;; The above copyright notice and this permission notice shall be included in 
;; all copies or substantial portions of the Software. 

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL 
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
;; DEALINGS IN THE SOFTWARE.

(defun build-slide ()
  (interactive)
  (with-current-buffer (current-buffer)
    (save-excursion
      (re-search-backward "^(slide")
      (mark-sexp)
      (kill-ring-save (region-beginning) (region-end))
      (kill-ring-save (point-min) (point-max))
      (with-temp-file "one-slide.tmp"
	(yank)
	(insert "(deck-reset)")
	(yank 2))
      (with-temp-file "compile.tmp"
	(insert "(load \"dslides.lisp\")(load \"one-slide.tmp\")(deck-real-output)"))
      (if (file-exists-p "slides-output.tex")
	  (delete-file  "slides-output.tex" nil))
      (when (get-buffer "*dslides-output*")
	(with-current-buffer "*dslides-output*"
	  (erase-buffer)))
      (call-process inferior-lisp-program
		    "compile.tmp" "*dslides-output*" nil)
      (let ((file-size (car (nthcdr 7 (file-attributes "slides-output.tex")))))
	(when (or (null file-size) (zerop file-size))
	  (error "slides-output.tex is empty, something went wrong with ibcl")))
      (call-process "pdflatex" nil "*dslides-output*" nil "clos.tex"))))

(defadvice message (around inhibit-message (format-string &rest args)
			   activate disable)
  (if (string-equal format-string "DocView: process %s changed status to %s.")
      (setq format-string ""))
    ad-do-it)

(defun show-slide ()
  (interactive)
  (build-slide)
  (let ((directory (file-name-directory (buffer-file-name))))
    (other-window 1)
    (ad-enable-advice 'message 'around 'inhibit-message)
    (ad-activate 'message)
    (let ((revert-without-query '("clos\.pdf")))
      (find-file (expand-file-name (concat directory "clos.pdf"))))
    (doc-view-first-page)
    (ad-disable-advice 'message 'around 'inhibit-message)
    (ad-activate 'message)
    (doc-view-fit-page-to-window)))

(define-key (eval 'slime-mode-map) (kbd "C-c v") #'show-slide)

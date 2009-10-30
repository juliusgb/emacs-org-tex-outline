;;; org-export-latex-outline.el - Produces a pdf outline using the latex outline
;; package of the notes written in emacs org-mode.
;;
;; Copyright (C) 2009 Julius Gamanyi
;;
;; Author: Julius Gamanyi <julius (dot) gb (at) googlemail (dot) com>
;; Keywords: latex outline pdf org-mode
;;
;; This file is not part of GNU Emacs.
;;
;; org-export-latex-outline.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; org-export-latex-outline.el is distributed in the hope that it will be
;; useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; org-export-latex-outline.el - Produces a pdf outline of the notes written in
;; emacs org-mode using the latex outline package.
;;
;; The latex outline package allows only 4 levels of depth and will have the
;; form:
;;
;; I. First main point
;;  A. First major sub-point of I.
;;    1. Sup-point of A.
;;      a. sub-point of 1.
;;
;;  B. Second major sub-point.
;;    1. Sub-point of B.
;;
;; II. Second main point.
;;
;;
;;; Current state:
;;
;; Please note that at the moment, this works with org-6.10a. I wanted to have a
;; working version before porting it to the current version of org-mode.
;;
;; Now that it's working, let the porting begin!
;;
;;
;;; Download:
;;
;;  $ http://github.com/juliusgb/emacs-org-tex-outline.git
;;
;;; Installation:
;;
;;  1. Save org-export-latex-outline.el in the /path/to/org-mode/lisp/
;;     directory.
;;  2. Apply the patches to both org-exp.el, org-install.el and
;;     org-export-latex.el.
;;  3. Re-start emacs.
;;  4. Add '#+LATEX_CLASS: outline' to the top of the org file you wish to export
;;     as an outline.
;;
;;The interactive function can be called by
;;
;; M-x `org-export-as-latex-outline'
;;
;; The code is pre-alpha. Any suggestions / contributions warmly welcomed.
;;
;;; Code:

(eval-when-compile
  (require 'cl))

(require 'org)
(require 'org-exp)
(require 'org-latex)

(defvar org-export-latex-outline-class "outline")
(defvar org-export-latex-outline-header nil)
(setq org-export-with-toc nil) ;; don't create table of contents
(setq org-export-headline-levels 4)
(defvar org-export-latex-packages-alist nil)

;; The next line contains the magic autoload comment
;;;###autoload
(defun org-export-as-latex-outline (arg &optional hidden ext-plist
                                        to-buffer body-only pub-dir)
  (interactive "P")

  ;; Make sure we have a file name when we need it.
  (when (and (not (or to-buffer body-only))
             (not buffer-file-name))
    (if (buffer-base-buffer)
        (org-set-local 'buffer-file-name
                       (with-current-buffer (buffer-base-buffer)
                         buffer-file-name))
      (error "Need a file name to be able to export")))

  (message "Exporting to LaTeX Outline...")
  (org-unmodified
   (remove-text-properties (point-min) (point-max)
         '(:org-license-to-kill nil)))
  (org-update-radio-target-regexp)
  (org-export-latex-set-initial-vars ext-plist arg)
  (let* ((wcf (current-window-configuration))
         (opt-plist org-export-latex-options-plist)
         (region-p (org-region-active-p))
         (rbeg (and region-p (region-beginning)))
         (rend (and region-p (region-end)))
         (subtree-p
    (if (plist-get opt-plist :ignore-subree-p)
        nil
      (when region-p
        (save-excursion
    (goto-char rbeg)
    (and (org-at-heading-p)
         (>= (org-end-of-subtree t t) rend))))))
         (opt-plist (setq org-export-opt-plist
        (if subtree-p
            (org-export-add-subtree-options opt-plist rbeg)
          opt-plist)))
         ;; Make sure the variable contains the updated values.
         (org-export-latex-options-plist opt-plist)
   (title (or (and subtree-p (org-export-get-title-from-subtree))
        (plist-get opt-plist :title)
        (and (not
        (plist-get opt-plist :skip-before-1st-heading))
       (org-export-grab-title-from-buffer))
        (file-name-sans-extension
         (file-name-nondirectory buffer-file-name))))
   (filename (concat (file-name-as-directory
          (or pub-dir
        (org-export-directory :LaTeX ext-plist)))
         (file-name-sans-extension
          (or (and subtree-p
             (org-entry-get rbeg "EXPORT_FILE_NAME" t))
        (file-name-nondirectory ;sans-extension
         buffer-file-name)))
         ".tex"))         
         (filename (if (equal (file-truename filename)
                              (file-truename buffer-file-name))
                       (concat filename ".tex")
                     filename))
         (buffer (if to-buffer
                     (cond
                      ((eq to-buffer 'string) (get-buffer-create
                                               "*Org LaTeX Outline Export*"))
                      (t (get-buffer-create to-buffer)))
                   (find-file-noselect filename)))
         (odd org-odd-levels-only)
         (header (org-export-latex-make-header title opt-plist))
         (skip (cond (subtree-p nil)
                     (region-p t)
                     ;; never skip first lines when exporting a subtree
                     (t (plist-get opt-plist :skip-before-1st-heading))))
         (text (plist-get opt-plist :text))
   (org-export-preprocess-hook
    (cons
     `(lambda () (org-set-local 'org-complex-heading-regexp
              ,org-export-latex-complex-heading-re))
     org-export-preprocess-hook))
   (first-lines (if skip "" (org-export-latex-first-lines
           opt-plist
           (if subtree-p
               (save-excursion
           (goto-char rbeg)
           (point-at-bol 2))
             rbeg)
           (if region-p rend))))
         (coding-system (and (boundp 'buffer-file-coding-system)
                             buffer-file-coding-system))
         (coding-system-for-write (or org-export-latex-coding-system
                                      coding-system))
         (save-buffer-coding-system (or org-export-latex-coding-system
                                        coding-system))
         (region (buffer-substring
                  (if region-p (region-beginning) (point-min))
                  (if region-p (region-end) (point-max))))
         (string-for-export
          (org-export-preprocess-string
           region :emph-multiline t
           :for-LaTeX t
           :comments nil
           :add-text (if (eq to-buffer 'string) nil text)
           :skip-before-1st-heading skip
           :select-tags (plist-get opt-plist :select-tags)
           :exclude-tags (plist-get opt-plist :exclude-tags)
           :LaTeX-fragments nil)))

    (set-buffer buffer)
    (erase-buffer)

    (and (fboundp 'set-buffer-file-coding-system)
         (set-buffer-file-coding-system coding-system-for-write))

    ;; insert the header and initial document commands
    (unless (or (eq to-buffer 'string) body-only)
      (insert header))

    ;; insert text found in #+TEXT
    (when (and text (not (eq to-buffer 'string)))
      (insert (org-export-latex-content
               text '(lists tables fixed-width keywords))
              "\n\n"))

    ;; insert lines before the first headline
    (unless (or skip (eq to-buffer 'string))
      (insert first-lines))

    ;; handle the case where the region does not begin with a section
    (when region-p
      (insert (with-temp-buffer
                (insert string-for-export)
                (org-export-latex-first-lines))))

    ;; export the content of headlines
    (org-export-latex-global
     (with-temp-buffer
       (insert string-for-export)
       (goto-char (point-min))
       (when (re-search-forward "^\\(\\*+\\) " nil t)
         (let* ((asters (length (match-string 1)))
                (level (if odd (- asters 2) (- asters 1))))
           (setq org-export-latex-add-level
                 (if odd (1- (/ (1+ asters) 2)) (1- asters)))
           (org-export-latex-parse-global level odd)))))

    ;; finalization
    (unless body-only
      (insert "\n\\end{outline}")
      (insert "\n\\end{document}"))
    (or to-buffer (save-buffer))
    (goto-char (point-min))
    (message "Exporting to LaTeX Outline...done")
    (prog1
        (if (eq to-buffer 'string)
            (prog1 (buffer-substring (point-min) (point-max))
              (kill-buffer (current-buffer)))
          (current-buffer))
      (set-window-configuration wcf)))
  (org-export-as-pdf-and-open arg))


(defun org-export-latex-make-header (title opt-plist)
  "Make the LaTeX header and return it as a string.
TITLE is the current title from the buffer or region.
OPT-PLIST is the options plist for current buffer."
  (let ((toc (plist-get opt-plist :table-of-contents))
        (author (plist-get opt-plist :author)))
    (concat
     (if (plist-get opt-plist :time-stamp-file)
         (format-time-string "% Created %Y-%m-%d %a %H:%M\n"))
     ;; insert LaTeX custom header
     (org-export-apply-macros-in-string org-export-latex-header)
     "\n"
     ;; insert information on LaTeX packages
     (when org-export-latex-packages-alist
       (mapconcat (lambda(p)
                    (if (equal "" (car p))
                        (format "\\usepackage{%s}" (cadr p))
                      (format "\\usepackage[%s]{%s}"
                              (car p) (cadr p))))
                  org-export-latex-packages-alist "\n"))
     ;; insert additional commands in the header
     (org-export-apply-macros-in-string
      (plist-get opt-plist :latex-header-extra))
     (org-export-apply-macros-in-string org-export-latex-append-header)
     ;; insert the title
     (format
      "\n\n\\title{%s}\n"
      ;; convert the title
      (org-export-latex-content
       title '(lists tables fixed-width keywords)))
     ;; insert author info
     (if (plist-get opt-plist :author-info)
   (format "\\author{%s}\n"
     (org-export-latex-fontify-headline (or author user-full-name)));????????????????????
       (format "%%\\author{%s}\n"
         (or author user-full-name)))
     ;; insert the date
     (format "\\date{%s}\n"
             (format-time-string
              (or (plist-get opt-plist :date)
                  org-export-latex-date-format)))
     ;; beginning of the document
     "\n\\begin{document}\n\n"
     ;; insert the title command
     (when (string-match "\\S-" title)
       (if (string-match "%s" org-export-latex-title-command)
     (format org-export-latex-title-command title)
   org-export-latex-title-command))
     "\n\n"
     ;; no table of contents
     ;; beginning of the outline
     "\\begin{outline}[enumerate]\n\n")))


;(defun reset-variables ()
(setq org-export-with-toc t)
(setq org-export-headline-levels 4)


(provide 'org-export-latex-outline)

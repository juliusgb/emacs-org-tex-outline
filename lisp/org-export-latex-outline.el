;;; org-export-latex-outline.el - Produces a pdf outline using the latex outline
;; package of the notes written in emacs org-mode.
;;
;; Copyright (C) 2009 Julius Gamanyi
;;
;; Author: Julius Gamanyi <julius.gb (at) googlemail (dot) com>
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
;; Please note that at the moment, when the outline export command is executed,
;; it just prints "hello world" in the minibuffer. Maybe others can develop it
;; further.
;;
;;; Download:
;;
;;   $ http://github.com/juliusgb/emacs-org-tex-outline.git
;;
;;; Installation:
;;
;;   1. Save org-export-latex-outline.el in the /path/to/org-mode/lisp/
;;      directory.
;;   2. Apply the email-formatted patch to both org-exp.el and org-install.el.
;;   3. Re-start emacs.
;;
;; The interactive function can be called by
;;
;; M-x `org-export-latex-as-outline'
;;
;;; Notes:
;;
;; The difficult part is exporting the body. Unlike the latex article class,
;; which automatically numbers the sections of the notes written in org-mode
;; based on the number of stars, the latex outline package requires you to
;; manually number your outline (maybe I'm missing something or I'm using an
;; unsuitable latex package).
;;
;; The current solution is to transfer that manual numbering to myself as I
;; write the outline, resulting in an org file of this format:
;;
;;   * I. Main Point 1.
;;     ** A. Sub-point of 1.
;;       *** 1. Sub-sub-point of 1.
;;       *** 2. Sub-sub-point of 2.
;;     ** B. Sub-point of 1.
;;   * II. Main Point 2.
;;     ** A. Sub-point of 2.
;;   * III. Main Point 3.
;;
;; Talk about ugly and the inconvenience when you move the subtrees.
;;
;; It would be ideal if:
;;  (1) the numbering was done automatically based on the number of stars on
;;      each heading,
;;  (2) we had the option to customise our own numbering at the top of the
;;      org file just as we can customise whether to include the date or table
;;      of contents at the top an org file that's to be exported to latex.
;;
;; The code is pre-pre-alpha. Any suggestions / contributions warmly welcomed.
;;
;;; Code:

(eval-when-compile
  (require 'cl))

(require 'org)
(require 'org-exp)
(require 'org-export-latex)

;; The next line contains the magic autoload comment
;;;###autoload
(defun org-export-as-latex-outline (arg)
  (interactive "P")
  (message "hello world."))


(provide 'org-export-latex-outline)

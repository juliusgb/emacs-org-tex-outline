Overview
=========

Produces a pdf outline of the notes written in emacs org-mode using the latex
outline package.

The latex outline package allows only 4 levels of depth and will have the form:

  I. First main point
    A. First major sub-point of I.
      1. Sup-point of A.
        a. sub-point of 1.
          * Another sub-sub-point.

    B. Second major sub-point.
      1. Sub-point of B.

  II. Second main point.

Any level deeper than 4 is shown as a bullet point.


Current state:
---------------
  It works with the current release of org-mode, that is, org-6.33f.

Download:
=========

 Click on the download tab, and choose your preferred compressed format.

Manual Installation:
--------------------

  1. Uncompress the contents of the .zip or .tar.gz into a directory of your
     choice.
     
  2. If you haven't modified these org-mode files (org-6.33f/lisp/org-exp.el,
     org-6.33f/lisp/org-install.el, and org-6.33f/lisp/org-latex.el) then
     copy all the files in the /path/to/uncompressed-download/lisp/ directory to
     /path/to/your/org-mode/lisp/ directory.

  3. If you've modified the org-mode files, check the patches directory and
     apply them to org-exp.el, org-install.el and org-latex.el.

  4. Re-start emacs.

Usage:
-------

  1. Add '#+LATEX_CLASS: outline' to the top of the org file you wish to export
     as an outline.

  2. Type C-c C-E to bring up the export menu.

  3. Type letter 'o', which exports your outline to pdf and opens it.

The interactive function can be called by

M-x `org-export-latex-as-outline'

The code is beta. Any suggestions / contributions warmly welcomed.

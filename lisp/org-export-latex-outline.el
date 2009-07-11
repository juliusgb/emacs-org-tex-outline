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

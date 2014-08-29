;; jmay's private commands

(defun upcase-region-or-word (numwords)
  "Upcase a region if selected, otherwise the next word (or words if prefix argument is provided)."
  (interactive "p")
  (if (region-active-p)
      (upcase-region (region-beginning) (region-end))
    (upcase-word numwords)))

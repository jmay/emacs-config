;;; 92_org.el --- special config for org mode features
;;
;;; Commentary:
;;
;; Utility functions for use in org mode, particularly org-babel stuff
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; install sql (includes postgresql) support for org-babel
;; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-sql.html
;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)
   (sh . t)
   (python . t)))
;; add additional languages with '((language . t)))

(defun babel-confirm (flag)
  "Report the setting of org-confirm-babel-evaluate.
If invoked with C-u, toggle the setting"
  (interactive "P")
  (if (equal flag '(4))
      (setq org-confirm-babel-evaluate (not org-confirm-babel-evaluate)))
  (message "Babel evaluation confirmation is %s"
           (if org-confirm-babel-evaluate "on" "off")))

;; (org-add-link-type
;;  "yt"
;;  (lambda (handle)
;;    (browse-url (concat "https://www.youtube.com/embed/" handle)))
;;  (lambda (path desc backend)
;;    (cl-case backend
;;      ;; You may want to change your width and height.
;;      (html (format "<iframe width=\"440\" height=\"335\" src=\"https://www.youtube.com/embed/%s\" frameborder=\"0\" allowfullscreen>%s</iframe>"
;;                    path (or desc "")))
;;      (latex (format "\href{%s}{%s}" path (or desc "video"))))))

(use-package org-download
  :ensure t)

;; Turn off ispell checking inside org mode source blocks
;; NOT WORKING ON 150824
;; http://endlessparentheses.com/ispell-and-org-mode.html
;; (defun endless/org-ispell ()
;;   "Configure `ispell-skip-region-alist' for `org-mode'."
;;   (make-local-variable 'ispell-skip-region-alist)
;;   (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
;;   (add-to-list 'ispell-skip-region-alist '("~" "~"))
;;   (add-to-list 'ispell-skip-region-alist '("=" "="))
;;   (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))
;; (add-hook 'org-mode-hook #'endless/org-ispell)

(setq org-imenu-depth 4)


;; http://pragmaticemacs.com/emacs/wrap-text-in-an-org-mode-block/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function to wrap blocks of text in org templates                       ;;
;; e.g. latex or src etc                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-begin-template ()
  "Make a template at point."
  (interactive)
  (if (org-at-table-p)
      (call-interactively 'org-table-rotate-recalc-marks)
    (let* ((choices '(("s" . "SRC")
                      ("e" . "EXAMPLE")
                      ("q" . "QUOTE")
                      ("v" . "VERSE")
                      ("c" . "CENTER")
                      ("l" . "LaTeX")
                      ("h" . "HTML")
                      ("a" . "ASCII")))
           (key
            (key-description
             (vector
              (read-key
               (concat (propertize "Template type: " 'face 'minibuffer-prompt)
                       (mapconcat (lambda (choice)
                                    (concat (propertize (car choice) 'face 'font-lock-type-face)
                                            ": "
                                            (cdr choice)))
                                  choices
                                  ", ")))))))
      (let ((result (assoc key choices)))
        (when result
          (let ((choice (cdr result)))
            (cond
             ((region-active-p)
              (let ((start (region-beginning))
                    (end (region-end)))
                (goto-char end)
                (insert "#+END_" choice "\n")
                (goto-char start)
                (insert "#+BEGIN_" choice "\n")))
             (t
              (insert "#+BEGIN_" choice "\n")
              (save-excursion (insert "#+END_" choice))))))))))


;; http://endlessparentheses.com/changing-the-org-mode-ellipsis.html
(setq org-ellipsis "⤵")

;;; 92_org.el ends here

;;; 92_org.el --- special config for org mode features
;;
;;; Commentary:
;;
;; Org-babel stuff
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; install sql (includes postgresql) support for org-babel
;; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-sql.html
;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)
   (sh . t)))
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

;;; 92_org.el ends here

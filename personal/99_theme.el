;;; 99_theme.el --- my personal theme setup
;;
;;; Commentary:
;;
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Systems.html
;; window-system ns is Nextstep (MacOSX)
;;
;;; Code:

(if (eq (window-system) 'ns)
    (progn
     (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
     (load-theme 'jmay t)
     ))

;; http://pragmaticemacs.com/emacs/change-text-size/
;;use larger font
(setq default-frame-alist '((font . "Source Code Pro-14")))

;; set-frame-font

;; (defadvice color-theme-alist (around sacha activate)
;;   (if (ad-get-arg 0)
;;       ad-do-it
;;     nil))
;; (use-package color-theme :ensure color-theme)
;; (use-package color-theme-solarized :ensure color-theme-solarized)
;; (defun sacha/setup-color-theme ()
;;   (interactive)
;;   (color-theme-solarized 'dark)
;;   (set-face-foreground 'secondary-selection "darkblue")
;;   (set-face-background 'secondary-selection "lightblue")
;;   (set-face-background 'font-lock-doc-face "black")
;;   (set-face-foreground 'font-lock-doc-face "wheat")
;;   (set-face-background 'font-lock-string-face "black")
;;   (set-face-foreground 'org-todo "green")
;;   (set-face-background 'org-todo "black"))
;;
;; (use-package color-theme
;;   :init
;;   (when window-system
;;     (sacha/setup-color-theme)))

;;; 99_theme.el ends here

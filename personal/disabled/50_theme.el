;;; theme.el --- my personal theme setup
;;
;; emacs can crash in load-theme when running in terminal mode (window-system is nil)

(cond ((eq (window-system) 'ns)
       (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
       (load-theme 'jmay t)

       ;; (load-theme 'manoj-dark t)
       ;; (load-theme 'zenburn t)                 ; low-contrast, background is grey
       ;; (load-theme 'Amelie t)
       ;; (load-theme 'spolsky t)
       ;; (load-theme 'firebelly t)

       (set-face-attribute 'default nil :family "Source Code Pro")
       (set-face-attribute 'default nil :weight 'light)
       (set-face-attribute 'default nil :height 140)
       )))

;;; jmay.el --- my personal emacs configuration; works with Prelude
;;
;;; License:
;; This work is licensed under a Creative Commons Attribution-ShareAlike 3.0 Unported License.
;; URL: http://creativecommons.org/licenses/by-sa/3.0/deed.en_US

;; Display theme

(load-theme 'manoj-dark t)

;; (load-theme 'zenburn t)                 ; low-contrast, background is grey

(set-face-attribute 'default nil :family "Source Code Pro")
(set-face-attribute 'default nil :weight 'light)
(set-face-attribute 'default nil :height 140)

;;(setq-default line-spacing 0)

;; I want a single etags file so that I can jump between projects and gems
;; MAYBE NOT...
;; (setq tags-table-list '("/Users/jmay/dev/TAGS"))

;; My packages
(setq prelude-packages (append '(
                                 auto-complete
                                 dash-at-point
                                 motion-mode
                                 deft
                                 minimap
                                 ) prelude-packages))

;; Install my packages
(prelude-install-packages)

;; my global key mappings

;; Dash
(global-set-key (read-kbd-macro "C-c m") 'dash-at-point)
(add-to-list 'dash-at-point-mode-alist '(perl-mode . "perl"))


;; navigation
(setq prelude-guru nil)

(global-set-key (read-kbd-macro "s-<down>") 'scroll-up-command)
(global-set-key (read-kbd-macro "s-<up>") 'scroll-down-command)

(setq whitespace-line-column 100)

;; use markdown-mode for .txt files

(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))

;;; jmay.el ends here

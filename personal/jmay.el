(set-face-attribute 'default nil :family "Source Code Pro")
(set-face-attribute 'default nil :weight 'light)
(set-face-attribute 'default nil :height 140)
;;(setq-default line-spacing 0)

(load-theme 'manoj-dark t)

;; I want a single etags file so that I can jump between projects and gems
(setq tags-table-list
      '("/Users/jmay/dev/TAGS"))

;; My packages
(setq prelude-packages (append '(
                                 dash-at-point
                                 motion-mode
                                 deft
                                 ) prelude-packages))

;; Install my packages
(prelude-install-packages)

;; my global key mappings

;; Dash
(global-set-key (read-kbd-macro "C-c m") 'dash-at-point)

;; multiple-cursors
(require 'multiple-cursors)
;; When you have an active region that spans multiple lines, the following will add a cursor to each line:
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; When you want to add multiple cursors not based on continuous lines, but based on keywords in the buffer, use:
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; navigation
(setq prelude-guru nil)

(global-set-key (read-kbd-macro "s-<down>") 'scroll-up-command)
(global-set-key (read-kbd-macro "s-<up>") 'scroll-down-command)

(setq whitespace-line-column 100)

;; use markdown-mode for .txt files

(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))
(setq org-export-with-smart-quotes t)

(add-hook 'ruby-mode-hook 'fci-mode)
(add-hook 'ruby-mode-hook 'linum-mode)

;; using deft with org-mode

(when (require 'deft nil 'noerror)
  (setq
   deft-extension "org"
   deft-directory "~/.deft"
   deft-text-mode 'org-mode)
  (global-set-key (kbd "<f9>") 'deft))

;;; jmay.el ends here

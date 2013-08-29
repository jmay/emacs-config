;;; bindings.el --- my personal keyboard bindings
;;
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html
;; "Sequences consisting of C-c and a letter (either upper or lower case) are reserved for users"
;; For multi-modifier combinations, write the modifiers in alphabetical order (C-H-M-s-S)
;;
;; Emacs understands Shift (S-), Control (C-), Meta (M-), Super (s-), Hyper (H-)
;; control key is Control
;; both shift keys are Shift
;; escape key is Meta
;;
;; OSX fn key is Hyper
;;
;; I prefer the following:
;; option/alt key is Hyper
;; command key is Super
;;
;; http://ergoemacs.org/emacs/keyboard_shortcuts.html

(setq mac-command-modifier 'super)
(setq mac-option-modifier 'hyper)

;; command-z is Undo; make sure that command-shift-z is Redo
(global-set-key (kbd "s-Z") 'undo-tree-redo)

;; Dash
(global-set-key (read-kbd-macro "C-c m") 'dash-at-point)
(add-to-list 'dash-at-point-mode-alist '(perl-mode . "perl"))


;; navigation
(global-set-key (read-kbd-macro "s-<down>") 'scroll-up-command)
(global-set-key (read-kbd-macro "s-<up>") 'scroll-down-command)


;; programming
(global-set-key (kbd "C-c \\") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "s-/") 'comment-or-uncomment-region-or-line)

;; todo, notes, journal
(global-set-key (kbd "C-c j") 'org-capture)
(global-set-key (kbd "C-c c") 'org-capture)

(global-set-key (kbd "C-c b") 'browse-url-at-point)

;; notes on other bindings
;;
;; windmove package sets shift-arrow bindings to navigate between windows,
;; so use Hyper key (Alt/Option) for selecting regions.

(global-set-key (kbd "H-<right>") 'forward-word)
(global-set-key (kbd "H-<left>") 'backward-word)
(global-set-key (kbd "H-<up>") 'previous-line)
(global-set-key (kbd "H-<down>") 'next-line)

(global-set-key (kbd "C-c =") 'align-to-equals)

(global-set-key (kbd "C-c y") 'browse-kill-ring)


;; I don't like these bindings; turn them off
(global-unset-key (kbd "s-n"))        ; C-x 5 2 also does this
(global-unset-key (kbd "C-\\"))       ; don't need emacs input methods

;; Up for consideration
;; swap isearch-forward C-s and isearch-forward-regexp C-M-s
;; ditto backward C-r, C-M-R

;;; bindings.el ends here

;; (defun my-unindent-region (N)
;;   (interactive "p")
;;   (if mark-active
;;       (progn (indent-rigidly (min (mark) (point)) (max (mark) (point)) (* N -2))
;;              (setq deactivate-mark nil))
;;     (self-insert-command N)))

;; (global-set-key (kbd "<backtab>") 'my-unindent-region)

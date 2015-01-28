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

;;; Code:
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
(global-set-key (read-kbd-macro "<next>") 'forward-paragraph)   ;; fn-down
(global-set-key (read-kbd-macro "<prior>") 'backward-paragraph) ;; fn-up

(key-chord-define-global "jj" nil) ;; disable this because I use jj in ruby
(key-chord-define-global "jw" 'ace-jump-word-mode)

;; http://sachachua.com/blog/2014/12/emacs-kaizen-ace-jump-zap-lets-use-c-u-zap-character/
(require 'use-package)
(use-package ace-jump-zap
             :ensure ace-jump-zap
             :bind
             (("M-z" . ace-jump-zap-up-to-char-dwim)
              ("C-M-z" . ace-jump-zap-to-char-dwim)))

;; programming
(global-set-key (kbd "C-c \\") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "s-/") 'comment-or-uncomment-region-or-line)

;; todo, notes, journal
(global-set-key (kbd "C-c j") 'org-capture)
(global-set-key (kbd "C-c c") 'org-capture)

(global-set-key (kbd "C-c b") 'browse-url-at-point)

(global-set-key (kbd "s-C") 'my-copy-simple)

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
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-%") 'query-replace)


;;; bindings.el ends here

;; (defun my-unindent-region (N)
;;   (interactive "p")
;;   (if mark-active
;;       (progn (indent-rigidly (min (mark) (point)) (max (mark) (point)) (* N -2))
;;              (setq deactivate-mark nil))
;;     (self-insert-command N)))
;; (global-set-key (kbd "<backtab>") 'my-unindent-region)

(global-set-key (kbd "C-*") 'mc/mark-all-like-this)
(global-set-key (kbd "H-)") 'mc/mark-next-like-this)

(add-hook 'markdown-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c v") 'markdown-preview-file)
            )
          )

(global-set-key (kbd "s-<right>") 'org-indent-item)
(global-set-key (kbd "s-<left>") 'org-outdent-item)

;; (global-set-key (kbd "C-c C-s") 'helm-spotify)

(add-hook 'ruby-mode-hook
          (lambda ()
            (define-key ruby-mode-map (kbd "C-c C-c") 'xmp)
            (define-key ruby-mode-map (kbd "C-c M-l") 'ruby-reload-and-go)
            (define-key ruby-mode-map (kbd "C-\\") 'comment-or-uncomment-region-or-line)
;;            (define-key rspec-mode-keymap (kbd "s") 'rspec-verify-single)
            ))
;; ruby-refactor adds C-c C-r keymap with {e, v, c, p, l}


;; easy-kill
(global-set-key [remap kill-ring-save] 'easy-kill)

(global-set-key (kbd "M-u") 'upcase-region-or-word)

;; other candidate keys for rebinding
;; C-z (default is suspend-frame)

(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "M-p") 'ace-window)

(global-set-key (kbd "C-c q") 'compact-uncompact-block)


(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "C-c M-l") 'org-mac-grab-link)))

(global-set-key (kbd "C-c C-b") 'bundle-console)

(global-set-key (kbd "C-c M-q") 'toggle-fill-unfill)

;; navigating between buffers
;; windmove moves the cursor; buf-move swaps entire buffers
(global-set-key (kbd "<A-left>")   'windmove-left)
(global-set-key (kbd "<A-right>")  'windmove-right)
(global-set-key (kbd "<A-up>")  'windmove-up)
(global-set-key (kbd "<A-down>")  'windmove-down)

(global-set-key (kbd "<A-H-left>")   'buf-move-left)
(global-set-key (kbd "<A-H-right>")  'buf-move-right)


;; http://oremacs.com/2015/01/14/repeatable-commands/
;; http://oremacs.com/2015/01/20/introducing-hydra/

;; (global-set-key (kbd "<f2> ]")
;;                 (def-rep-command
;;                   '(("]" . text-scale-increase)
;;                     ("[" . text-scale-decrease))))
;; (global-set-key (kbd "<f2> [")
;;                 (def-rep-command
;;                   '(("[" . text-scale-decrease)
;;                     ("]" . text-scale-increase))))

(use-package hydra
  :ensure t
  :config
  (progn
    (hydra-create "<f2>"
                  '(("]" text-scale-increase)
                    ("[" text-scale-decrease)))
    )
  )

;;; 90_bindings.el ends here

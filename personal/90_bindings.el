;;; bindings.el --- my personal keyboard bindings
;;
;;; Commentary:
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation

(global-set-key (read-kbd-macro "s-<down>") 'scroll-up-command)
(global-set-key (read-kbd-macro "s-<up>") 'scroll-down-command)
(global-set-key (read-kbd-macro "<next>") 'forward-paragraph)   ;; fn-down
(global-set-key (read-kbd-macro "<prior>") 'backward-paragraph) ;; fn-up

(key-chord-define-global "jj" nil) ;; disable this because I use jj in ruby
;; (key-chord-define-global "jw" 'ace-jump-word-mode)
;; (key-chord-define-global "yy" 'ace-jump-word-mode)
;; switching from ace-jump to avy (included with ace-window)
(key-chord-define-global "jw" 'avy-goto-word-1)
(key-chord-define-global "yy" 'avy-goto-word-1)

;; http://sachachua.com/blog/2014/12/emacs-kaizen-ace-jump-zap-lets-use-c-u-zap-character/
(require 'use-package)
(use-package ace-jump-zap
             :ensure ace-jump-zap
             :bind
             (("M-z" . ace-jump-zap-up-to-char-dwim)
              ("C-M-z" . ace-jump-zap-to-char-dwim)))

;; avy-goto-line supports letter-combo shortcuts and line numbers
;; http://oremacs.com/2015/05/17/avy-goto-line/

(global-set-key (kbd "M-g g") 'avy-goto-line)
(global-set-key (kbd "s-l") 'avy-goto-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; switch-window displays big numbers in each window when there are
;; more than 2. Turned this off because I'm using arrow navigation
;; with windmove (see below)
;;
;;(global-set-key (kbd "C-x o") 'switch-window)

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
    (defhydra hydra-zoom (global-map "<f2>")
      "zoom"
      ("]" text-scale-increase "in")
      ("[" text-scale-decrease "out")
      )

    (defhydra hydra-window (:color amaranth)
      "window"
      ;; navigation
      ("h" windmove-left)
      ("j" windmove-down)
      ("k" windmove-up)
      ("l" windmove-right)
      ;; create new window and navigate to i
      ("v" (lambda ()
             (interactive)
             (split-window-right)
             (windmove-right))
       "vert")
      ("x" (lambda ()
             (interactive)
             (split-window-below)
             (windmove-down))
       "horz")
      ;; ("t" transpose-frame "'")
      ("o" delete-other-windows "one" :color blue)
      ("a" ace-window "ace")
      ("s" ace-swap-window "swap")
      ("d" ace-delete-window "del")
      ("i" ace-maximize-window "ace-one" :color blue)
      ("b" ido-switch-buffer "buf")
      ;; ("m" headlong-bookmark-jump "bmk")
      ("q" nil "cancel")
      )

    (global-set-key (kbd "<f8>") 'hydra-window/body)
    )
  )

(use-package reveal-in-finder
  :ensure t
  :config
  (progn
    ;; make this the default binding for C-c C-o
    ;; org-mode changes behavior when point is on a link
    (global-set-key (kbd "C-c C-o") 'reveal-in-finder)
    )
  )

;; Default binding for toggle-frame-fullscreen is <f11>
;; But OSX reserves f11 for hide-apps-show-desktop
;; I don't want to steal the OS setting, so use left-command-f11.
(global-set-key (kbd "s-<f11>") 'toggle-frame-fullscreen)

(global-set-key (kbd "s-(") 'insert-parentheses)
(global-set-key (kbd "s-9") 'insert-parentheses)

;;; 90_bindings.el ends here

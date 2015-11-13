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
(setq mac-right-command-modifier 'alt)


;; http://emacsformacosx.com/tips
;; The easiest way is to use the Customize interface: "M-x customize-group RET ns RET".
;; ns-alternate-modifier
;; ns-command-modifier
;; ns-control-modifier
;; ns-function-modifier
;; ns-option-modifier (just a different name for ns-alternate-modifier)
;; ns-right-alternate-modifier
;; ns-right-command-modifier
;; ns-right-control-modifier
;; ns-right-option-modifier


;; ;; command-z is Undo; make sure that command-shift-z is Redo
;; (global-set-key (kbd "s-Z") 'undo-tree-redo)
;;
;; ;; Dash
;; (global-set-key (read-kbd-macro "C-c m") 'dash-at-point)
;; (add-to-list 'dash-at-point-mode-alist '(perl-mode . "perl"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Navigation

;; (global-set-key (read-kbd-macro "s-<down>") 'scroll-up-command)
;; (global-set-key (read-kbd-macro "s-<up>") 'scroll-down-command)
;; (global-set-key (read-kbd-macro "<next>") 'forward-paragraph)   ;; fn-down
;; (global-set-key (read-kbd-macro "<prior>") 'backward-paragraph) ;; fn-up

;; (key-chord-define-global "jj" nil) ;; disable this because I use jj in ruby
;; ;; (key-chord-define-global "jw" 'ace-jump-word-mode)
;; ;; (key-chord-define-global "yy" 'ace-jump-word-mode)
;; ;; switching from ace-jump to avy (included with ace-window)
;; (key-chord-define-global "jw" 'avy-goto-word-1)
;; (key-chord-define-global "yy" 'avy-goto-word-1)

;; http://sachachua.com/blog/2014/12/emacs-kaizen-ace-jump-zap-lets-use-c-u-zap-character/
(require 'use-package)
(use-package ace-jump-zap
             :ensure ace-jump-zap
             :bind
             (("M-z" . ace-jump-zap-up-to-char-dwim)
              ("C-M-z" . ace-jump-zap-to-char-dwim)))

;; avy-goto-line supports letter-combo shortcuts and line numbers
;; http://oremacs.com/2015/05/17/avy-goto-line/

;; (global-set-key (kbd "M-g g") 'avy-goto-line)
;; (global-set-key (kbd "s-l") 'avy-goto-line)

;; ;; http://endlessparentheses.com/improving-page-navigation.html
;; (define-key prog-mode-map "\C-x\C-n" #'forward-page)
;; (define-key prog-mode-map "\C-x\C-p" #'backward-page)
;;
;; (setq page-delimiter
;;       (rx bol ";;;" (not (any "#")) (* not-newline) "\n"
;;           (* (* blank) (opt ";" (* not-newline)) "\n")))
;;
;; ;; right-command+w to select current word/sentence/para/etc for action
;; (global-set-key (kbd "A-w") 'er/expand-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; programming
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

;; (global-set-key (kbd "H-<right>") 'forward-word)
;; (global-set-key (kbd "H-<left>") 'backward-word)
;; (global-set-key (kbd "H-<up>") 'previous-line)
;; (global-set-key (kbd "H-<down>") 'next-line)

;; (global-set-key (kbd "C-c =") 'align-to-equals)
;;
;; (global-set-key (kbd "C-c y") 'browse-kill-ring)
;;
;;
;; ;; I don't like these bindings; turn them off
;; (global-unset-key (kbd "s-n"))        ; C-x 5 2 also does this
;; (global-unset-key (kbd "C-\\"))       ; don't need emacs input methods

;; ;; Up for consideration
;; ;; swap isearch-forward C-s and isearch-forward-regexp C-M-s
;; ;; ditto backward C-r, C-M-R
;; (global-set-key (kbd "C-s") 'isearch-forward-regexp)
;; (global-set-key (kbd "C-r") 'isearch-backward-regexp)
;; (global-set-key (kbd "C-M-s") 'isearch-forward)
;; (global-set-key (kbd "C-M-r") 'isearch-backward)
;; (global-set-key (kbd "M-%") 'query-replace-regexp)
;; (global-set-key (kbd "C-M-%") 'query-replace)


;;; bindings.el ends here

;; (defun my-unindent-region (N)
;;   (interactive "p")
;;   (if mark-active
;;       (progn (indent-rigidly (min (mark) (point)) (max (mark) (point)) (* N -2))
;;              (setq deactivate-mark nil))
;;     (self-insert-command N)))
;; (global-set-key (kbd "<backtab>") 'my-unindent-region)

;; (global-set-key (kbd "C-*") 'mc/mark-all-like-this)
;; (global-set-key (kbd "H-)") 'mc/mark-next-like-this)

;; (add-hook 'markdown-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-c v") 'markdown-preview-file)
;;             )
;;           )

;; (global-set-key (kbd "s-<right>") 'org-indent-item)
;; (global-set-key (kbd "s-<left>") 'org-outdent-item)

;; (global-set-key (kbd "C-c C-s") 'helm-spotify)

;; (add-hook 'ruby-mode-hook
;;           (lambda ()
;;             (define-key ruby-mode-map (kbd "C-c C-c") 'xmp)
;;             (define-key ruby-mode-map (kbd "C-c M-l") 'ruby-reload-and-go)
;;             (define-key ruby-mode-map (kbd "C-\\") 'comment-or-uncomment-region-or-line)
;; ;;            (define-key rspec-mode-keymap (kbd "s") 'rspec-verify-single)
;;             ))
;; ;; ruby-refactor adds C-c C-r keymap with {e, v, c, p, l}
;;
;;
;; ;; easy-kill - this is already defined by prelude
;; ;; (global-set-key [remap kill-ring-save] 'easy-kill)
;;
;; (global-set-key (kbd "M-u") 'upcase-region-or-word)

;; other candidate keys for rebinding
;; C-z (default is suspend-frame)

;; switch-window displays big numbers in each window when there are
;; more than 2. Turned this off because I'm using arrow navigation
;; with windmove (see below)
;;
;;(global-set-key (kbd "C-x o") 'switch-window)

;; (global-set-key (kbd "M-p") 'ace-window)
;;
;; (global-set-key (kbd "C-c q") 'compact-uncompact-block)


;; (use-package org-mac-link
;;   :ensure t
;;   )
;;
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (define-key org-mode-map (kbd "C-c M-l") 'org-mac-grab-link)
;;             (define-key org-mode-map (kbd "s-,") 'org-begin-template);; Command-, (no shift needed, not <)
;;             )
;;           )

;; org-begin-template is defined in 75_org.el


;; (global-set-key (kbd "C-c C-b") 'bundle-console)
;;
;; (global-set-key (kbd "C-c M-q") 'toggle-fill-unfill)

;; ;; navigating between buffers
;; ;; windmove moves the cursor; buf-move swaps entire buffers
;; (global-set-key (kbd "<A-left>")   'windmove-left)
;; (global-set-key (kbd "<A-right>")  'windmove-right)
;; (global-set-key (kbd "<A-up>")  'windmove-up)
;; (global-set-key (kbd "<A-down>")  'windmove-down)
;;
;; (global-set-key (kbd "<A-H-left>")   'buf-move-left)
;; (global-set-key (kbd "<A-H-right>")  'buf-move-right)

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


;; http://kitchingroup.cheme.cmu.edu/blog/2015/09/27/Upping-my-Emacs-navigation-game
;; Upping my Emacs navigation game

(defhydra hydra-navigate (:color red
                          :hint nil)
  "
_f_: forward-char       _w_: forward-word       _n_: next-line
_b_: backward-char      _W_: backward-word      _p_: previous-line
^ ^                     _o_: subword-right      _,_: beginning-of-line
^ ^                     _O_: subword-left       _._: end-of-line

_s_: forward sentence   _a_: forward paragraph  _g_: forward page
_S_: backward sentence  _A_: backward paragraph _G_: backward page

_h_: helm mini _B_: buffer list _i_: window
_<left>_: previous buffer   _<right>_: next buffer
_<up>_: scroll-up           _<down>_: scroll-down

_[_: backward-sexp _]_: forward-sexp
_<_ beginning of buffer _>_ end of buffer _m_: set mark _/_: jump to mark
"
  ("f" forward-char)
  ("b" backward-char)
  ("w" forward-word)
  ("W" backward-word)
  ("n" next-line)
  ("p" previous-line)
  ("o" subword-right)
  ("O" subword-left)
  ("s" forward-sentence)
  ("S" backward-sentence)
  ("a" forward-paragraph)
  ("A" backward-paragraph)
  ("g" forward-page)
  ("G" backward-page)
  ("<right>" next-buffer)
  ("<left>" previous-buffer)
  ("h" helm-mini :color blue)
  ("i" ace-window :color blue)
  ("m" org-mark-ring-push)
  ("/" org-mark-ring-goto :color blue)
  ("B" helm-buffers-list)
  ("<up>" scroll-up)
  ("<down>" scroll-down)
  ("<" beginning-of-buffer)
  (">" end-of-buffer)
  ("." end-of-line)
  ("[" backward-sexp)
  ("]" forward-sexp)
  ("," beginning-of-line)
  ("q" nil "quit" :color blue))

(global-set-key (kbd "s-n") 'hydra-navigate/body)
(global-set-key (kbd "A-n") 'hydra-navigate/body)



;; http://kitchingroup.cheme.cmu.edu/blog/2015/09/28/A-cursor-goto-hydra-for-emacs
;; A cursor goto hydra for emacs

(defhydra goto (:color blue :hint nil)
  "
Goto:
^Char^              ^Word^                ^org^                    ^search^
^^^^^^^^---------------------------------------------------------------------------
_c_: 2 chars        _w_: word by char     _h_: headline in buffer  _o_: helm-occur
_C_: char           _W_: some word        _a_: heading in agenda
_L_: char in line   _s_: subword by char  _q_: swoop org buffers   _f_: search forward
^  ^                _S_: some subword     ^ ^                      _b_: search backward
-----------------------------------------------------------------------------------
_B_: helm-buffers       _l_: avy-goto-line
_m_: helm-mini          _i_: ace-window
_R_: helm-recentf

_n_: Navigate           _._: mark position _/_: jump to mark
"
  ("c" avy-goto-char-2)
  ("C" avy-goto-char)
  ("L" avy-goto-char-in-line)
  ("w" avy-goto-word-1)
  ;; jump to beginning of some word
  ("W" avy-goto-word-0)
  ;; jump to subword starting with a char
  ("s" avy-goto-subword-1)
  ;; jump to some subword
  ("S" avy-goto-subword-0)

  ("l" avy-goto-line)
  ("i" ace-window)

  ("h" helm-org-headlines)
  ("a" helm-org-agenda-files-headings)
  ("q" helm-multi-swoop-org)

  ("o" helm-occur)
;;  ("p" swiper-helm)

  ("f" isearch-forward)
  ("b" isearch-backward)

  ("." org-mark-ring-push :color red)
  ("/" org-mark-ring-goto :color blue)
  ("B" helm-buffers-list)
  ("m" helm-mini)
  ("R" helm-recentf)
  ("n" hydra-navigate/body)
  )

(global-set-key (kbd "s-g") 'goto/body)
(global-set-key (kbd "A-g") 'goto/body)

;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple-cursors

(use-package multiple-cursors
  :ensure t
  :config
  (progn
    ;; When you have an active region that spans multiple lines, the following will add a cursor to each line:
    (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

    ;; When you want to add multiple cursors not based on continuous lines, but based on keywords in the buffer, use:
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-*") 'mc/mark-all-like-this)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

;; (define-key my-keys-minor-mode-map (kbd "C-i") 'some-function)


;; (org mode only) key binding to insert today's date at point
(defun org-insert-today-date ()
  "Insert today's date in org date format."
  (interactive)
  (org-insert-time-stamp (current-time))
  )

(use-package org
  :ensure t
  :config
  (define-key org-mode-map (kbd "C-c <") 'org-insert-today-date)
  )

(global-unset-key (kbd "C-x p"))

;;; 90_bindings.el ends here

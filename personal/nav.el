;; My personal keyboard bindings
;;
;; Emacs understands Shift (S-), Control (C-), Meta (M-), Super (s-), Hyper (H-)
;; control key is Control
;; both shift keys are Shift
;; escape key is Meta
;; option/alt key is Super
;; command key is Hyper
;;
;; For multi-modifier combinations, write the modifiers in alphabetical order (C-H-M-s-S)
;;
;; http://ergoemacs.org/emacs/keyboard_shortcuts.html

(setq mac-command-modifier 'super)
(setq mac-option-modifier 'hyper)

;; command-z is Undo; make sure that command-shift-z is Redo
(global-set-key (kbd "s-Z") 'undo-tree-redo)

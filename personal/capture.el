;; configuration for org mode, todo & journal capture
;;
;; Reference:
;;
;; https://www.gnu.org/software/emacs/manual/html_node/org/Template-expansion.html
;; http://members.optusnet.com.au/~charles57/GTD/datetree.html
;; https://www.gnu.org/software/emacs/manual/html_node/org/Conflicts.html

(setq org-export-with-smart-quotes t)

;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(setq org-default-notes-file "~/.deft/notes.org")

(setq org-capture-templates
      '(
        ("t"
         "TODO"
         entry
         (file+headline "~/.deft/gtd.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("o"
         "Otherbase TODO"
         entry
         (file+headline "~/.deft/otherbase-todo.org" "Tasks")
         "* TODO %?\n  %i\n")
        ("t"
         "Veriphyr TODO"
         entry
         (file+headline "~/.deft/veriphyr-todo.org" "Tasks")
         "* TODO %?\n  %i\n")
        ("e"
         "Emacs TODO"
         entry
         (file+headline "~/.deft/emacs-todo.org" "Emacs TODO")
         "* TODO %?\n  %i\n")
        ("j"
         "Journal"
         entry
         (file+datetree "~/.deft/journal.org")
         "* %?\n\n")
        )
      )

;; using deft with org-mode

(when (require 'deft nil 'noerror)
  (setq
;;   deft-extension "org"
   deft-directory "~/.deft"
   deft-text-mode 'org-mode
   deft-auto-save-interval 0
  )
  (global-set-key (kbd "<f9>") 'deft))

;; 130718 currently I don't like the deft auto-save because it triggers whitespace-cleanup
;; couldn't turn it off once Deft has been loaded, needed to do this:
;; (cancel-function-timers 'deft-auto-save)

;; http://www.emacswiki.org/emacs/AutoFillMode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

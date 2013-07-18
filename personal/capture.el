;; configuration for org mode, todo & journal capture
;;
;; Reference:
;;
;; https://www.gnu.org/software/emacs/manual/html_node/org/Template-expansion.html
;; http://members.optusnet.com.au/~charles57/GTD/datetree.html

(setq org-export-with-smart-quotes t)

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
         "* TODO %?\n  %i\n  %a")
        ("t"
         "Veriphyr TODO"
         entry
         (file+headline "~/.deft/veriphyr-todo.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
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
   deft-extension "org"
   deft-directory "~/.deft"
   deft-text-mode 'org-mode
   deft-auto-save-interval 30.0
  )
  (global-set-key (kbd "<f9>") 'deft))

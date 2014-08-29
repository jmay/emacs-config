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
        ("v"
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
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; for MobileOrg
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-directory "~/Dropbox/Apps/MobileOrg")


(defun my-org-export-change-options (plist backend)
  (cond
   ((equal backend 'html)
    (plist-put plist :with-toc nil)
    (plist-put plist :section-numbers t))
   ((equal backend 'md)
    (plist-put plist :with-toc nil)
    (plist-put plist :section-numbers nil)))
  plist)

;; (add-to-list 'org-export-filter-options-functions 'my-org-export-change-options)


;; idea for how to completely conceal/reveal the PROPERTIES blocks
;; https://stackoverflow.com/questions/17478260/completely-hide-the-properties-drawer-in-org-mode
;; (defun lawlist-org-cycle-hide-drawers (state)
;;   "Re-hide all drawers after a visibility state change."
;;   (when (and (derived-mode-p 'org-mode)
;;        (not (memq state '(overview folded contents))))
;;     (save-excursion
;;       (let* ((globalp (memq state '(contents all)))
;;              (beg (if globalp (point-min) (point)))
;;              (end (if globalp (point-max)
;;         (if (eq state 'children)
;;       (save-excursion (outline-next-heading) (point))
;;           (org-end-of-subtree t)))))
;;   (goto-char beg)
;;   (while (re-search-forward "^.*DEADLINE:.*$\\|^\\*\\* Someday.*$\\|^\\*\\* None.*$\\|^\\*\\* Planning.*$\\|^\\* TASKS.*$" end t)
;;      (save-excursion
;;     (beginning-of-line 1)
;;     (when (looking-at "^.*DEADLINE:.*$\\|^\\*\\* Someday.*$\\|^\\*\\* None.*$\\|^\\*\\* Planning.*$\\|^\\* TASKS.*$")
;;       (let ((b (match-end 0)))
;;   (if (re-search-forward
;;        "^[ \t]*:END:"
;;        (save-excursion (outline-next-heading) (point)) t)
;;       (outline-flag-region b (point-at-eol) t)
;;     (user-error ":END: line missing at position %s" b))))))))))

;; capture.el ends here

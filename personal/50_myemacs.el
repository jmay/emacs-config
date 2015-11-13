(require 'use-package)

(defun markdown-preview-file ()
  "Run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command
   (format "open -a /Applications/Marked\\ 2.app %s"
           (shell-quote-argument (buffer-file-name))))
  )

;; show fill column
(add-hook 'prog-mode-hook 'fci-mode)

;; show line numbers
(setq linum-format "%4d ")
(add-hook 'prog-mode-hook 'linum-mode)
;; scss-mode (for CSS, SASS) is not derived from prog-mode
(add-hook 'scss-mode-hook 'linum-mode)

(setq comment-empty-lines t)

(defun align-to-equals (begin end)
  "Align region to equal signs"
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\)=" 1 1 ))

;; http://www.emacswiki.org/emacs/AutoIndentation
(add-hook 'prog-mode-hook '(lambda ()
                             (local-set-key (kbd "RET") 'newline-and-indent)))

(add-hook 'prog-mode-hook
          (lambda ()
            (when buffer-file-name
              (add-hook 'after-save-hook
                        'check-parens
                        nil t))))

;; http://emacsredux.com/blog/2013/07/24/highlight-comment-annotations/
;; TODO consider alternatives at http://www.emacswiki.org/emacs/FixmeMode

(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\)\b"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

(require 'auto-complete-config)
(ac-config-default)
(define-key ac-mode-map (kbd "C-c ,") 'auto-complete)

(use-package ruby-hash-syntax
  :ensure t
)

(add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)

;; inferior ruby
(defun ruby-reload-and-go ()
  "Send current buffer file to the inferior Ruby.
Then switch to the process buffer."
  (interactive)
  (comint-check-source buffer-file-name) ; Check to see if buffer needs saved.
  (comint-send-string (inf-ruby-proc) (concat "(load \""
                                              (buffer-file-name)
                                              "\"\)\n"))
  (ruby-switch-to-inf t))

(add-hook 'projectile-mode-hook 'projectile-rails-on)

(use-package sql-indent
  :ensure t
)

(use-package buffer-move
  :ensure t
  :bind (
         ("C-H-<left>" . buf-move-left)
         ("C-H-<right>" . buf-move-right)
         )
  )

(defun upcase-region-or-word (numwords)
  "Upcase a region if selected, otherwise the next word (or words if prefix argument is provided)."
  (interactive "p")
  (if (region-active-p)
      (upcase-region (region-beginning) (region-end))
    (upcase-word numwords)))

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (forward-line)))

(defun compact-uncompact-block ()
  "Remove or add line ending chars on current paragraph.
This command is similar to a toggle of `fill-paragraph'.
When there is a text selection, act on the region."
  (interactive)

  ;; This command symbol has a property “'stateIsCompact-p”.
  (let (currentStateIsCompact (bigFillColumnVal 90002000) (deactivate-mark nil))
    ;; 90002000 is just random. you can use `most-positive-fixnum'

    (save-excursion
      ;; Determine whether the text is currently compact.
      (setq currentStateIsCompact
            (if (eq last-command this-command)
                (get this-command 'stateIsCompact-p)
              (if (> (- (line-end-position) (line-beginning-position)) fill-column) t nil) ) )

      (if (region-active-p)
          (if currentStateIsCompact
              (fill-region (region-beginning) (region-end))
            (let ((fill-column bigFillColumnVal))
              (fill-region (region-beginning) (region-end))) )
        (if currentStateIsCompact
            (fill-paragraph nil)
          (let ((fill-column bigFillColumnVal))
            (fill-paragraph nil)) ) )

      (put this-command 'stateIsCompact-p (if currentStateIsCompact nil t)) ) ) )

(defun my-copy-simple (&optional beg end)
  "Save the current region (or line) to the `kill-ring' after stripping extra whitespace and new lines"
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((my-text (buffer-substring-no-properties beg end)))
    (with-temp-buffer
      (insert my-text)
      (goto-char 1)
      (while (looking-at "[ \t\n]")
        (delete-char 1))
      (let ((fill-column 9333999))
        (fill-region (point-min) (point-max)))
      (kill-region (point-min) (point-max)))))

;; bindings
(global-set-key (kbd "C-c q") 'compact-uncompact-block)
(global-set-key (kbd "s-C") 'my-copy-simple)

(use-package highlight-indentation
  :ensure t
  :config
  (progn
    (set-face-background 'highlight-indentation-face "#222")
    (set-face-background 'highlight-indentation-current-column-face "#444")
    (add-hook 'prog-mode-hook
              (lambda ()
                (highlight-indentation-mode)
                (highlight-indentation-current-column-mode)
                ))
    )
  )

(setq sql-connection-alist
      '((schoolistry (sql-product 'postgres)
                     (sql-port 5432)
                     (sql-server "localhost")
                     (sql-user "jmay")
                     (sql-password "")
                     (sql-database "schoolistry"))
        (schoolistry-prod (sql-product 'postgres)
                          (sql-port 5432)
                          (sql-server "remote")
                          (sql-user "remote")
                          (sql-password "remote")
                          (sql-database "postgres"))
;; using ssh port forwarding
;; requires password entry in buffer, but displays no prompt
        (veriphyr (sql-product 'postgres)
                  (sql-port 5100)
                  (sql-server "localhost")
                  (sql-user "jmay")
                  (sql-database "g1"))
        )
      )

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

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

;; (setq org-default-notes-file "~/.deft/notes.org")

;; force UTF-8
(setq org-export-coding-system 'utf-8)

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
         "* %?\n%i\n")
        ("s"
         "Schoolistry Journal"
         entry
         (file+datetree "~/.deft/journal.org")
         "* %? :schoolistry:\n%i\n")
        )
      )

;; using deft with org-mode

;; (when (require 'deft nil 'noerror)
;;   (setq
;; ;;   deft-extension "org"
;;    deft-directory "~/.deft"
;;    deft-text-mode 'org-mode
;;    deft-auto-save-interval 0
;;   )
;;   (global-set-key (kbd "<f9>") 'deft))

;; 130718 currently I don't like the deft auto-save because it triggers whitespace-cleanup
;; couldn't turn it off once Deft has been loaded, needed to do this:
;; (cancel-function-timers 'deft-auto-save)

;; http://www.emacswiki.org/emacs/AutoFillMode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)

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

;; fontify code in code blocks
(setq org-src-fontify-natively t)

(use-package org-bullets
  :ensure t
  :config
  (progn
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; install sql (includes postgresql) support for org-babel
;; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-sql.html
;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)
   (sh . t)
   (python . t)))
;; add additional languages with '((language . t)))

(defun babel-confirm (flag)
  "Report the setting of org-confirm-babel-evaluate.
If invoked with C-u, toggle the setting"
  (interactive "P")
  (if (equal flag '(4))
      (setq org-confirm-babel-evaluate (not org-confirm-babel-evaluate)))
  (message "Babel evaluation confirmation is %s"
           (if org-confirm-babel-evaluate "on" "off")))

;; (org-add-link-type
;;  "yt"
;;  (lambda (handle)
;;    (browse-url (concat "https://www.youtube.com/embed/" handle)))
;;  (lambda (path desc backend)
;;    (cl-case backend
;;      ;; You may want to change your width and height.
;;      (html (format "<iframe width=\"440\" height=\"335\" src=\"https://www.youtube.com/embed/%s\" frameborder=\"0\" allowfullscreen>%s</iframe>"
;;                    path (or desc "")))
;;      (latex (format "\href{%s}{%s}" path (or desc "video"))))))

(use-package org-download
  :ensure t)

;; Turn off ispell checking inside org mode source blocks
;; NOT WORKING ON 150824
;; http://endlessparentheses.com/ispell-and-org-mode.html
;; (defun endless/org-ispell ()
;;   "Configure `ispell-skip-region-alist' for `org-mode'."
;;   (make-local-variable 'ispell-skip-region-alist)
;;   (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
;;   (add-to-list 'ispell-skip-region-alist '("~" "~"))
;;   (add-to-list 'ispell-skip-region-alist '("=" "="))
;;   (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))
;; (add-hook 'org-mode-hook #'endless/org-ispell)

(setq org-imenu-depth 4)


;; http://pragmaticemacs.com/emacs/wrap-text-in-an-org-mode-block/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function to wrap blocks of text in org templates                       ;;
;; e.g. latex or src etc                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-begin-template ()
  "Make a template at point."
  (interactive)
  (if (org-at-table-p)
      (call-interactively 'org-table-rotate-recalc-marks)
    (let* ((choices '(("s" . "SRC")
                      ("e" . "EXAMPLE")
                      ("q" . "QUOTE")
                      ("v" . "VERSE")
                      ("c" . "CENTER")
                      ("l" . "LaTeX")
                      ("h" . "HTML")
                      ("a" . "ASCII")))
           (key
            (key-description
             (vector
              (read-key
               (concat (propertize "Template type: " 'face 'minibuffer-prompt)
                       (mapconcat (lambda (choice)
                                    (concat (propertize (car choice) 'face 'font-lock-type-face)
                                            ": "
                                            (cdr choice)))
                                  choices
                                  ", ")))))))
      (let ((result (assoc key choices)))
        (when result
          (let ((choice (cdr result)))
            (cond
             ((region-active-p)
              (let ((start (region-beginning))
                    (end (region-end)))
                (goto-char end)
                (insert "#+END_" choice "\n")
                (goto-char start)
                (insert "#+BEGIN_" choice "\n")))
             (t
              (insert "#+BEGIN_" choice "\n")
              (save-excursion (insert "#+END_" choice))))))))))


;; http://endlessparentheses.com/changing-the-org-mode-ellipsis.html
(setq org-ellipsis "⤵")

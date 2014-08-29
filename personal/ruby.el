;; ruby.el
;;
;; My personal extensions for Ruby programming

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (next-line)))

(setq comment-empty-lines t)

(eval-after-load 'ruby-mode
  '(define-key ruby-mode-map (kbd "C-\\") 'comment-or-uncomment-region-or-line))

(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

;; show fill column
(add-hook 'prog-mode-hook 'fci-mode)
;; show line numbers
(add-hook 'prog-mode-hook 'linum-mode)
(setq linum-format "%4d ")
;; highlight indentation
(add-hook 'prog-mode-hook 'highlight-indentation-mode)


;; rubymotion

(add-hook 'ruby-mode-hook 'motion-recognize-project)
(remove-hook 'ruby-mode-hook 'motion-recognize-project)
;;(add-to-list 'ac-modes 'motion-mode)
;;(add-to-list 'ac-sources 'ac-source-dictionary)


;; http://www.emacswiki.org/emacs/AutoIndentation
(add-hook 'prog-mode-hook '(lambda ()
                             (local-set-key (kbd "RET") 'newline-and-indent)))

;; check for matching parens *after* saving (so the work is saved even if there's an error)
(add-hook 'prog-mode-hook
          (lambda ()
            (when buffer-file-name
              (add-hook 'after-save-hook
                        'check-parens
                        nil t))))

(defun align-to-equals (begin end)
  "Align region to equal signs"
  (interactive "r")
  (align-regexp begin end "\\(\\s-*\\)=" 1 1 ))


;; http://emacsredux.com/blog/2013/07/24/highlight-comment-annotations/
;; TODO consider alternatives at http://www.emacswiki.org/emacs/FixmeMode

(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\)\b"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

;; autocompletion
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(require 'auto-complete-config)
(ac-config-default)
(define-key ac-mode-map (kbd "C-c ,") 'auto-complete)

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

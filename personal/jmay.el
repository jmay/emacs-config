;;; jmay.el --- my personal emacs configuration; works with Prelude
;;
;;; License:
;; This work is licensed under a Creative Commons Attribution-ShareAlike 3.0 Unported License.
;; URL: http://creativecommons.org/licenses/by-sa/3.0/deed.en_US
;;
;;; References:
;; https://dl.dropboxusercontent.com/u/3968124/sacha-emacs.html
;; http://doc.norang.ca/org-mode.html
;; http://ftp.gnu.org/old-gnu/Manuals/elisp-manual-20-2.5/html_chapter/elisp_41.html
;; http://daemianmack.com/magit-cheatsheet.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display theme

(load-theme 'manoj-dark t)

;; (load-theme 'zenburn t)                 ; low-contrast, background is grey

(set-face-attribute 'default nil :family "Source Code Pro")
(set-face-attribute 'default nil :weight 'light)
(set-face-attribute 'default nil :height 140)

;;(setq-default line-spacing 0)

;; I want a single etags file so that I can jump between projects and gems
;; MAYBE NOT...
;; (setq tags-table-list '("/Users/jmay/dev/TAGS"))

;; My packages

(setq prelude-packages (append '(
                                 auto-complete
                                 dash-at-point
                                 motion-mode
                                 deft
                                 minimap
                                 ;; http://www.emacswiki.org/emacs/BrowseKillRing
                                 browse-kill-ring
                                 yasnippet
                                 ;; https://github.com/Bruce-Connor/smart-mode-line
                                 smart-mode-line
                                 ;; to get markdown options to appear in org-mode
                                 ox-md
                                 ) prelude-packages))


;; (require 'ox-md)      ; to get markdown options to appear in org-mode

;; Install my packages
(prelude-install-packages)

(add-to-list 'package-archive-exclude-alist '(("melpa" org)))

(setq prelude-guru nil)                 ; enable keyboard arrow keys for navigation

(setq whitespace-line-column 100)

;; activate the smart mode line
(if after-init-time (sml/setup)
  (add-hook 'after-init-hook 'sml/setup))

;; use markdown-mode for .txt files

(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))

;; TODO additional packages to consider
;; http://sachachua.com/blog/2011/12/emacs-artbollocks-mode-el-and-writing-more-clearly/

;; http://ergoemacs.org/emacs/modernization_fill-paragraph.html
;; this is a toggling version of fill-paragraph/region (there is no built-in unfill command)

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

(global-set-key (kbd "C-c q") 'compact-uncompact-block)

;;; jmay.el ends here

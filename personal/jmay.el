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

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; (load-theme 'manoj-dark t)
(load-theme 'jmay t)

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
                                 ruby-refactor
                                 ;; to get markdown options to appear in org-mode
                                 easy-kill ;; https://github.com/leoliu/easy-kill
                                 hlinum ;; highlight current line number
                                 powerline ;; attractive mode line
                                 highlight-indentation
                                 ) prelude-packages))

;;(load "../helm-spotify/helm-spotify")

;; ox-md is not a package, it is in emacs core, but it must be
;; required to active the markdown export options in org-mode
(require 'ox-md)
;;(require 'emmet-mode)
(hlinum-activate)

;; https://github.com/milkypostman/powerline
;; activate powerline (mode line theme)
(powerline-default-theme)

;; (highlight-indentation-mode)
;; (set-face-background 'highlight-indentation-face "#202020")
;; (set-face-background 'highlight-indentation-current-column-face "#999999")

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

;; I don't want spell checking in emacs
(setq prelude-flyspell t)


;; http://emacsredux.com/blog/2013/05/18/instant-access-to-init-dot-el/
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))
(global-set-key (kbd "C-c I") 'find-user-init-file)


(require 'org-publish)
(setq org-publish-project-alist
      '(("html"
         :base-directory "~/.deft"
         :base-extension "org"
         :publishing-directory "~/Documents/exports"
         :publishing-function org-publish-org-to-html)
        ("pdf"
         :base-directory "~/.deft/"
         :base-extension "org"
         :publishing-directory "~/Documents/exports"
         :publishing-function org-publish-org-to-pdf)
        ("all" :components ("html" "pdf"))))


(require 'edit-server)
(edit-server-start)
;;; jmay.el ends here


;; default value of 'midnight-hook is 'clean-buffer-list
;; I do not want emacs to close all my buffers every night
(remove-hook 'midnight-hook 'clean-buffer-list)




(defface powerline-jmay1
  '((t (:inherit mode-line :foreground "#2FDE3A" :background "#222222")))
  "Jason's powerline face 1."
  :group 'powerline)

(defface powerline-jmay2
  '((t (:inherit mode-line :background "#333333" :foreground "#2FDE3A")))
  "Jason's powerline face 2."
  :group 'powerline)

(defface powerline-inactive1
  '((t (:background "grey11" :inherit mode-line-inactive)))
  "Powerline face 1."
  :group 'powerline)

(defface powerline-inactive2
  '((t (:background "grey20" :inherit mode-line-inactive)))
  "Powerline face 2."
  :group 'powerline)


(defun powerline-jmay-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-jmay1 'powerline-inactive1))
                          (face2 (if active 'powerline-jmay2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          powerline-default-separator
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           powerline-default-separator
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" face1 'l)
                                     (powerline-buffer-size face1 'l)
                                     (powerline-raw mode-line-mule-info face1 'l)
                                     (powerline-buffer-id face1 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format face1 'l))
                                     (powerline-raw " ")
                                     (funcall separator-left mode-line face1)
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-vc face2 'r)))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
                                     (powerline-raw "%4l" face1 'l)
                                     (powerline-raw ":" face1 'l)
                                     (powerline-raw "%3c" face1 'r)
                                     (funcall separator-right face1 mode-line)
                                     (powerline-raw " ")
                                     (powerline-raw "%6p" nil 'r)
                                     (powerline-hud face2 face1))))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

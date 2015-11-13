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

;;(setq-default line-spacing 0)

;; I want a single etags file so that I can jump between projects and gems
;; MAYBE NOT...
;; (setq tags-table-list '("/Users/jmay/dev/TAGS"))

(require 'use-package)

;; https://github.com/tarsius/auto-compile
;; (setq load-prefer-newer t)
;; (require 'auto-compile)
;; (auto-compile-on-load-mode 1)
;; (auto-compile-on-save-mode 1)
;; (setq auto-compile-display-buffer nil)
;; (setq auto-compile-mode-line-counter t)

;; My packages

;; (setq prelude-packages (append '(
;;                                  ;; auto-complete
;;                                  ;; dash-at-point
;;                                  ;; motion-mode
;;                                  ;; deft
;;                                  ;; minimap
;;                                  ;; http://www.emacswiki.org/emacs/BrowseKillRing
;;                                  ;; browse-kill-ring
;;                                  ;; yasnippet
;;                                  ;; https://github.com/Bruce-Connor/smart-mode-line
;;                                  ;; smart-mode-line
;;                                  ;; ruby-refactor
;;                                  ;; to get markdown options to appear in org-mode
;;                                  ;; easy-kill ;; https://github.com/leoliu/easy-kill
;;                                  ;; hlinum ;; highlight current line number
;; ;;                                 powerline ;; attractive mode line
;;                                  ;; highlight-indentation
;;                                  ;; minitest
;;                                  ;; switch-window
;;                                  ) prelude-packages))

;; (ido-vertical-mode 1)
;;(load "../helm-spotify/helm-spotify")


;; ox-md is not a package, it is in emacs core, but it must be
;; required to active the markdown export options in org-mode
;; rg(require 'ox-md)
;;(require 'Emmett-mode)
;; (hlinum-activate)

;; (require 'minitest)
;; (add-hook 'ruby-mode-hook 'minitest-mode)

;; Install my packages
;; (prelude-install-packages)

;; (add-to-list 'package-archive-exclude-alist '(("melpa" org)))

;; (setq prelude-guru nil)                 ; enable keyboard arrow keys for navigation
;;(guru-global-mode +1)
;;(guru-mode +1)

;; (setq whitespace-line-column 100)

;; activate the smart mode line
;;(if after-init-time (sml/setup)
;;  (add-hook 'after-init-hook 'sml/setup))

;; ;; use markdown-mode for .txt files
;; (add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))
;; ;; j2-mode for .es6 files
;; (add-to-list 'auto-mode-alist '("\\.es6\\'" . js2-mode))

;; TODO additional packages to consider
;; http://sachachua.com/blog/2011/12/emacs-artbollocks-mode-el-and-writing-more-clearly/

;; http://ergoemacs.org/emacs/modernization_fill-paragraph.html
;; this is a toggling version of fill-paragraph/region (there is no built-in unfill command)

;; I don't want spell checking in emacs
;;(setq prelude-flyspell t)


;; ;; http://emacsredux.com/blog/2013/05/18/instant-access-to-init-dot-el/
;; (defun find-user-init-file ()
;;   "Edit the `user-init-file', in another window."
;;   (interactive)
;;   (find-file-other-window user-init-file))
;; (global-set-key (kbd "C-c I") 'find-user-init-file)


;; (require 'edit-server)
;; (edit-server-start)

;; default value of 'midnight-hook is 'clean-buffer-list
;; I do not want emacs to close all my buffers every night
;; (remove-hook 'midnight-hook 'clean-buffer-list)

;; (custom-set-variables
;;  '(js2-basic-offset 2)
;;  '(js2-bounce-indent-p t)
;;  )

;; https://github.com/rranelli/emacs-dotfiles/blob/master/lisp/init-bootstrap.el

(setq jwm-initialization-errors ())

(defun jwm-safe-require (feature)
  "Safely requires FEATURE."
  (condition-case ex
      (require feature)
    ('error (add-to-list 'jwm-initialization-errors
			 (format "[ERROR LOADING \"%s\"]: %s" (symbol-name feature) ex)))))

(defun jwm-safe-load (file)
  "Safely loads FILE."
  (condition-case ex
      (load file)
    ('error (add-to-list 'jwm-initialization-errors
			 (format "[ERROR LOADING \"%s\"]: %s" file ex)))))

(defun jwm-safe-load-init-files ()
  (mapc 'jwm-safe-load (directory-files "./pieces" 't "^[^#]*.el$")))

;; (jwm-safe-load-init-files)

;; ;; emacs should ask before carrying out C-x C-c
;; (defvar confirm-kill-emacs 'yes-or-no-p)

;;; my _init.el ends here

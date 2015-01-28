;;; 05_dev.el --- my configuration for things related to development
;;
;;; Commentary:
;;
;; I want indentation highlighting in all programming modes.
;;
;;
;; http://sachachua.com/blog/2014/12/emacs-configuration-use-package/
;; http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html
;;
;;; Code:
(require 'use-package)

(require 'derived) ;; needed for highlight-indentation to load
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

;; http://sachachua.com/blog/2015/01/emacs-kaizen-ace-isearch-combines-ace-jump-mode-helm-swoop/
;; (use-package ace-isearch
;;   :ensure t
;;   )

;;; 05_dev.el ends here

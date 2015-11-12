;; interesting packages that I have disabled

;; Sublime Text-style minimap
;; disabled because it is slow and interrupts editing flow in the main area
;; such a tool should be seamless

;; http://www.emacswiki.org/emacs/Sublimity
;; (require 'sublimity)
;; (require 'sublimity-scroll)
;; (require 'sublimity-map)

;; appears to have been abandoned
;; some sort of CEDET support for ruby syntax
;; (require 'wisent-ruby)
;;
;; ideas: a "toggle" map for various switches; a "launcher" map for external apps
;; (define-prefix-command 'my/whatever-map)
;; (global-set-key (kbd "H-l") 'my/whatever-map)
;; (global-set-key (my/whatever-map "x" 'do-something)

;; (yas-global-mode 1)


;; (setq sql-connection-alist
;;       '((schoolistry (sql-product 'postgres)
;;                      (sql-port 5432)
;;                      (sql-server "localhost")
;;                      (sql-user "jmay")
;;                      (sql-password "")
;;                      (sql-database "schoolistry"))
;;         (schoolistry-prod (sql-product 'postgres)
;;                           (sql-port 5432)
;;                           (sql-server "remote")
;;                           (sql-user "remote")
;;                           (sql-password "remote")
;;                           (sql-database "postgres"))
;;         (veriphyr (sql-product 'postgres)
;;                   (sql-port 5432)
;;                   (sql-server "veriphyr-jmay-jv1.cxulj2mlappu.us-east-1.rds.amazonaws.com")
;;                   (sql-user "jmay")
;;                   (sql-database "g1"))
;;         )
;;       )
;;
;; (add-hook 'sql-interactive-mode-hook
;;           (lambda ()
;;             (toggle-truncate-lines t)))

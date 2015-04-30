;;; 80_projects.el
;;
;; Settings for my current projects

;; sql-postgres
;; https://truongtx.me/2014/08/23/setup-emacs-as-an-sql-database-client/

;; use sql-connect for these
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
        (veriphyr (sql-product 'postgres)
                          (sql-port 5432)
                          (sql-server "veriphyr-jmay-jv1.cxulj2mlappu.us-east-1.rds.amazonaws.com")
                          (sql-user "jmay")
                          (sql-database "g1"))
        )
      )

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

;;; 80_projects.el ends here

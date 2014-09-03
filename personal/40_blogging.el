;;; blogging.el --- settings for publishing to blogs
;;
;; https://github.com/punchagan/org2blog

(require 'org2blog-autoloads)

(setq org2blog/wp-blog-alist
      '(("jmay-blog"
         :url "http://jmay.wordpress.com/xmlrpc.php"
         :username "jmay"
         :default-title "Hello World"
         :default-categories ("org2blog" "emacs")
         :tags-as-categories nil)))
        ;; ("another-blog"
         ;; :url "http://jmay.wordpress.com/xmlrpc.php"
         ;; :username "jmay")))

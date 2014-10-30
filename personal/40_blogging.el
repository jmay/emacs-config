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


;; https://gist.github.com/xahlee/d364cbbff9b3abd12d29
                                        ; http://emacs.stackexchange.com/questions/1051/copy-region-from-emacs-without-newlines
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

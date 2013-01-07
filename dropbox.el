;; dropbox.el --- an emacs tramp backend for dropbox
;; Copyright 2011 Pavel Panchekha <pavpanchekha@gmail.com>
;;
;; Based on emacs-yammer (https://github.com/psanford/emacs-yammer/blob/master/yammer.el)


(add-to-list 'load-path "./oauth/")
(load "oauth.el")
(require 'tramp)

(defvar dropbox-consumer-key "iybkzcei2msvtfs")
(defvar dropbox-consumer-secret "h0i6bjickvpk0nt")

(defvar dropbox-request-url "https://api.dropbox.com/0/oauth/request_token")
(defvar dropbox-access-url "https://api.dropbox.com/0/oauth/access_token")
(defvar dropbox-authorization-url "https://www.dropbox.com/0/oauth/authorize")
(defvar dropbox-access-token nil)
(defvar dropbox-locale nil)

(defvar dropbox-token-file "~/.dropbox-token")

(defvar dropbox-api-content-host "api.dropbox.com")

(defun dropbox-authenticate (username)
  "Get authentication token for dropbox"
  (if (file-exists-p dropbox-token-file)
      (save-excursion
        (find-file dropbox-token-file)
        (let ((str (buffer-substring (point-min) (point-max))))
          (if (string-match "\\([^:]*\\):\\(.*\\)" str)
              (setq dropbox-access-token
                    (make-oauth-access-token
                     :consumer-key dropbox-consumer-key
                     :consumer-secret dropbox-consumer-secret
                     :auth-t (make-oauth-t
                              :token (match-string 1 str)
                              :token-secret (match-string 2 str))))))
        (save-buffer)
        (kill-this-buffer)))
  (unless dropbox-access-token ; Oh, we need to get a token
    (setq dropbox-access-token
          (oauth-authorize-app dropbox-consumer-key dropbox-consumer-secret
                               dropbox-request-url dropbox-access-url
                               dropbox-authorization-url))
    (save-excursion
      (find-file dropbox-token-file)
      (end-of-buffer)
      (let ((token (oauth-access-token-auth-t dropbox-access-token)))
        (insert (format "%s:%s\n"
                        (oauth-t-token token)
                        (oauth-t-token-secret token))))
      (save-buffer)
      (kill-this-buffer)))
  dropbox-access-token)

(defun dropbox-connect (email)
  (interactive "MEmail/Dropbox username: ")

  (let* ((token (dropbox-authenticate email))
         (part (oauth-access-token-auth-t token)))
    (setq dropbox-token (oauth-t-token part))
    (setq dropbox-token-2 (oauth-t-token-secret part)))

  (add-to-list 'tramp-methods
         `("db"  (tramp-login-program "python")
                 (tramp-login-args (("/Users/pavel/api/clients/tramp/dbsh.py") (,dropbox-token ,dropbox-token-2)))
                 (tramp-remote-sh "sh"))))

;; dropbox.el --- an emacs tramp backend for dropbox
;; Copyright 2011 Pavel Panchekha <pavpanchekha@gmail.com>
;;
;; Based on emacs-yammer (https://github.com/psanford/emacs-yammer/blob/master/yammer.el)


(require 'oauth)
(require 'json)
(load-file "dropbox-secrets.el")

(defvar dropbox-request-url       "https://api.dropbox.com/1/oauth/request_token")
(defvar dropbox-access-url        "https://api.dropbox.com/1/oauth/access_token")
(defvar dropbox-authorization-url "https://api.dropbox.com/1/oauth/authorize")
(defvar dropbox-access-token nil)
(defvar dropbox-locale nil)

(defvar dropbox-token-file "~/.dropbox-token")
(defvar dropbox-api-content-host "api.dropbox.com")
(setf oauth-nonce-function (function oauth-internal-make-nonce))

(defun dropbox-url (name &optional path)
  (let ((ppath (concat "https://" dropbox-api-content-host "/1/" name)))
    (if path
        (concat ppath "/dropbox/" path)
      path)))

(defun dropbox-get (name &optional path)
  (with-current-buffer (oauth-fetch-url dropbox-access-token (dropbox-url name path))
    (beginning-of-line)
    (json-read)))

(defun dropbox-post (name &optional path args)
  (with-current-buffer (oauth-post-url dropbox-access-token (dropbox-url name path) args)
    (beginning-of-line)
    (json-read)))

(defun dropbox-error-p (json)
  (assoc 'error json))

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
  "Connect to Dropbox, hacking in the Dropbox syntax into find-file"
  (interactive "MEmail/Dropbox username: ")

  (let* ((token (dropbox-authenticate email))
         (part (oauth-access-token-auth-t token)))
    (setq dropbox-token (oauth-t-token part))
    (setq dropbox-token-2 (oauth-t-token-secret part)))

  (if (not (assoc "\\`/db:" file-name-handler-alist))
      (setf file-name-handler-alist
            (cons '("\\`/db:" . dropbox-handler) file-name-handler-alist))))

(defun dropbox-handler (operation &rest args)
  "Handles IO operations to Dropbox files"

  (let ((handler (cdr (assoc operation dropbox-handler-alist))))
    (apply handler args)))

(defconst dropbox-handler-alist
  '((load . dropbox-handle-load)
    (make-symbolic-link . dropbox-handle-make-symbolic-link)
    (file-name-as-directory . dropbox-handle-file-name-as-directory)
    (file-name-directory . dropbox-handle-file-name-directory)
    (file-name-nondirectory . dropbox-handle-file-name-nondirectory)
    (file-truename . dropbox-handle-file-truename)
    (file-exists-p . dropbox-handle-file-exists-p)
    (file-directory-p . dropbox-handle-file-directory-p)
    (file-executable-p . dropbox-handle-file-executable-p)
    (file-readable-p . dropbox-handle-file-readable-p)
    (file-regular-p . dropbox-handle-file-regular-p)
    (file-symlink-p . dropbox-handle-file-symlink-p)
    (file-writable-p . dropbox-handle-file-writable-p)
    (file-ownership-preserved-p . dropbox-handle-file-ownership-preserved-p)
    (file-newer-than-file-p . dropbox-handle-file-newer-than-file-p)
    (file-attributes . dropbox-handle-file-attributes)
    (file-modes . dropbox-handle-file-modes)
    (directory-files . dropbox-handle-directory-files)
    (directory-files-and-attributes
     . dropbox-handle-directory-files-and-attributes)
    (file-name-all-completions . dropbox-handle-file-name-all-completions)
    (file-name-completion . dropbox-handle-file-name-completion)
    (add-name-to-file . dropbox-handle-add-name-to-file)
    (copy-file . dropbox-handle-copy-file)
    (copy-directory . dropbox-handle-copy-directory)
    (rename-file . dropbox-handle-rename-file)
    (set-file-modes . dropbox-handle-set-file-modes)
    (set-file-times . dropbox-handle-set-file-times)
    (make-directory . dropbox-handle-make-directory)
    (delete-directory . dropbox-handle-delete-directory)
    (delete-file . dropbox-handle-delete-file)
    (directory-file-name . dropbox-handle-directory-file-name)
    (executable-find . dropbox-handle-executable-find)
    (start-file-process . dropbox-handle-start-file-process)
    (process-file . dropbox-handle-process-file)
    (shell-command . dropbox-handle-shell-command)
    (insert-directory . dropbox-handle-insert-directory)
    (expand-file-name . dropbox-handle-expand-file-name)
    (substitute-in-file-name . dropbox-handle-substitute-in-file-name)
    (file-local-copy . dropbox-handle-file-local-copy)
    (file-remote-p . dropbox-handle-file-remote-p)
    (insert-file-contents . dropbox-handle-insert-file-contents)
    (insert-file-contents-literally
     . dropbox-handle-insert-file-contents-literally)
    (write-region . dropbox-handle-write-region)
    (find-backup-file-name . dropbox-handle-find-backup-file-name)
    (make-auto-save-file-name . dropbox-handle-make-auto-save-file-name)
    (unhandled-file-name-directory . dropbox-handle-unhandled-file-name-directory)
    (dired-compress-file . dropbox-handle-dired-compress-file)
    (dired-recursive-delete-directory
     . dropbox-handle-dired-recursive-delete-directory)
    (dired-uncache . dropbox-handle-dired-uncache)
    (set-visited-file-modtime . dropbox-handle-set-visited-file-modtime)
    (verify-visited-file-modtime . dropbox-handle-verify-visited-file-modtime)
    (file-selinux-context . dropbox-handle-file-selinux-context)
    (set-file-selinux-context . dropbox-handle-set-file-selinux-context)
    (vc-registered . dropbox-handle-vc-registered)))

(defun dropbox-handle-file-name-directory (filename)
  "Return the directory component in file name FILENAME"

  (if (string-match "^\\(/db:.*\\)/.*$" filename)
      (match-string 1 filename)
    "/db:"))

(defun dropbox-strip-file-name-prefix (filename)
  (substring filename 4))

(defun dropbox-handle-file-name-nondirectory (filename)
  "Return the filename component in file name FILENAME"

  (if (string-match "^/db:.*/\\(.*\\)$" filename)
      (match-string 1 filename)
    (substring filename 4)))

(defun dropbox-handle-expand-file-name (filename &optional default-directory)
  "Return the canonicalized, absolute version of FILENAME"

  filename)

(defun dropbox-handle-file-name-completion (file directory &optional predicate)
  "Complete file name FILE in directory DIRECTORY.
   Returns string if that string is the longest common prefix to files that start with FILE;
           t if only one such file, and it is named FILE;
           nil if no such files"

  (let* ((files (directory-files directory)))
    (try-completion file files predicate)))

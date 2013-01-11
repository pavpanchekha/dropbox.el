;; dropbox.el --- an emacs tramp backend for dropbox
;; Copyright 2011 Pavel Panchekha <pavpanchekha@gmail.com>
;;
;; Based on emacs-yammer (https://github.com/psanford/emacs-yammer/blob/master/yammer.el)

;; TODO
; - Return permissions other than -rwx------ if folder has shares
; - dropbox-handle-set-visited-file-modtime might need actual implementation
; - Switching to deleted buffer on file open
; - Implement `replace` on insert-file-contents
; - Implement `lockname` and `mustbenew` on write-region
; - Default directory on open /db: buffers should be their /db: parent
; - Implement perma-trashing files
; - Make RECURSIVE on DELETE-DIRECTORY work lock-free using /sync/batch
; - Figure out why TRASH is not passed to DELETE-DIRECTORY
; - Make verbosity and secrets defcustoms

(require 'oauth)
(require 'json)
(load-file "dropbox-secrets.el")

(defvar dropbox-request-url       "https://api.dropbox.com/1/oauth/request_token")
(defvar dropbox-access-url        "https://api.dropbox.com/1/oauth/access_token")
(defvar dropbox-authorization-url "https://api.dropbox.com/1/oauth/authorize")
(defvar dropbox-access-token nil)
(defvar dropbox-locale nil)

(defvar dropbox-token-file "~/.dropbox-token")
(defvar dropbox-api-host "api.dropbox.com")
(defvar dropbox-api-content-host "api-content.dropbox.com")
(setf oauth-nonce-function (function oauth-internal-make-nonce))
(defvar dropbox-prefix "/db:")

(defvar dropbox-message-verbose nil)

(defun dropbox-message (fmt-string &rest args)
  (when dropbox-message-verbose
    (apply 'message fmt-string args)))

(defconst url-non-sanitized-chars
  (delete ?~ (append url-unreserved-chars '(?/ ?:))))

(defun url-hexify-url (string)
  "Return a new string that is STRING URI-encoded.
First, STRING is converted to utf-8, if necessary.  Then, for each
character in the utf-8 string, those found in `url-non-sanitized-chars'
are left as-is, all others are represented as a three-character
string: \"%\" followed by two lowercase hex digits."
  (mapconcat (lambda (byte)
               (if (memq byte url-non-sanitized-chars)
                   (char-to-string byte)
                 (format "%%%02x" byte)))
             (if (multibyte-string-p string)
                 (encode-coding-string string 'utf-8)
               string)
             ""))

(defvar dropbox-content-apis '("files" "files_put" "thumbnails" "commit_chunked_upload"))

(defun dropbox-url (name &optional path)
  (let ((ppath (concat "https://"
                       (if (member name dropbox-content-apis)
                           dropbox-api-content-host
                         dropbox-api-host)
                       "/1/" name)))
    (if path
        (concat ppath "/dropbox/" (url-hexify-url (string-strip-prefix "/" (dropbox-strip-file-name-prefix path))))
      ppath)))

(defvar dropbox-cache '())
(defvar dropbox-cache-timeout 60)

(defun dropbox-strip-final-slash (path)
  (if (= (aref path (- (length path) 1)) 47)
      (substring path 0 -1)
    path))

(defun dropbox-cached (name path)
  (let ((cached (assoc (cons name (dropbox-strip-final-slash path))
                       dropbox-cache)))
    (if (and cached
             (time-less-p (time-subtract (current-time) (cadr cached))
                          `(0 ,dropbox-cache-timeout 0)))
        (cddr cached)
      nil)))

(defun dropbox-cache (name path value)
  (setf path (dropbox-strip-final-slash path))
  (let ((cached (assoc (cons name path) dropbox-cache)))
    (if cached
        (setf (cdr cached) (cons (current-time) value)))
    (setf dropbox-cache (cons `((,name . ,path) . (,(current-time) . ,value))
                              dropbox-cache))

    (if (and (string= name "metadata")
             (not (dropbox-error-p value))
             (assoc 'contents value))
        (loop for ent across (cdr (assoc 'contents value))
              for path = (concat "/db:"
                                 (string-strip-prefix "/" (cdr (assoc 'path ent))))
              do (dropbox-cache "metadata" path ent)))

    value))

(defun dropbox-uncache (name path)
  (setf dropbox-cache (remove-if '(lambda (x) (equal (car x) (cons name path)))
                                 dropbox-cache)))

(defun dropbox-clear-cache ()
  (interactive)

  (setf dropbox-cache '()))

(defmacro with-default-directory (dir &rest body)
  (declare (indent 1))
  (let ((old-dir (gensym)) (val (gensym)))
    `(let ((,old-dir default-directory))
       (cd ,dir)
       (let ((,val (progn ,@body)))
         (cd ,old-dir)
         ,val))))

(defvar curl-tracefile nil)

(defun dropbox-get (name &optional path)
  (dropbox-message "Requesting %s for %s" name path)
  (with-default-directory "~/"
    (let ((extra-curl-args (if (and curl-tracefile
                                    (not extra-curl-args))
                               `("--trace" ,curl-tracefile)
                               extra-curl-args)))
      (oauth-fetch-url dropbox-access-token (dropbox-url name path)))))

(defun dropbox-get-http-code (buf)
  (save-excursion
    (with-current-buffer buf
      (beginning-of-buffer)
      (end-of-line)
      (let ((rline (buffer-substring (point-min) (point))))
        (string-match (concat "^\\(HTTP/[\\.[:digit:]]+\\)" "[[:space:]]+"
                              "\\([[:digit:]]\\{3\\}\\)" "[[:space:]]+"
                              "\\(.*\\)$")
                      rline)
        (list (match-string 1 rline) (string-to-number (match-string 2 rline))
              (match-string 3 rline))))))

(defun dropbox-http-success-p (code)
  (and (>= (cadr code) 200) (< (cadr code) 300)))

(defun dropbox-http-down-p (code)
  (and (>= (cadr code) 500) (< (cadr code) 600)))

(defun dropbox-get-json (name &optional path want-contents)
  "Get JSON for the NAME endpoint for path PATH.  The 'contents
field is not guaranteed to be present unless WANT-CONTENTS is
non-nil."

  (let ((cached (dropbox-cached name path)))
    (when (and want-contents (not (assoc 'contents cached)))
      (setf cached nil))
    (or cached
        (with-current-buffer (dropbox-get name path)
          (let ((code (dropbox-get-http-code (current-buffer))))
            (if (dropbox-http-down-p code)
                (error "Dropbox seems to be having problems: %d %s"
                       (cadr code) (caddr code))))
          (beginning-of-line)
          (let ((json-false nil))
            (dropbox-cache name path (json-read)))))))

(defun dropbox-post (name &optional path args)
  (dropbox-uncache name path)
  (dropbox-message "Requesting %s for %s" name path)
  (let ((buf (with-default-directory "~/"
               (oauth-post-url dropbox-access-token (dropbox-url name path) args))))
    (with-current-buffer buf
      (let ((code (dropbox-get-http-code buf)))
        (if (dropbox-http-down-p code)
            (error "Dropbox seems to be having problems: %d %s"
                   (cadr code) (caddr code))))
      (beginning-of-line)
      (let ((json-false nil))
        (json-read)))))

(defun dropbox-error-p (json)
  (assoc 'error json))

(defun dropbox-authenticate ()
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

(defun dropbox-connect ()
  "Connect to Dropbox, hacking in the Dropbox syntax into find-file"
  (interactive)

  (let* ((token (dropbox-authenticate))
         (part (oauth-access-token-auth-t token)))
    (setq dropbox-token (oauth-t-token part))
    (setq dropbox-token-2 (oauth-t-token-secret part)))

  (setf file-name-handler-alist
        (cons '("\\`/db:" . dropbox-handler) file-name-handler-alist)))

(defun dropbox-handler (operation &rest args)
  "Handles IO operations to Dropbox files"

  (if (not (eq operation 'file-remote-p))
      (dropbox-message "Dropbox'ing operation %s for %s" operation args))

  (let ((handler (cdr (assoc operation dropbox-handler-alist))))
    (if handler
        (let ((retval (apply handler args)))
          (if (not (eq operation 'file-remote-p)) (dropbox-message "... returning %s" retval))
          retval)
      (let* ((inhibit-file-name-handlers
              `(dropbox-handler
                tramp-file-name-handler
                tramp-vc-file-name-handler
                tramp-completion-file-name-handler
                . ,inhibit-file-name-handlers))
             (inhibit-file-name-operation operation)
             (retval (apply operation args)))
        (dropbox-message "... fall-through returning %s" retval)
        retval))))

(defconst dropbox-handler-alist
  '(;; Handled
    ; Path parsing
    (file-name-directory . dropbox-handle-file-name-directory)
    (file-name-nondirectory . dropbox-handle-file-name-nondirectory)
    (expand-file-name . dropbox-handle-expand-file-name)
    (file-truename . dropbox-handle-file-truename)
    (substitute-in-file-name . dropbox-handle-substitute-in-file-name)
    (directory-file-name . dropbox-handle-directory-file-name)
    (file-name-as-directory . dropbox-handle-file-name-as-directory)
    (find-backup-file-name . dropbox-handle-find-backup-file-name)
    (make-auto-save-file-name . dropbox-handle-make-auto-save-file-name)
    (unhandled-file-name-directory . dropbox-handle-unhandled-file-name-directory)

    ; Predicates
    (file-directory-p . dropbox-handle-file-directory-p)
    (file-executable-p . dropbox-handle-file-executable-p)
    (file-exists-p . dropbox-handle-file-exists-p)
    (file-newer-than-file-p . dropbox-handle-file-newer-than-file-p)
    (file-remote-p . dropbox-handle-file-remote-p)
    (file-symlink-p . dropbox-handle-file-symlink-p)
    (file-writable-p . dropbox-handle-file-writable-p)
    (vc-registered . dropbox-handle-vc-registered)

    ; Attributes
    (file-attributes . dropbox-handle-file-attributes)
    (file-modes . dropbox-handle-file-modes)
    (set-visited-file-modtime . dropbox-handle-set-visited-file-modtime)

    ; Directory Contents
    (directory-files . dropbox-handle-directory-files)
    (file-name-all-completions . dropbox-handle-file-name-all-completions)
    (file-name-completion . dropbox-handle-file-name-completion)
    (make-directory . dropbox-handle-make-directory)
    (delete-file . dropbox-handle-delete-file)
    (delete-directory . dropbox-handle-delete-directory)

    ; File Contents
    (insert-file-contents . dropbox-handle-insert-file-contents)
    (write-region . dropbox-handle-write-region)

    ; Misc
    (process-file . dropbox-handle-process-file)

    ;; Unhandled
    ; Predicates
    (file-ownership-preserved-p . dropbox-handle-file-ownership-preserved-p)
    (file-readable-p . dropbox-handle-file-readable-p)
    (file-regular-p . dropbox-handle-file-regular-p)

    ; Attributes
    (set-file-modes . dropbox-handle-set-file-modes)
    (set-file-times . dropbox-handle-set-file-times)
    (verify-visited-file-modtime . dropbox-handle-verify-visited-file-modtime)
    (file-selinux-context . dropbox-handle-file-selinux-context)
    (set-file-selinux-context . dropbox-handle-set-file-selinux-context)

    ; Directory Contents
    (directory-files-and-attributes
     . dropbox-handle-directory-files-and-attributes)
    (make-symbolic-link . dropbox-handle-make-symbolic-link)
    (copy-file . dropbox-handle-copy-file)
    (copy-directory . dropbox-handle-copy-directory)
    (rename-file . dropbox-handle-rename-file)
    (executable-find . dropbox-handle-executable-find)

    ; File Contents
    (file-local-copy . dropbox-handle-file-local-copy)
    (insert-file-contents-literally
     . dropbox-handle-insert-file-contents-literally)

    ; Misc
    (load . dropbox-handle-load)
    (add-name-to-file . dropbox-handle-add-name-to-file)
    (start-file-process . dropbox-handle-start-file-process)
    (shell-command . dropbox-handle-shell-command)
    (insert-directory . dropbox-handle-insert-directory)
    (dired-insert-directory . dropbox-handle-dired-insert-directory)
    (dired-compress-file . dropbox-handle-dired-compress-file)
    (dired-recursive-delete-directory
     . dropbox-handle-dired-recursive-delete-directory)
    (dired-uncache . dropbox-handle-dired-uncache)))

;; Path Parsing

(defun dropbox-handle-file-name-directory (filename)
  "Return the directory component in file name FILENAME"

  (if (string-match "^\\(/db:.*/\\).*$" filename)
      (match-string 1 filename)
    "/db:"))

(defun dropbox-strip-file-name-prefix (filename)
  (substring filename 4))

(defun dropbox-handle-file-name-nondirectory (filename)
  "Return the filename component in file name FILENAME"

  (cond
   ((string-match "^/db:.*/\\(.*\\)$" filename)
    (match-string 1 filename))
   (t (substring filename 4))))

(defun dropbox-handle-expand-file-name (filename &optional default-directory)
  "Return the canonicalized, absolute version of FILENAME"

  filename)

(defun dropbox-handle-file-truename (filename)
  filename)

(defun dropbox-handle-substitute-in-file-name (filename)
  "Replace slashes with one slash"

  (replace-regexp-in-string ".*//+" "/" filename))

(defun dropbox-handle-directory-file-name (directory)
  "Remove the final slash from a directory name"

  (if (eq (aref directory (1- (length directory))) ?/)
      (substring directory 0 -1)
    directory))

(defun dropbox-handle-file-name-as-directory (directory)
  "Remove the final slash from a directory name"

  (if (and
       (not (eq (aref directory (1- (length directory))) ?/))
       (not (string= directory "/db:")))
      (concat directory "/")
    directory))

(defun dropbox-handle-find-backup-file-name (fn)
  nil)

(defun dropbox-handle-make-auto-save-file-name ()
  (make-temp-file (file-name-nondirectory buffer-file-name)))

(defun dropbox-handle-unhandled-file-name-directory (filename)
  "/db:")

;; Predicates
(defun dropbox-file-p (filename)
  "Return t if file FILENAME is a Dropbox file (i.e. starts with `dropbox-prefix')"

  (string-prefix-p dropbox-prefix filename))

(defun dropbox-handle-file-directory-p (filename)
  "Return t if file FILENAME is a directory, too"

  (if (or (string= filename "/db:") (string= filename "/db:/"))
      t
    (let ((resp (dropbox-get-json "metadata" filename)))
      (if (dropbox-error-p resp)
          nil
        (cdr (assoc 'is_dir resp))))))

(defun dropbox-handle-file-executable-p (filename)
  (file-directory-p filename))

(defun dropbox-handle-file-exists-p (filename)
  "Return t if file FILENAME exists"

  (let ((resp
         (dropbox-get-json "metadata" filename)))
    (and (not (dropbox-error-p resp))
         (not (assoc 'is_deleted resp)))))

(defun dropbox-handle-file-newer-than-file-p (file1 file2)
  ; these files might not both be dropbox files
  (let ((file1attr (file-attributes file1))
	(file2attr (file-attributes file2)))
    (let ((time1 (if file1attr (elt file1attr 4) nil))
	  (time2 (if file2attr (elt file2attr 4) nil)))
      (if time1
	  (if time2
	      (time-less-p time2 time1)
	    t)
	nil))))

(defun dropbox-handle-file-remote-p (file &optional identification connected)
  "Test whether FILE is a remote file"

  (dropbox-message file)

  (if (and connected (not dropbox-access-token))
      nil
    (case identification
      ((method) "/db:")
      ((user) "")
      ((host) "")
      ((localname) (dropbox-strip-file-name-prefix file))
      (t "/db:"))))

(defun dropbox-handle-file-symlink-p (filename)
  nil)

(defun dropbox-handle-file-readable-p (filename)
  t)

(defun dropbox-handle-file-writable-p (filename)
  t)

(defun dropbox-handle-vc-registered (file)
  nil)

;; Attributes

(defun dropbox-handle-file-attributes (filename &optional id-format)
  (let ((resp
         (dropbox-get-json "metadata" filename)))
    (if (dropbox-error-p resp)
        nil
      (let ((date (date-to-time (cdr (assoc 'modified resp)))))
      (list (cdr (assoc 'is_dir resp)) ; Is dir?
            1 ; Number of links
            0 ; UID
            0 ; GID
            date ; atime
            date ; mtime
            date ; ctime
            (cdr (assoc 'bytes resp)) ; size in bytes
            ; TODO figure out if folder has any shares
            (concat (if (cdr (assoc 'is_dir resp)) "d" "-") "rwx------") ; perms
            nil
            0
            0)))))

(defun dropbox-handle-file-modes (filename)
  448) ; 448 = 0b111000000 is rwx------

(defun dropbox-handle-set-visited-file-modtime (&optional time-list)
  ; TODO: this might need to be implemented
  nil)

;; Directory Contents

(defun string-strip-prefix (prefix str)
  (if (string-prefix-p prefix str)
      (substring str (length prefix))
      str))

(defun dropbox-extract-fname (file path &optional full)
  (let ((fname (string-strip-prefix "/" (cdr (assoc 'path file)))))
    (if (cdr (assoc 'is_dir file)) (setf fname (concat fname "/")))
    (if full (concat dropbox-prefix fname)
      (string-strip-prefix "/" (string-strip-prefix path fname)))))

(defun dropbox-handle-directory-files (directory &optional full match nosort)
  "Return a list of names of files in DIRECTORY.
There are three optional arguments:
If FULL is non-nil, return absolute file names.  Otherwise return names
 that are relative to the specified directory.
If MATCH is non-nil, mention only file names that match the regexp MATCH.
If NOSORT is non-nil, the list is not sorted--its order is unpredictable.
Otherwise, the list returned is sorted with `string-lessp'.
NOSORT is useful if you plan to sort the result yourself."

  (let* ((path (dropbox-strip-file-name-prefix directory))
	 (metadata (dropbox-get-json "metadata" directory t)) ; want-contents: t
	 (unsorted
	  (if (cdr (assoc 'is_dir metadata))
	      (loop for file across (cdr (assoc 'contents metadata))
		    for fname = (dropbox-extract-fname file path full)
		    if (or (null match) (string-match match fname))
		    collect fname)
	    nil)))
    (if nosort unsorted (sort unsorted 'string-lessp))))

(defun dropbox-handle-file-name-all-completions (file directory &optional predicate)
  "Complete file name FILE in directory DIRECTORY.
   Returns string if that string is the longest common prefix to files that start with FILE;
           t if only one such file, and it is named FILE;
           nil if no such files"

  (let* ((files (directory-files directory)))
    (all-completions file files predicate)))

(defun dropbox-handle-file-name-completion (file directory &optional predicate)
  "Complete file name FILE in directory DIRECTORY.
   Returns string if that string is the longest common prefix to files that start with FILE;
           t if only one such file, and it is named FILE;
           nil if no such files"

  (let ((files (directory-files directory))
        (predicate (if (eq predicate 'file-exists-p) nil predicate)))
    (try-completion file files predicate)))

(defun dropbox-handle-make-directory (dir &optional parents)
  "Create the directory DIR and, if PARENT is non-nil, all parents"

  (if (or parents
          (let ((parent (file-name-directory (directory-file-name dir))))
            (and (file-exists-p parent) (file-directory-p parent))))
      (dropbox-cache "metadata" dir
                     (dropbox-post
                      "fileops/create_folder" nil
                      `(("root" . "dropbox")
                        ("path" . ,(dropbox-strip-file-name-prefix dir)))))))

(defun dropbox-handle-delete-file (filename &optional trash)
  "Delete file name FILENAME.  If TRASH is nil, permanently delete it."

  (if trash
      (dropbox-cache "metadata" filename
                     (dropbox-post "fileops/delete" nil
                                   `(("root" . "dropbox")
                                     ("path" . ,(dropbox-strip-file-name-prefix
                                                 filename)))))
    (error "Perma-trashing files not yet implemented")))

(defun dropbox-handle-delete-directory (directory &optional recursive trash)
  "Delete the directory DIRECTORY.  If TRASH is nil, permanently delete it.
   If RECURSIVE is nil, throw an error if the directory has contents"

  (if (not recursive)
      (error "Non-recursive directory delete not yet implemented")
    (if nil ;(not trash) ; Emacs passes only one argument from delete-directory
        (error "Perma-trashing directories not yet implemented")
      (dropbox-cache "metadata" directory
                     (dropbox-post "fileops/delete" nil
                                   `(("root" . "dropbox")
                                     ("path" . ,(dropbox-strip-file-name-prefix
                                                 directory))))))))

;; File contents

(defun dropbox-handle-insert-file-contents (filename &optional visit beg end replace)
  ; TODO: Fails on images with switch to deleted buffer
  ; TODO: implement replace
  (barf-if-buffer-read-only)
  (let* ((buf (current-buffer))
         (respbuf (dropbox-get "files" filename))
         (http-code (dropbox-get-http-code respbuf)))
    (if (file-exists-p filename)
        (progn
          (switch-to-buffer respbuf)
          (beginning-of-buffer)
          (re-search-forward "\r\n\r\n")
          (delete-region (point-min) (point))
          (switch-to-buffer buf)
          (save-excursion (insert-buffer-substring respbuf beg end)))
      (switch-to-buffer buf)
      (set-buffer-modified-p nil))
    (when visit
      (setf buffer-file-name filename)
      (setf buffer-read-only (not (file-writable-p filename))))))

; Redefine oauth-curl-retrieve to take extra-curl-args and to echo the curl command
(defun oauth-curl-retrieve (url)
  "Retrieve via curl"
  (url-gc-dead-buffers)
  (set-buffer (generate-new-buffer " *oauth-request*"))
  (let ((curl-args `("-s" ,(when oauth-curl-insecure "-k")
                     "-X" ,url-request-method
                     "-i" ,url
                     ,@(when oauth-post-vars-alist
                         (apply
                          'append
                          (mapcar
                           (lambda (pair)
                             (list
                              "-d"
                              (concat (car pair) "="
                                      (oauth-hexify-string (cdr pair)))))
                           oauth-post-vars-alist)))
                     ,@(oauth-headers-to-curl url-request-extra-headers)
                     ,@extra-curl-args)))
    (message "curl-args: %s" curl-args)
    (apply 'call-process "curl" nil t nil curl-args))
  (url-mark-buffer-as-dead (current-buffer))
  (current-buffer))

(defvar extra-curl-args nil)

(defun dropbox-upload (local-path remote-path)
  (save-excursion
    (let* ((extra-curl-args `("--data-binary" ,(concat "@" local-path)))
          (url-request-extra-headers '(("Content-Type" . "application/octet-stream")))
          (resp (dropbox-post "files_put" remote-path '())))
      (if (dropbox-error-p resp)
          nil
        (dropbox-cache "metadata" remote-path)))))

(defun dropbox-handle-write-region (start end filename &optional
					  append visit lockname mustbenew)
  "Write current region into specified file.
When called from a program, requires three arguments:
START, END and FILENAME.  START and END are normally buffer positions
specifying the part of the buffer to write.
If START is nil, that means to use the entire buffer contents.
If START is a string, then output that string to the file
instead of any buffer contents; END is ignored.

Optional fourth argument APPEND if non-nil means
  append to existing file contents (if any).  If it is an integer,
  seek to that offset in the file before writing.
Optional fifth argument VISIT, if t or a string, means
  set the last-save-file-modtime of buffer to this file's modtime
  and mark buffer not modified.
If VISIT is a string, it is a second file name;
  the output goes to FILENAME, but the buffer is marked as visiting VISIT.
  VISIT is also the file name to lock and unlock for clash detection.
If VISIT is neither t nor nil nor a string,
  that means do not display the \"Wrote file\" message.
The optional sixth arg LOCKNAME, if non-nil, specifies the name to
  use for locking and unlocking, overriding FILENAME and VISIT.
The optional seventh arg MUSTBENEW, if non-nil, insists on a check
  for an existing file with the same name.  If MUSTBENEW is `excl',
  that means to get an error if the file already exists; never overwrite.
  If MUSTBENEW is neither nil nor `excl', that means ask for
  confirmation before overwriting, but do go ahead and overwrite the file
  if the user confirms."

  ; TODO: implement lockname and mustbenew
  (assert (not append)) ; TODO: implement append

  (let ((localfile (make-auto-save-file-name)))
    (write-region start end localfile nil 1)
    (let ((resp (dropbox-upload localfile filename)))
      (if (dropbox-error-p resp)
          nil
        (when (stringp visit)
          (set-visited-file-name visit))
        (when (or (eq t visit) (stringp visit))
          (set-buffer-modified-p nil))
        (when (or (eq t visit) (eq nil visit) (stringp visit))
          (message "Wrote %s" filename))))))

(defun dropbox-handle-insert-directory
  (filename switches &optional wildcard full-directory-p)
  "Like `insert-directory' for Dropbox files. Code adapted from
`tramp-sh-handle-insert-directory'."

  (setq filename (expand-file-name filename))
  (let ((localname (dropbox-strip-file-name-prefix filename)))
    (when (stringp switches)
      (setq switches (split-string switches)))
    (unless full-directory-p
      (setq switches (add-to-list 'switches "-d" 'append)))
    (setq switches (mapconcat 'shell-quote-argument switches " "))
    (dropbox-message
     "Inserting directory `ls %s %s', wildcard %s, fulldir %s"
     switches filename wildcard full-directory-p)

    ; -rw-r--r--   1 ahaven  staff   1476 Jan  7 12:48 tramp.py
    (if (not full-directory-p)
        (let ((attributes (file-attributes filename 'string)))
          (insert (format "  %s %2d %8s %8s %8d %s "
                          (elt attributes 8)
                          (elt attributes 1)
                          (elt attributes 2)
                          (elt attributes 3)
                          (elt attributes 7)
                          (format-time-string "%X" (elt attributes 4))))
          (let ((fname (file-name-nondirectory (directory-file-name filename)))
                (start (point)))
            (insert fname)
            (when (elt attributes 0)
              (put-text-property start (point) 'dired-filename t))
          (newline)))
      (insert "  " filename) ; print directory
      (newline)
      (let ((acct-info (dropbox-get-json "account/info")))
        (unless (null acct-info)
          (let ((quota-info (cdr (assoc 'quota_info acct-info))))
            (insert (format "total used %d available %d"
                            (+ (cdr (assoc 'shared quota-info))
                               (cdr (assoc 'normal quota-info)))
                            (cdr (assoc 'quota quota-info))))
            (newline))))
      (loop for file in (if wildcard
                            (directory-files (file-name-directory filename) t filename)
                          (directory-files filename t))
            do (insert-directory file switches)))))

(defun dropbox-handle-dired-insert-directory (dir switches &optional file-list wildcard hdr)
  (dropbox-handle-insert-directory dir switches wildcard t))
;; Misc

(defun dropbox-handle-process-file (program &optional infile buffer display &rest args)
  nil)

(defun dropbox-handle-copy-file (file newname &optional ok-if-already-exists
                                      keep-time preserve-uid-gid preserve-selinux-context)
  (cond
   ((and (dropbox-file-p file) (dropbox-file-p newname))
    (dropbox-cache "metadata" newname
                   (dropbox-post
                    "fileops/copy" nil
                    `(("root" . "dropbox")
                      ("from_path" . ,(dropbox-strip-file-name-prefix file))
                      ("to_path" . ,(dropbox-strip-file-name-prefix newfile))))))
   ((and (dropbox-file-p file) (not (dropbox-file-p newname)))
    (save-excursion
      (let* ((buf (current-buffer))
             (respbuf (dropbox-get "files" file))
             (http-code (dropbox-get-http-code respbuf)))
        (if (not (file-exists-p filename))
            (error "File to copy doesn't exist")
          (switch-to-buffer respbuf)
          (beginning-of-buffer)
          (re-search-forward "\r\n\r\n")
          (write-region (point) (point-max) newname)
          (switch-to-buffer buf)))))
   ((and (not (dropbox-file-p file)) (dropbox-file-p newname))
    (dropbox-upload file newname))))

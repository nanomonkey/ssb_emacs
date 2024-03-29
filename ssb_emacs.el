;; -*- lexical-binding: t; -*-
;; Secure Scuttlebutt sbot server calls ;;

(require 'json)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Start ssb server and set your id ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ssb-start-server () 
"creates an asyncronous process starting called SSB-SERVER running SBOT with argument SERVER 
and a buffer called SSB-SERVER-BUFFER"
(interactive)
(start-process "ssb-server"   "ssb-server-buffer" "sbot" "server"))

(defun ssb-stop-server ()
  (interactive)
  (process-send-eof "ssb-server")
  (kill-buffer "ssb-server-buffer"))

(defun ssb-check-server ()
  (if (not (process-status "ssb-server"))
      (if (y-or-n-p "SSB server is not running, start?")
          (progn 
            (ssb-start-server)
            (sleep-for 1)))))

(defun ssb-whoami ()
  (interactive)
  (ssb-check-server)
  (setq ssb_id
        (plist-get (json-read-from-string 
                    (shell-command-to-string "sbot whoami")) :id)))

(defun ssb-command (command &rest arguments)
 (ssb-check-server)
 (json-read-from-string
  (shell-command-to-string
   (format "sbot %s" (shell-quote-argument (apply #'format comand arguments))))))

;;;;;;;;;;
;; Pubs ;;
;;;;;;;;;;

(defun ssb-join-pub (code)
  (shell-command-to-string (concat
                            "sbot invite.accept " code)))

(defun ssb-join-pub (code)
  (ssb-command "invite.accept" code))

;;;;;;;;;;;;;;;;;;;;;;
;; Publish messages ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun ssb-publish (text) 
  ; publish a message
  (shell-command-to-string 
   (concat "sbot publish --type post --text \"" text "\"")))

(defun ssb-quick-message (message)
  (interactive)
  ; Create a quick message from the minibuffer.  No use of RET, /n only.
  (interactive "sMessage: " )
  (ssb-publish message))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Follow and Unfollow  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ssb-follow (feed_id)
  (shell-command-to-string 
   (format "sbot publish --type contact --contact %s --following" 
           feed_id)))

(defun ssb-unfollow (feed_id)
  (shell-command-to-string 
   (format "sbot publish --type contact --contact %s --no-following" 
           feed_id)))

;;;;;;;;;;;;;;;;;;;
;; Read Messages ;;
;;;;;;;;;;;;;;;;;;;

(defun ssb-live-feed (id)
  ; read raw live feed
  (interactive)
  (async-shell-command "sbot feed --live"))

(defun ssb-last-ten (id)
  " read the last n messages"
  (async-shell-command  "sbot feed --limit 10"))

;(print ssb_id)
;(ssb-last-ten ssb_id)

(defun ssb-read-log (user_id)
  ;; read a specific user feed
  (shell-command-to-string 
   (concat "sbot createUserStream --id " user_id)))
;(ssb-read-log ssb_id)

(defun ssb-read-last (id) 
  (shell-command-to-string 
   (concat "sbot createUserStream --id " id " --limit 1 --reverse")))
;(print (ssb-read-last ssb_id))


(defun ssb-get (message_id) 
  (shell-command-to-string (concat "sbot get  " message_id)))

(defun ssb-log-type (type &optional args)
  (shell-command-to-string (concat "sbot logt --type \"" type "\"" args)))

(defun ssb-related-messages (message_id)
  (shell-command-to-string (concat "sbot relatedMessages --id " message_id)))


;;;;;;;;;;;;;;;;;;;;;
;; Message Parsing ;;
;;;;;;;;;;;;;;;;;;;;;

(defun ssb-message-value (message_data)
  (plist-get (json-read-from-string message_data) :value))

(defun ssb-message-author (message_data) 
  (plist-get (ssb-message-value message_data) :author))

(defun ssb-message-timestamp (message_data)
  (format-time-string "%D %H:%M"
                      (plist-get (ssb-message-value message_data) 
                                 :timestamp)))

(defun ssb-message-content (message_data) 
  (plist-get (ssb-message-value message_data) :content))

(defun ssb-message-text (message_data)
  (plist-get (ssb-message-content message_data) :text))

(defun ssb-message-type (message_data) 
  (plist-get (ssb-message-content message_data) :type))

(defun ssb-message-channel (message_data) 
  (plist-get (ssb-message-content message_data) :channel))

(defun ssb-message-name (message_data)
  (plist-get (ssb-message-content message_data) :name))

(defun ssb-message-previous (message_data)
  (plist-get message_data :previous))

;; get name using existing flume name.json file

(defun ssb-get-name (user_id)
  (with-temp-buffer
    (insert-file-contents ssb_name_file)
    (goto-char 1)
    (search-forward user_id)
    (setq startPos (+ 3  (point)))
    (search-forward ",")
    (setq endPos (- (point) 2))
    (buffer-substring-no-properties startPos endPos)))

;;(get-name "@EMovhfIrFk4NihAKnRNhrfRaqIhBv1Wj8pTxJNgvCCY=.ed25519")


(defun get-name (user-id)
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (command (concat "sbot links --source " user-id " --dest " user-id " --rel about --values"))
         (about (json-read-from-string (shell-command-to-string command))))))

(defun get-thread (post-id)
  "gets the full thread that a post is a part of"
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (command (concat "sbot links --values --rel root --dest " post-id))
         (thread (json-read-from-string (shell-command-to-string command))))))

(defun get-posts-after (timestamp)
  (shell-command-to-string (concat "sbot logt --type post --gte " timestamp)))

;;(get-post-after (format-time-string "%s" (time-add (current-time) (* -24 60 60 1000))))

(defun ssb-one-about-message ()
  ;; procedural method of grabbing an about message from a buffer
  (search-forward "{")
  (setq startPos (1- (point)))
  (search-forward "}")
  (search-forward "}")
  (search-forward "}")
  (setq endPos (point))
  (json-read-from-string (buffer-substring-no-properties startPos endPos)))


; (ssb-start-server)

(defun ssb-decode (message_id)
  ; untested decode message
  (shell-command-to-string (concat "sbot private.unbox " message_id)))


(defun ssb-display-buffer (message_data)
  (pop-to-buffer (get-buffer-create "SSB-Message"))
  (erase-buffer)
  (insert message_data)
  (newline)
  (insert (concat "Author: " (get-name 
                              (ssb-message-author message_data)))) 
  (insert (concat " Time: " (ssb-timestamp message_data)))
  (newline)
  (let (type)
    (setq type (ssb-type message_data))
    (cond ((string= type "post")
           (insert (ssb-text message_data)))
          ((string= type "channel")
           (insert (concat "subscribed to" (ssb-channel message_data)))
           (t (insert type)))))
  (insert-button "Previous" 
                 'action 
                 (lambda (x) 
                   (ssb-get (ssb-message-previous (message_data))))))

(defun ssb-display-last ()
  (interactive)
  (ssb-display-buffer (ssb-read-last ssb_id)))


;;;;;;;;;;;;
;; images ;;
;;;;;;;;;;;;

(defun b64->hex (b64-string)
  (mapconcat (lambda (x) (format "%x" x)) (base64-decode-string b64-string) ""))

(defun blob-path (blob_id ssb_path)
  (let* ((id (b64->hex (substring (file-name-sans-extension blob_id) 1)))
         (blob_type (file-name-extension blob_id)))
    (concat ssb_path "/blobs/" blob_type "/" (substring id 0 2) "/" (substring id 2))))

(defun display-image (blob_id)
  (newline)
  (insert-image (create-image (blob-path blob_id "~/.ssb"))))


;; doesn't work yet

(defun ssb-get-blob (blob_id)
  (with-temp-buffer
    (let ((coding-system-for-write 'binary))
      (shell-command "sbot blobs.get \"" blob_id "\"")
      (image-mode))))

(defun ssb-list-blobs ()
  (interactive)
  (with-temp-buffer
    (insert (shell-command-to-string "sbot blobs.ls"))
    (goto-char 1)
    (while (not (eobp))
      (ssb-get-blob 
       (buffer-substring-no-properties 
        (beginning-of-line) 
        (end-of-line))))))

(defun ssb-write-blob (blob_id)
  (with-temp-buffer
    (insert (shell-command "sbot blobs.get \"" blob_id "\""))
    (write-file "image.jpg")))

(defun ssb-list-blobs ()
  (interactive)
  (with-temp-buffer
    (insert (shell-command-to-string "sbot blobs.ls"))
    (goto-char 1)
    (while (not (eobp))
      (ssb-get-blob 
       (buffer-substring-no-properties 
        (beginning-of-line) 
        (end-of-line))))))

;(ssb-get-blob "&//URUOPzcwXbPGgvX0phR7AOjM+UmO2VnQXfvAluKIk=.sha256")

(defun blob-preview-sentinel (blob event)
  (if (equal event "finished\n")
      (progn
        (switch-to-buffer "blob-preview-buffer")
        (when (display-images-p)
          (image-mode)))
    (warn "Blob Preview failed: %s" event)))

(defun ssb-blob-preview (blob_id)
  "Preview blob." 
  (interactive)
  (let ((b (get-buffer "blob-preview-buffer")))
    (when b
      (kill-buffer b)))
  (let ((process-connection-type nil)
        (buf (get-buffer-create "blob-preview-buffer"))
        (coding-system-for-read 'raw-text)
        (coding-system-for-write 'binary))
    (let ((blob (start-process "blob" buf
                               "sbot" "blobs.get" 
                               (shell-quote-argument blob_id))))
      (process-send-eof blob)
      (set-process-sentinel blob 'blob-preview-sentinel))))

;(ssb-blob-preview "&//URUOPzcwXbPGgvX0phR7AOjM+UmO2VnQXfvAluKIk=.sha256")


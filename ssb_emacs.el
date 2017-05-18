;; Secure Scuttlebutt Emacs Major Mode ;;

(require 'json)
(setq json-object-type 'plist)
(setq ssb_name_file "~/.ssb/ssb_names.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Start ssb server and set your id ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ssb-start-server () 
(interactive)
(start-process "ssb-server"   "ssb-server-buffer" "sbot" "server"))

(defun ssb-stop-server ()
  (interactive)
  (process-send-eof "ssb-server")
  (kill-buffer "ssb-server-buffer"))


(defun ssb-whoami ()
  (interactive)
  (setq ssb_id
        (plist-get (json-read-from-string 
                    (shell-command-to-string "sbot whoami")) :id)))

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
  (async-shell-command "sbot feed --live"))

(defun ssb-read-log (user_id)
  ;; read a specific user feed
  (shell-command-to-string 
      (concat "sbot createUserStream --id " user_id)))

(defun ssb-read-last (id) 
  (shell-command-to-string 
   (concat "sbot createUserStream --id " id " --limit 1 --reverse")))

(defun ssb-get (message_id) 
  (shell-command-to-string (concat "sbot get  " message_id)))

(defun ssb-log-type (type &optional args)
  (shell-command-to-string (concat "sbot logt --type \"" type "\"" args)))


;(ssb-read-last ssb_id)

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

;; Create local name hashtable and populate it from about stream
(defun ssb-create-name-hash ()
  (setq names (make-hash-table :test 'equal))
  (with-temp-buffer
    (insert-file-contents ssb_name_file)
    (goto-char 1)
    (while (not (eobp))
      (puthash 
       (read 
        (buffer-substring-no-properties 
         (begining-of-line) (end-of-line)) )) )))

(defun ssb-save-names ()
  (find-file ssb_name_file)
  (erase-buffer)
  (prin1 names (current-buffer))
  (save-buffer)
  (kill-buffer (current-buffer)))

(defun ssb-name ()
  (let (command timestamp json) 
    (setq timestamp (gethash "last_ts" names))
    (setq command "sbot logt --type \"about\" --limit 1")
    (if timestamp (setq command 
                        (concat command " --gt \"" timestamp "\"")))
    (setq json (json-read-from-string (shell-command-to-string command)))
    (if (not (equal json "End of file while parsing JSON"))
        (progn  (let* ((msg-value (plist-get json :value))
                       (msg-content (plist-get msg-value :content)))
                  (puthash "last_ts" 
                           (number-to-string (plist-get json :ts)) names)
                  (if (string= (plist-get msg-value :author) 
                               (plist-get msg-content :about)) 
                      (puthash (plist-get msg-content :about)
                               (plist-get msg-content :name) names)))
                (ssb-name)))))

(defun ssb-one-about-message ()
  ;; procedural method of grabbing an about message from a buffer
  (search-forward "{")
  (setq startPos (1- (point)))
  (search-forward "}")
  (search-forward "}")
  (search-forward "}")
  (setq endPos (point))
  (json-read-from-string (buffer-substring-no-properties startPos endPos)))

(defun ssb-process-about (json)
      (let ((msg-value (plist-get json :value))
          (msg-content (plist-get (plist-get json :value) :content)))
      (if (string= (plist-get msg-value :author)
                   (plist-get msg-content :about))
          (puthash
           (plist-get msg-content :about)
           (plist-get msg-content :name)
           names))))

(defun ssb-get-names2 ()
  (with-temp-buffer
    (insert (shell-command-to-string "sbot logt --type \"about\""))
    (goto-char 1)
    (while (not (eobp))
      (ssb-process-about (ssb-one-about-message)))
    (ssb-save-names)))

(defun ssb-get-names ()
  ; attempt do get names with one call...
  (dolist (json
           (json-read-from-string 
            (shell-command-to-string "sbot logt --type \"about\"")))
    (let ((msg-value (plist-get json :value))
          (msg-content (plist-get (plist-get json :value) :content)))
      (if (string= (plist-get msg-value :author)
                   (plist-get msg-content :about))
          (puthash
           (plist-get msg-content :about)
           (plist-get msg-content :name)
           names)))))


; (ssb-save-names)
; (ssb-get-names)
; (ssb-start-server)
; (ssb-name)
; (hash-table-count names)
; (gethash "last_ts" names)
; (gethash id names)
; (require 'subr-x)
; (hash-table-values names) 
; (clrhash names)
; (gethash id names)
; (maphash (lambda (k v) (print (concat k ":" v))) names)
; (puthash "last_ts" "1491593797498" names)
; (puthash "last_ts" "2491593797498" names)

(defun ssb-decode (message_id)
  ; untested decode message
  (shell-command-to-string (concat "sbot private.unbox " message_id)))


(defun ssb-display-buffer (message_data)
  (pop-to-buffer (get-buffer-create "SSB-Message"))
  (erase-buffer)
  (insert (concat "Author: " (gethash (ssb-author message_data) names))) 
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
                 'action '(ssb-get (ssb-message-previous (message_data)))))

(defun ssb-display-last ()
  (interactive)
  (ssb-display-buffer (ssb-read-last id)))

 

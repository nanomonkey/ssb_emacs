(require 'json)

(defun ssb-start-server () (async-shell-command "sbot server"))

(defun ssb-whoami ()
  (alist-get 'id (json-read-from-string 
                  (shell-command-to-string "sbot whoami"))))

(defun ssb-id ()
  (if (ssb-whoami)
      (setq id (ssb-whoami))
    (progn (ssb-start-server) (ssb-id))))

(ssb-id)

(defun ssb-read-log (user_id)
  (shell-command-to-string 
      (concat "sbot createUserStream --id " id)))

(defun ssb-read-last (id) 
  (shell-command-to-string 
   (concat "sbot createUserStream --id " id " --limit 1 --reverse")))

(defun ssb-get (message_id) 
  (shell-command-to-string (concat "sbot get  " message_id)))

(defun ssb-log-type (type &optional args)
  (shell-command-to-string (concat "sbot logt --type \"" type "\"" args)))

(defun ssb-get-previous (message_id)
  ; returns previous message id given a message id
  (json-read-from-string (ssb-get (alist-get 'previous message_id))))

(defun ssb-value (message_data)
  (alist-get 'value (json-read-from-string message_data)))

(defun ssb-author (message_data) 
  (alist-get 'author (ssb-value message_data)))

(defun ssb-timestamp (message_data)
  (format-time-string "%D %H:%M"
                      (alist-get 'timestamp (ssb-value message_data))))

(defun ssb-content (message_data) 
  (alist-get 'content (ssb-value message_data)))

(defun ssb-text (message_data)
  (alist-get 'text (ssb-content message_data)))

(defun ssb-type (message_data) 
  (alist-get 'type (ssb-content message_data)))

(defun ssb-channel (message_data) 
  (alist-get 'channel (ssb-content message_data)))

(defun ssb-name (message_data)
  (alist-get 'name (ssb-content message_data)))

(setq names (make-hash-table))

(defun ssb-name ()
  (let (command timestamp json) 
    (setq timestamp (gethash "name-ts" names))
    (setq command "sbot logt --type \"about\" --keys --limit 1")
    (if timestamp (concat command " --gt " timestamp))
    (print command)
    (setq json (json-read-from-string (shell-command-to-string command)))
    (print json)
    (puthash "name_ts" (alist-get 'ts json) names)
    (print (alist-get 'ts json))
    (if (string= (alist-get 'author (alist-get 'value json)) 
                 (alist-get 'about (alist-get 'value json))) 
        (puthash (alist-get 'about (alist-get 'value json))
                 (alist-get 'name (alist-get 'value json))))))

; (gethash "name_ts" names)
; (ssb-name)

(defun ssb-names ()
  (pop-to-buffer (get-buffer-create "SSB-Names"))
  (insert (shell-command-to-string "sbot logt --type \"about\" "))
  (goto-char (point-min))
  (goto-char (search-forward "author\": "))
  (setq author ()))
    

;(ssb-start-server)
;(ssb-names)
;(maphash 'print names)

(defun ssb-decode (message_id) 
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
           (t (insert type))))))

;(ssb-display (ssb-read-last id))
(ssb-display-buffer (ssb-read-last id))

(defun ssb-publish (text) 
  ; publish a message
  (shell-command-to-string 
   (concat "sbot publish --type post --text \"" text "\"")))

(defun ssb-quick-message (message)
  ; Create a quick message from the minibuffer.  No use of RET, /n only.
  (interactive "sMessage: " )
  (ssb-publish message))

(gethash "@+D0ku/LReK6kqd3PSrcVCfbLYbDtTmS4Bd21rqhpYNA=.ed25519" names)
(defun ssb-live-feed (id)
  (async-shell-command "sbot feed --live --reverse --limit 5"))


;set keymaps
(global-set-key "\C-s s" 'ssb-start-server)
(global-set-key "\C-s p" 'ssb-publish)
(global-set-key "\C-s c" 'ssb-quick-message)
(global-set-key "\C-s w" 'ssb-whoami)

 

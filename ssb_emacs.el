(require 'json)

(defun ssb-start-server () (async-shell-command "sbot server"))

(defun ssb-whoami ()
  (cdr (assoc 'id (json-read-from-string (shell-command-to-string "sbot whoami")))))

(setq id (ssb-whoami))

(defun ssb-read-log (user_id)
  (shell-command-to-string 
      (concat "sbot createUserStream --id " id)))
  
(setq logs (ssb-read-log id))

(defun ssb-read-last (id) 
  (shell-command-to-string 
   (concat "sbot createUserStream --id " id " --limit 1 --reverse")))

(defun ssb-get (message_id) 
  (shell-command-to-string (concat "sbot get " message_id)))

(defun ssb-get-previous (message_id)
  ; returns previous message id given a message id
  (json-read-from-string (ssb-get (cdr (assoc 'previous message_id)))))

(defun ssb-message-type (message_data) 
  (cdr 
   (assoc 'type 
          (cdr 
           (assoc 'content 
                  (cdr (assoc 'value 
                              (json-read-from-string message_data))))))))

(defun ssb-message-timestamp (message_data)
  (format-time-string "%D %H:%M"
   (cdr (assoc 'timestamp 
               (cdr (assoc 'value (json-read-from-string message_data)))))))

(defun ssb-message-text (message_data)
  (cdr (assoc 'text (cdr (assoc 'content
                          (cdr (assoc 'value 
                                      (json-read-from-string 
                                       message_data))))))))

(defun ssb-decode (message_id) 
  (shell-command-to-string (concat "sbot private.unbox " message_id)))


(defun ssb-display (message_data)
  (with-output-to-temp-buffer "Message"
    (print (concat "Type: " (ssb-message-type message_data)))
    (print (concat  "Time: " (ssb-message-timestamp message_data)))
    (print (ssb-message-text message_data))
    (print message_data)))


(ssb-display (ssb-read-last id))

(defun ssb-publish (text) 
  ; publish a message
  (shell-command-to-string 
   (concat "sbot publish --type post --text \"" text "\"")))

(defun ssb-quick-message (message)
  ; Create a quick message from the minibuffer.  No use of RET, /n only.
  (interactive "sMessage: " )
  (ssb-publish message))


;set keymaps
(global-set-key "\C-s s" 'ssb-start-server)
(global-set-key "\C-s p" 'ssb-publish)
(global-set-key "\C-s c" 'ssb-quick-message)


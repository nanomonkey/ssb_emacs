(require 'json)

(defun ssb-start-server () (shell-command-to-string "sbot server"))

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

(defun ssb-message-type (message_id) 
  (cdr (assoc 'type (cdr (assoc 'content message_id)))))


(defun ssb-decode (message_id) 
  (shell-command-to-string (concat "sbot private.unbox " message_id)))


(defun ssb-publish (text) 
  ; publish a message
  (shell-command-to-string 
   (concat "sbot publish --type post --text \"" text "\"")))


(defun ssb-message (message)
  (interactive "sMessage: " )
  (print message))


;set keymaps
(global-set-key "\C-s s" 'ssb-start-server)
(global-set-key "\C-s p" 'ssb-publish)
(global-set-key "\C-s c" 'ssb-message)


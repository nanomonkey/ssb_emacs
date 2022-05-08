;; Secure Scuttlebutt Major Mode

(defvar ssb-mode-hook nil
 "*List of functions called when entering SSB mode.*")

(defvar ssb-mode-map nil
"Keymap for SSB major mode.")
(if ssb-mode-map
 nil
 (setq ssb-mode-map (make-keymap))
 (define-key ssb-mode-map "\C-c \C-c" ssb-start-server)) 

;(defvar ssb-mode-abbrev-table ...)

(defun display-message (message)
 "message is a plist"
 (let (buffer (generate-new-buffer "*SSB Message*")
       (author (assoc author message))
       (text (assoc text message))
       (time-stamp (assoc timestamp message)))
   (switch-to-buffer buffer)
   (insert author timestamp)
   (insert text)
))

(defun ssb-mode ()
  "Major mode for Secure Scuttlebutt
   Special commands:
  \\{ssb-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'ssb-mode)
  (setq mode-name "SSB")
  (make-local-variable 'ssb-names-file)  
  (setq local-variable "~/.ssb/flume/names.json")
  (use-local-map 'ssb-mode-map) 
  (run-hooks 'ssb-mode-hook)
  (ssb-start-server)
  (ssb-whoami))

(provide 'ssb)  ;; allows users to (require 'name)


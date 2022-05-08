
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Render via Patchfoo   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This is probably the easiest way to get started.
; ssb is rendered locally via Patchfoo, then
; viewed with eww.
; Install patchfoo as a sbot plugin, then run 
; M-x ssb-eww-patchfoo

(defun ssb-start-patchfoo ()
  (interactive)
  (start-process "ssb-patchfoo"
                 "ssb-patchfoo-buffer" 
                 "sbot" "server" "-patchfoo.port" "8027"))

(defun ssb-stop-patchfoo ()
  (interactive)
  (process-send-eof "ssb-patchfoo")
  (kill-buffer "ssb-patchfoo-buffer"))

(defun ssb-check-patchfoo ()
  (if (not (process-status "ssb-patchfoo"))
      (if (y-or-n-p "Patchfoo is not running, start?")
          (progn 
            (ssb-start-patchfoo)
            (sleep-for 1)))))

(defun ssb-eww-patchfoo ()
  (interactive)
  (ssb-check-patchfoo)
  (eww "http://localhost:8027"))



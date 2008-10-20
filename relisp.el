(defconst relisp-terminal-string "__terminal-string__")
(defconst relisp-terminal-regexp (concat relisp-terminal-string "[[:space:]]*"))

(defun relisp-receiver (closure output)
  (message (comment-string-strip 
	    (car (split-string output relisp-terminal-string)) t t)))
  
(setq relisp-controller-process 
      (start-process "relisp-controller" nil "/home/don/src/relisp/relisp_controller"))
(setq relisp-tq 
      (tq-create relisp-controller-process))


(defun ruby-eval (code)
  (let ((message (concat relisp-terminal-string "\n" 
			 code                   "\n" 
			 relisp-terminal-string "\n")))
    (if (equal (process-status relisp-controller-process) 'run)
	(tq-enqueue relisp-tq message relisp-terminal-regexp nil 'relisp-receiver))))
  
(ruby-eval "a = 3")

(ruby-eval "2 + a")


;; (defun relisp-filter-function (process output)
;;   (setq return (eval (read output)))
;;   (if (equal (process-status process) 'run)
;;       (process-send-string process (prin1-to-string return)))
;;   (message (prin1-to-string a))
;;   (accept-process-output relisp-controller-process))

;; (setq relisp-controller-process 
;;       (start-process "relisp-controller" nil "/home/don/src/relisp/relisp_controller" "some_script.rb"))
;; (set-process-filter    relisp-controller-process 'relisp-filter-function)
;; (accept-process-output relisp-controller-process)


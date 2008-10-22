(defun puts (str)
  (message (prin1-to-string str)))

(defun trim-leading-whitespace (str)
  "..."
  (let ((s (if (symbolp str)(symbol-name str) str))
	(whitespace-regexp "\\( \\|\f\\|\t\\|\n\\)"))
    (save-excursion
      (while (and
	      (not (null (string-match (concat "^" whitespace-regexp) s)))
	      (> (length s) (string-match (concat "^" whitespace-regexp) s)))
	(setq s (replace-match "" t nil s))))
    s))

(defun trim-trailing-whitespace (str)
  "..."
  (let ((s (if (symbolp str)(symbol-name str) str))
	(whitespace-regexp "\\( \\|\f\\|\t\\|\n\\)"))
    (save-excursion
      (while (and
	      (not (null (string-match (concat whitespace-regexp "$") s)))
	      (> (length s) (string-match (concat whitespace-regexp "$") s)))
	(setq s (replace-match "" t nil s))))
    s))

(defun chomp (str)
  "..."
  (trim-leading-whitespace (trim-trailing-whitespace str)))

;; relisp

(defconst relisp-over-string "__relisp-over-string__")
(defconst relisp-terminal-string "__relisp-terminal-string__")
(defconst relisp-endofmessage-string (concat "\\(" relisp-over-string "\\|" relisp-terminal-string "\\)" 
					 "[[:space:]]*"))
(defvar relisp-transaction-finished-p 0)

(defun relisp-controller-alive-p nil
  (equal (process-status relisp-controller-process) 'run))
  
(defun relisp-start-controller nil
  (setq relisp-controller-process 
	(start-process "relisp-controller" nil "/home/don/src/relisp/relisp_controller"))
  (setq relisp-tq 
	(tq-create relisp-controller-process)))

(defun ruby-eval (code)
  (unless (relisp-controller-alive-p)
    (relisp-start-controller))
  (let ((message (concat relisp-terminal-string "\n" 
			 relisp-over-string     "\n" 
			 code                   "\n" 
			 relisp-terminal-string "\n")))
    (if (relisp-controller-alive-p)
	(progn
	  (setq relisp-transaction-number 
		(if (boundp 'relisp-transaction-number) 
		    (+ 1 relisp-transaction-number)
		  1))
	  (tq-enqueue relisp-tq message relisp-endofmessage-string relisp-transaction-number 'relisp-receiver)
	  (while (and (relisp-controller-alive-p) 
		      (< relisp-transaction-finished-p relisp-transaction-number))
	    (accept-process-output))))))
  
(defun relisp-receiver (closure output)
  (if (string-match relisp-over-string output)
      (progn
	(setq relisp-ruby-return (chomp (car (split-string output relisp-over-string))))
	(ruby-eval (eval (read relisp-ruby-return))))
    (setq relisp-ruby-return (chomp (car (split-string output relisp-terminal-string)))))
  (setq relisp-transaction-finished-p closure))

(ruby-eval "ruby_method")

;; (ruby-eval "ruby_method")
(puts relisp-ruby-return)

;;; first, some string functions

(defun puts (str)
  (message (prin1-to-string str)))

(defun trim-leading-whitespace (str)
  "Remove leading whitespace characters from STR."
  (let ((s (if (symbolp str)(symbol-name str) str))
	(whitespace-regexp "\\( \\|\f\\|\t\\|\n\\)"))
    (save-excursion
      (while (and
	      (not (null (string-match (concat "^" whitespace-regexp) s)))
	      (> (length s) (string-match (concat "^" whitespace-regexp) s)))
	(setq s (replace-match "" t nil s))))
    s))

(defun trim-trailing-whitespace (str)
  "Remove trailing whitespace characters from STR."
  (let ((s (if (symbolp str)(symbol-name str) str))
	(whitespace-regexp "\\( \\|\f\\|\t\\|\n\\)"))
    (save-excursion
      (while (and
	      (not (null (string-match (concat whitespace-regexp "$") s)))
	      (> (length s) (string-match (concat whitespace-regexp "$") s)))
	(setq s (replace-match "" t nil s))))
    s))

(defun chomp (str)
  "Remove leading and trailing whitespace from STR."
  (trim-leading-whitespace (trim-trailing-whitespace str)))

;;; now relisp stuff

(defvar relisp-transaction-list nil)

(defun relisp-slave-alive-p nil
  (and (boundp 'relisp-slave-process) 
       (equal (process-status relisp-slave-process) 'run)
       (boundp 'relisp-question-code)
       (boundp 'relisp-answer-code)
       (boundp 'relisp-error-code)))

(defun relisp-ruby-slave-path (slave)
  (setq relisp-ruby-slave-path (expand-file-name slave)))

(defvar relisp-slave-name "relisp-slave")

(defun relisp-stop-slave nil
  (if (boundp 'relisp-slave-process)
      (delete-process relisp-slave-process)))

(defun relisp-start-slave nil
  (relisp-stop-slave)
  (setq relisp-transaction-list nil)
  (setq relisp-transaction-number 0)
  (setq relisp-slave-process 
	(start-process relisp-slave-name nil relisp-ruby-slave-path))
  (setq relisp-tq 
	(tq-create relisp-slave-process))
  (makunbound 'relisp-question-code)
  (makunbound 'relisp-answer-code)
  (makunbound 'relisp-error-code)
  (tq-enqueue relisp-tq "\n" "\n" 'relisp-answer-code   'relisp-start-slave-receiver)
  (tq-enqueue relisp-tq "\n" "\n" 'relisp-question-code       'relisp-start-slave-receiver)
  (tq-enqueue relisp-tq "\n" "\n" 'relisp-error-code 'relisp-start-slave-receiver)
  (while (and (equal (process-status relisp-slave-process) 'run)
	      (not (and (boundp 'relisp-question-code)
			(boundp 'relisp-answer-code)
			(boundp 'relisp-error-code))))
    (accept-process-output))
  (relisp-update-endofmessage-regexp)
  relisp-slave-process)

(defun relisp-start-slave-receiver (variable output)
  (set variable (chomp output)))

(defun relisp-update-endofmessage-regexp nil
  (setq relisp-endofmessage-regexp (concat "\\("      relisp-question-code 
					        "\\|" relisp-answer-code 
					        "\\|" relisp-error-code
                                           "\\)" 
					   "[[:space:]]*")))

(defun relisp-new-transaction-number nil
  (if (boundp 'relisp-transaction-number) 
      (setq relisp-transaction-number (+ 1 relisp-transaction-number))
    (setq relisp-transaction-number 1)))

(defun relisp-send-to-ruby (message)
  (let ((tq-num (relisp-new-transaction-number)))
    (push tq-num relisp-transaction-list)
    (if relisp-emacs-master-p
	(progn
	  (tq-enqueue relisp-tq message relisp-endofmessage-regexp nil 'relisp-receiver)
	  (while (and (relisp-slave-alive-p) 
		      (member tq-num relisp-transaction-list))
	    (accept-process-output)))
      (message message)
      (relisp-receiver nil (relisp-accept-process-output)))))

(defun relisp-accept-process-output nil
  (setq input "")
  (setq input-line "")
  (while (null (string-match relisp-endofmessage-regexp input-line))
     (setq input-line (read-from-minibuffer ""))
     (setq input (concat input input-line)))
  input)

(defun relisp-process-ruby-response nil
  (if (boundp 'relisp-ruby-return)
      (if (string-match (concat "\n?" relisp-error-code "[[:space:]]*") relisp-ruby-return)
	  (concat "RUBY ERROR: " (replace-match "" nil t relisp-ruby-return))
	(if (relisp-slave-alive-p)
	    (read (trim-trailing-whitespace relisp-ruby-return))
	  ;; need to raise an error here
	  (concat "relisp-slave is not alive.")))
    "relisp-slave must have died"))

(defun relisp-contact-ruby (message)
  (if (relisp-slave-alive-p)
      (progn
	(relisp-send-to-ruby message)
	(relisp-process-ruby-response))
    nil))

(defun relisp-form-question (code)
  (unless (stringp code)
    (setq code (prin1-to-string code)))
  (concat code "\n" relisp-question-code "\n"))

(defun relisp-form-answer (code)
;;  (unless (stringp code)
    (setq code (prin1-to-string code))
  (concat code "\n" relisp-answer-code "\n"))

(defun ruby-eval (ruby-code)
  (relisp-contact-ruby (relisp-form-question ruby-code)))

(defun relisp-answer-ruby (question)
  (setq question (read (chomp (car (split-string question relisp-question-code)))))
  (relisp-contact-ruby (relisp-form-answer (eval question))))

(defun relisp-receiver (closure message)
  (makunbound 'relisp-ruby-return)
  (if (string-match relisp-question-code message)
      (relisp-answer-ruby message)
    (setq return-val (chomp (car (split-string message relisp-answer-code)))))
  (pop relisp-transaction-list)
  ;; if a deeper (recursive) level has set this, leave it alone
  (puts (boundp 'relisp-ruby-return))
  (unless (boundp 'relisp-ruby-return)
    (setq relisp-ruby-return return-val)))

(defvar relisp-emacs-master-p t)

(defun relisp-become-slave nil
  (setq relisp-emacs-master-p nil)
  ;; redefine ruby-eval for this condition
  
  ;; get constants
  (message "\n")
  (setq relisp-answer-code (read-from-minibuffer ""))
  (message "\n")
  (setq relisp-question-code (read-from-minibuffer ""))
  (message "\n")
  (setq relisp-error-code (read-from-minibuffer ""))
  (loop 
   (setq input "")
   (setq input-line "")
   (while (null (string-match relisp-question-code input-line))
     (setq input-line (read-from-minibuffer ""))
     (setq input (concat input input-line)))
   (setq question (read (chomp (car (split-string input-line relisp-question-code)))))
   (message (relisp-form-answer (eval question)))))
     



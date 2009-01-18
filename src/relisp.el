;;; first, some string functions

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

(defun strip (str)
  "Remove leading and trailing whitespace from STR."
  (trim-leading-whitespace (trim-trailing-whitespace str)))

;;; now relisp stuff

(defvar relisp-buffer-name "*Relisp*")

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
  (if (and (boundp 'relisp-ruby-slave-path) (file-exists-p relisp-ruby-slave-path))
      (setq relisp-slave-process (start-process relisp-slave-name nil "ruby" relisp-ruby-slave-path))
    (setq relisp-slave-process (start-process relisp-slave-name nil
					      "ruby" 
					      "-x"))
    (process-send-string relisp-slave-name 
			 (concat "#! ruby \n"
				 "$:.unshift File.join(File.dirname('" (symbol-file 'relisp-transaction-list) "'), '../lib')\n"
				 "require 'relisp'\n" 
				 "Relisp::RubySlave.new.start\n"
				 "__END__\n")))
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
  (set variable (strip output)))

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
	  (tq-enqueue relisp-tq (concat message "\n") relisp-endofmessage-regexp nil 'relisp-receiver)
	  (while (and (relisp-slave-alive-p) 
		      (member tq-num relisp-transaction-list))
	    (accept-process-output)))
      (message message)
      (relisp-receiver nil (relisp-accept-process-output)))))

(defun relisp-accept-process-output nil
  (setq output "")
  (setq output-line "")
  (while (null (string-match relisp-endofmessage-regexp output-line))
     (setq output-line (read-from-minibuffer ""))
     (setq output (concat output output-line)))
  output)

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
  (concat code "\n" relisp-question-code))

(defun relisp-form-answer (code)
;;  (unless (stringp code)
    (setq code (prin1-to-string code))
  (concat code "\n" relisp-answer-code))

(defun relisp-log (text)
  (get-buffer-create relisp-buffer-name)
  (unless (string-match relisp-endofmessage-regexp (strip text))
    (set-buffer relisp-buffer-name)
    (end-of-buffer)
    (insert text "\n")))


(defun ruby-eval (ruby-code)
  (relisp-log (concat "relisp> " ruby-code))
  (relisp-contact-ruby (relisp-form-question ruby-code)))

(defun relisp-answer-ruby (question)
  (setq question (strip (car (split-string question relisp-question-code))))
  (relisp-log (concat "?=> " question))
  (setq question (read question))
  (setq relisp-eval-result (eval question))
  (relisp-log (concat "relisp> " (prin1-to-string relisp-eval-result)))
  (relisp-contact-ruby (relisp-form-answer relisp-eval-result)))

(defun relisp-receiver (closure message)
  (makunbound 'relisp-ruby-return)
  (if (string-match relisp-question-code message)
      (relisp-answer-ruby message)
    (setq return-val (strip (car (split-string message relisp-answer-code))))
    (relisp-log (concat "=> " return-val)))
  (pop relisp-transaction-list)
  ;; if a deeper (recursive) level has set this, leave it alone
  (unless (boundp 'relisp-ruby-return)
    (setq relisp-ruby-return return-val)))

(defvar relisp-emacs-master-p t)

(defun relisp-become-slave nil
  (setq relisp-emacs-master-p nil)
  ;; get constants
  (message "SEND CONSTANTS")
  (message "(prompt for answer code)")
  (setq relisp-answer-code (read-from-minibuffer ""))
  (message "(prompt for question code)")
  (setq relisp-question-code (read-from-minibuffer ""))
  (message "(prompt for error code)")
  (setq relisp-error-code (read-from-minibuffer ""))
  (relisp-update-endofmessage-regexp)
  (while (equal 1 1) ;; loop is only a CL function, I guess
   (setq input "")
   (setq input-line "")
   (while (null (string-match relisp-question-code (strip input-line)))
     (setq input-line (read-from-minibuffer ""))
     (setq input (concat input input-line)))
   (setq question (read (strip (car (split-string input relisp-question-code)))))
   (setq relisp-eval-result (eval question))
   (message (relisp-form-answer relisp-eval-result))))
     

(provide 'relisp)

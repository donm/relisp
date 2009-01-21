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

(defvar relisp-slave-name "relisp-slave")
(defvar relisp-buffer-name "*Relisp*")
(defvar relisp-emacs-master-p t)

(defun relisp-slave-alive-p nil
  (and (boundp 'relisp-slave-process) 
       (equal (process-status relisp-slave-process) 'run)
       (boundp 'relisp-question-code)
       (boundp 'relisp-answer-code)
       (boundp 'relisp-error-code)))

(defun relisp-ruby-slave-path (slave)
  (setq relisp-ruby-slave-path (expand-file-name slave)))

(defun relisp-filter-slave-output (process output-line)
  (setq relisp-ruby-output (concat relisp-ruby-output output-line)))

(defun relisp-stop-slave nil
  (if (boundp 'relisp-slave-process)
      (delete-process relisp-slave-process)))

(defun relisp-start-slave nil
  (setq relisp-emacs-master-p t)
  (relisp-stop-slave)
  (setq relisp-ruby-output "")
  (if (and (boundp 'relisp-ruby-slave-path) (file-exists-p relisp-ruby-slave-path))
      (setq relisp-slave-process (start-process relisp-slave-name nil "ruby" relisp-ruby-slave-path))
    (setq relisp-slave-process (start-process relisp-slave-name nil
					      "ruby" 
					      "-x"))
    (process-send-string relisp-slave-name 
			 (concat "#! ruby \n"
				 "$:.unshift File.join(File.dirname('" (symbol-file 'relisp-slave-name) "'), '../lib')\n"
				 "require 'relisp'\n" 
				 "Relisp::RubySlave.new.start\n"
				 "__END__\n")))
  (set-process-filter relisp-slave-process 'relisp-filter-slave-output)
  (relisp-write-to-ruby "")
  (while (null (string-match "\n" relisp-ruby-output))
    (accept-process-output))
  (setq relisp-answer-code (strip relisp-ruby-output))
  (setq relisp-ruby-output "")
  (relisp-write-to-ruby "")
  (while (null (string-match "\n" relisp-ruby-output))
    (accept-process-output))
  (setq relisp-question-code (strip relisp-ruby-output))
  (setq relisp-ruby-output "")
  (relisp-write-to-ruby "")
  (while (null (string-match "\n" relisp-ruby-output))
    (accept-process-output))
  (setq relisp-error-code (strip relisp-ruby-output))
  (setq relisp-ruby-output "")
  (relisp-update-endofmessage-regexp)
  relisp-slave-process)

(defun relisp-update-endofmessage-regexp nil
  (setq relisp-endofmessage-regexp (concat "\\("      relisp-question-code 
					        "\\|" relisp-answer-code 
					        "\\|" relisp-error-code
                                           "\\)" 
					   "[[:space:]]*")))

;; (defun relisp-new-transaction-number nil
;;   (if (boundp 'relisp-transaction-number) 
;;       (setq relisp-transaction-number (+ 1 relisp-transaction-number))
;;     (setq relisp-transaction-number 1)))


(defun relisp-write-to-ruby (message)
  (if relisp-emacs-master-p
      (process-send-string relisp-slave-process (concat message "\n"))
    (message message)))

(defun relisp-read-from-ruby nil
  (if relisp-emacs-master-p
      (relisp-accept-slave-output)
    (relisp-accept-master-output)))
    
(defun relisp-accept-slave-output nil
  (while (and (relisp-slave-alive-p) 
	      (null (string-match relisp-endofmessage-regexp 
				  relisp-ruby-output)))
    (accept-process-output))
  (let ((val relisp-ruby-output))
    (setq relisp-ruby-output "")
    val))

(defun relisp-accept-master-output nil
  (setq output "")
  (setq output-line "")
  (while (null (string-match relisp-endofmessage-regexp output-line))
     (setq output-line (read-from-minibuffer ""))
     (setq output (concat output output-line)))
  output)

(defun relisp-form-question (code)
  (unless (stringp code)
    (setq code (prin1-to-string code)))
  (concat code "\n" relisp-question-code))

(defun relisp-form-answer (code)
;;  (unless (stringp code)
    (setq code (prin1-to-string code))
  (concat code "\n" relisp-answer-code))

(defun relisp-log (text)
  (if relisp-emacs-master-p
      (progn
	(get-buffer-create relisp-buffer-name)
	(unless (string-match relisp-endofmessage-regexp (strip text))
	  (set-buffer relisp-buffer-name)
	  (goto-char (point-max))
	  (insert text "\n")))))

(defun ruby-eval (ruby-code)
  (if (and relisp-emacs-master-p (not (relisp-slave-alive-p)))
      (relisp-start-slave))
  (relisp-log (concat "lisp?> " ruby-code))
  (relisp-write-to-ruby (relisp-form-question ruby-code))
  (setq message (relisp-read-from-ruby))
  (while (string-match relisp-question-code message)
    (relisp-answer-ruby message)
    (setq message (relisp-read-from-ruby)))
  (setq relisp-ruby-return (strip (car (split-string message relisp-answer-code))))
  (relisp-log (concat "ruby=> " relisp-ruby-return "\n"))
  (if (string-match (concat "\n?" relisp-error-code "[[:space:]]*") relisp-ruby-return)
      (concat "RUBY ERROR: " (replace-match "" nil t relisp-ruby-return))
    (eval (read (trim-trailing-whitespace relisp-ruby-return)))))

(defun relisp-answer-ruby (question)
  (setq question (strip (car (split-string question relisp-question-code))))
  (relisp-log (concat "ruby?> " question))
  (setq question (read question))
  (setq relisp-eval-result (eval question))
  (relisp-log (concat "lisp=> " (prin1-to-string relisp-eval-result)))
  (relisp-write-to-ruby (relisp-form-answer relisp-eval-result))
  (pop relisp-transaction-list))

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

(defun relisp-to-ruby (object)
  (let ((var (read (ruby-eval "new_elisp_variable"))))
    (set var object)
    (concat "elisp_eval('" (prin1-to-string var) "')")))



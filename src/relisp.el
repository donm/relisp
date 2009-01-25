;;; relisp.el --- library for ruby/elisp interaction

;; Copyright (C) 2009 <don@ohspite.net>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA


;;; TODO:
;; signal errors when ruby returns an error?


;;; first, some general string functions

(defun relisp-trim-leading-whitespace (str)
  "Remove leading whitespace characters from STR."
  (let ((s (if (symbolp str)(symbol-name str) str))
	(whitespace-regexp "\\( \\|\f\\|\t\\|\n\\)"))
    (save-excursion
      (while (and
	      (not (null (string-match (concat "^" whitespace-regexp) s)))
	      (> (length s) (string-match (concat "^" whitespace-regexp) s)))
	(setq s (replace-match "" t nil s))))
    s))

(defun relisp-trim-trailing-whitespace (str)
  "Remove trailing whitespace characters from STR."
  (let ((s (if (symbolp str)(symbol-name str) str))
	(whitespace-regexp "\\( \\|\f\\|\t\\|\n\\)"))
    (save-excursion
      (while (and
	      (not (null (string-match (concat whitespace-regexp "$") s)))
	      (> (length s) (string-match (concat whitespace-regexp "$") s)))
	(setq s (replace-match "" t nil s))))
    s))

(defun relisp-strip (str)
  "Remove leading and trailing whitespace from STR."
  (relisp-trim-leading-whitespace (relisp-trim-trailing-whitespace str)))

;;; now real stuff

(defvar relisp-slave-name "relisp-slave" "Name of the relisp ruby slave process.")
(defvar relisp-buffer-name "*Relisp*" "Name of the relisp output buffer.")

(defun relisp-log (text)
  "Insert TEXT at the end of `relisp-buffer-name', unless emacs is the slave."
  (when relisp-emacs-master-p
    (get-buffer-create relisp-buffer-name)
    (unless (string-match (relisp-endofmessage-regexp) (relisp-strip text))
      (set-buffer relisp-buffer-name)
      (goto-char (point-max))
      (insert text "\n"))))

(defun relisp-write-to-ruby (message)
  "Send MESSAGE to the ruby process."
  (if relisp-emacs-master-p
      (process-send-string relisp-slave-process (concat message "\n"))
    (message message)))

(defun relisp-read-from-ruby nil
  "Accept ruby input, stopping at a match to `relisp-endofmessage-regexp'."
  (if relisp-emacs-master-p
      (relisp-accept-slave-output)
    (relisp-accept-master-output)))
    
(defun relisp-accept-slave-output nil
  "Accept a full ruby message when ruby is the slave process."
  (while (and (relisp-slave-alive-p) 
	      (null (string-match (relisp-endofmessage-regexp)
				  relisp-ruby-output)))
    (accept-process-output))
  (let ((val relisp-ruby-output))
    (setq relisp-ruby-output "")
    val))

(defun relisp-accept-master-output nil
  "Accept a full ruby message when emacs is the slave process."
  (let ((output "") 
	(output-line ""))
    (while (null (string-match (relisp-endofmessage-regexp) output-line))
      (setq output-line (read-from-minibuffer ""))
      (setq output (concat output output-line)))
    output))

(defun relisp-endofmessage-regexp nil
  "Return a regexp that matches codes indicating a message termination."
  (concat "\\(" relisp-question-code 
	  "\\|" relisp-answer-code 
	  "\\|" relisp-error-code
	  "\\)" 
	  "[[:space:]]*"))

(defun ruby-eval (ruby-code)
  "Have ruby evaluate RUBY-CODE and return the result.
The result is an elisp object equivalent to the ruby result of
RUBY-CODE.  Reads input from the minibuffer unless an argument is
given."
  (interactive "sruby> ")
  (let (message result)
    (if (and relisp-emacs-master-p (not (relisp-slave-alive-p)))
	(relisp-start-slave))
    (relisp-log (concat "lisp?> " ruby-code))
    (relisp-write-to-ruby (relisp-form-question ruby-code))
    (setq message (relisp-read-from-ruby))
    (while (string-match relisp-question-code message)
      (relisp-answer-ruby message)
      (setq message (relisp-read-from-ruby)))
    (setq relisp-ruby-return (relisp-strip (car (split-string message relisp-answer-code))))
    (relisp-log (concat "ruby=> " relisp-ruby-return "\n"))
    (setq result (if (string-match (concat "\n?" relisp-error-code "[[:space:]]*") relisp-ruby-return)
		     (concat "RUBY ERROR: " (replace-match "" nil t relisp-ruby-return))
		   (eval (read (relisp-trim-trailing-whitespace relisp-ruby-return)))))
    (if (interactive-p) 
	(message (prin1-to-string result)))
    result))

(defun relisp-answer-ruby (question)
  "Evaluate the QUESTION from ruby and send ruby the result."
  (setq question (relisp-strip (car (split-string question relisp-question-code))))
  (relisp-log (concat "ruby?> " question))
  (setq question (read question))
  (condition-case error-description
      (progn 
	(set relisp-previous-result (eval question))
	(relisp-log (concat "lisp=> " (prin1-to-string (type-of (eval relisp-previous-result)))))
	(relisp-log (concat " ...   " (prin1-to-string (eval relisp-previous-result))))
	(relisp-write-to-ruby (prin1-to-string (type-of (eval relisp-previous-result))))
	(relisp-write-to-ruby (relisp-form-answer (eval relisp-previous-result))))
    (error (relisp-write-to-ruby 
	    (relisp-form-error
	     (error-message-string error-description))))))

(defun relisp-form-question (code)
  "Return a string that tells ruby to evaluate CODE."
  (unless (stringp code)
    (setq code (prin1-to-string code)))
  (concat code "\n" relisp-question-code))

(defun relisp-form-answer (code)
  "Return a string that tells ruby that CODE is the answer to its query."
  (setq code (prin1-to-string code))
  (concat code "\n" relisp-answer-code))

(defun relisp-form-error (code)
  "Return a string that tells ruby that CODE is an error message."
  (setq code (prin1-to-string code))
  (concat code "\n" relisp-error-code))

(defun relisp-to-ruby (object)
  "Return a string that, when evaluated in ruby, results in OBJECT."
  (let ((var (ruby-eval "new_elisp_variable")))
    (set var object)
    (concat "elisp_eval('" (prin1-to-string var) "')")))

(defun relisp-start-slave nil
  "Start a ruby slave process to do emacs's bidding.
If `relisp-ruby-slave-path' is bound, then that file is read and
the Relisp::RubySlave object must be started there.  Otherwise
emacs starts a ruby process and starts a RubySlave on its own."
  (interactive)
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
  (set-process-filter relisp-slave-process 'relisp-slave-output-filter)
  (relisp-write-to-ruby "")
  (while (null (string-match "\n" relisp-ruby-output))
    (accept-process-output))
  (setq relisp-answer-code (relisp-strip relisp-ruby-output))
  (setq relisp-ruby-output "")
  (relisp-write-to-ruby "")
  (while (null (string-match "\n" relisp-ruby-output))
    (accept-process-output))
  (setq relisp-question-code (relisp-strip relisp-ruby-output))
  (setq relisp-ruby-output "")
  (relisp-write-to-ruby "")
  (while (null (string-match "\n" relisp-ruby-output))
    (accept-process-output))
  (setq relisp-error-code (relisp-strip relisp-ruby-output))
  (setq relisp-ruby-output "")
  (relisp-write-to-ruby "")
  (while (null (string-match "\n" relisp-ruby-output))
    (accept-process-output))
  (setq relisp-previous-result (read (relisp-strip relisp-ruby-output)))
  (setq relisp-ruby-output "")
  relisp-slave-process)

(defun relisp-become-slave nil
  "Convert the emacs process into a slave.  Only called by ruby."
  (setq relisp-emacs-master-p nil)
  ;; get constants
  (message "SEND CONSTANTS")
  (message "(prompt for answer code)")
  (setq relisp-answer-code (read-from-minibuffer ""))
  (message "(prompt for question code)")
  (setq relisp-question-code (read-from-minibuffer ""))
  (message "(prompt for error code)")
  (setq relisp-error-code (read-from-minibuffer ""))
  (message "(prompt for previous result variable)")
  (setq relisp-previous-result (read (read-from-minibuffer "")))
  (let (input input-line)
    (while t ;; loop is only a CL function, I guess
      (setq input "")
      (setq input-line "")
      (while (null (string-match relisp-question-code (relisp-strip input-line)))
	(setq input-line (read-from-minibuffer ""))
	(setq input (concat input input-line)))
      (relisp-answer-ruby input))))


(defun relisp-slave-output-filter (process output-line)
  "Listen to PROCESS and add each OUTPUT-LINE to `relisp-ruby-output'."
  (setq relisp-ruby-output (concat relisp-ruby-output output-line)))

(defun relisp-stop-slave nil
  "Kill the ruby slave process."
  (if (boundp 'relisp-slave-process)
      (delete-process relisp-slave-process)))

(defun relisp-slave-alive-p nil
  "Return t if the ruby slave is alive and well."
  (and (boundp 'relisp-slave-process) 
       (equal (process-status relisp-slave-process) 'run)
       (boundp 'relisp-question-code)
       (boundp 'relisp-answer-code)
       (boundp 'relisp-error-code)))

(provide 'relisp)

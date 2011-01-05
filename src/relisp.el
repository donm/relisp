;;; relisp.el --- library for ruby/elisp interaction

;; Copyright (C) 2009, 2010 Don March

;; Author: Don March <don@ohspite.net>
;; Created: 2009-01-25
;; Version: 1.1.0
;; Keywords: ruby emacs elisp bridge

;; This file is part of Relisp.

;; Relisp is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; Relisp is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Code: 

(defvar relisp-slave-name "relisp-slave" 
  "Name of the relisp ruby slave process.")
(defvar relisp-buffer-name "*Relisp*" 
  "Name of the relisp output buffer.")
(defvar relisp-endofmessage-regexp nil
  "A regexp that matches codes indicating a message termination.")

;; to prohibit free variable warnings
(defvar relisp-emacs-master-p t)
(defvar relisp-slave-process)
(defvar relisp-begin-answer-code)
(defvar relisp-answer-code)
(defvar relisp-question-code)
(defvar relisp-command-code)
(defvar relisp-error-code)
(defvar relisp-begin-slave-code)
(defvar relisp-previous-result)

(put 'relisp-ruby-error 
     'error-conditions '(error relisp-ruby-error))
(put 'relisp-ruby-error
     'error-message "Error in ruby process")

(defun relisp-ruby-send (message &optional process)
  "Send MESSAGE to the slave or master ruby PROCESS.
PROCESS defaults to `relisp-slave-process' when nil."
  (if relisp-emacs-master-p
      (progn 
	(or process (setq process relisp-slave-process))
	(process-send-string process (concat message "\n")))
    (message message)))

(defun relisp-ruby-slave-read (&optional process terminator-regexp)
  "Accept a full message from ruby slave."
  (or process (setq process relisp-slave-process))
  (or terminator-regexp
      (setq terminator-regexp relisp-endofmessage-regexp))
  (let ((buffer (process-buffer process)))
    (with-current-buffer buffer
      (delete-region (point-min) (point-max)))
    (while (and (relisp-slave-alive-p process)
		(not (string-match terminator-regexp
				   (with-current-buffer buffer (buffer-string)))))
      (accept-process-output process nil nil t))
    (with-current-buffer buffer (buffer-string))))

(defun relisp-ruby-master-read (&optional terminator-regexp)
  "Accept a full message from ruby master."
  (or terminator-regexp
      (setq terminator-regexp relisp-endofmessage-regexp))
  (let ((output ""))
    (while (not (string-match terminator-regexp output))
      (setq output (concat output (read-from-minibuffer ""))))
    output))

(defun relisp-ruby-read (&optional process terminator-regexp)
  "Accept ruby message, stopping at a match to TERMINATOR-REGEXP."
  (if relisp-emacs-master-p
      (progn 
	(relisp-ruby-slave-read process terminator-regexp))
    (relisp-ruby-master-read terminator-regexp)))

;; From here on out, no more process stuff

(defun relisp-strip (str &optional side)
  "Return STR stripped of leading and/or trailing whitespace.

If SIDE is 'start (or 'leading) or 'end (or 'trailing), only trim
whitespace on that side of the string."  
  (when (not (memq side '(end trailing)))
    (when (string-match "^[\n\t\r\f\v ]+" str)
      (setq str (replace-match "" nil nil str))))
  (when (not (memq side '(start leading)))
    (when (string-match "[\n\t\r\f\v ]+$" str)
      (setq str (replace-match "" nil nil str))))
  str)

(defun relisp-log (&optional text)
  "Insert TEXT at the end of the log buffer, when emacs is mater."
  (or text (setq text ""))
  (when (and relisp-emacs-master-p
	     (if (and (boundp 'relisp-endofmessage-regexp)
		      relisp-endofmessage-regexp)
		 (and (not (string-match relisp-endofmessage-regexp (relisp-strip text)))
		      (not (string-match relisp-begin-slave-code (relisp-strip text))))
	       t)
      (with-current-buffer (get-buffer-create relisp-buffer-name)
	(goto-char (point-max))
	(insert text "\n")))))

;; relisp-form-command and -question convert the argument to a string,
;; if necessary, to catch end cases like `nil', but -form-answer and
;; -error always take raw code as arguments.
(defun relisp-form-command (code)
  "Return a string that tells ruby to evaluate CODE."
  (unless (stringp code)
    (setq code (prin1-to-string code)))
  (concat code "\n" relisp-command-code))

(defun relisp-form-question (code)
  "Return a string that tells ruby to evaluate CODE and return the result."
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
  "Return a string that, when evaluated in ruby, results in OBJECT.
This function is deprecated."
  (let ((var (ruby-eval "new_elisp_variable")))
    (set var object)
    (concat "elisp_eval('" (prin1-to-string var) "')")))

(defun relisp-ruby-receive (&optional process safe)
  "Handle messages from emacs after ruby-eval or ruby-exec are called."
  (let ((message (relisp-ruby-read process))
	question)
    (while (or (setq questionp (string-match relisp-question-code message))
	       (string-match relisp-command-code  message))
      (relisp-exec (relisp-strip (car (split-string message relisp-endofmessage-regexp)))
		   questionp)
      (setq message (relisp-ruby-read process)))
    (setq message (relisp-strip (car (split-string message relisp-answer-code))))
    (relisp-log (concat "ruby=> " message "\n"))
    (when (and (not safe) (string-match relisp-error-code message))
      (signal 'relisp-ruby-error (list (replace-match "" nil t message))))
    message))

(defun ruby-exec (ruby-code &optional process return)
  "Have ruby evaluate RUBY-CODE, returning result if RETURN is non-nil.
The result is an elisp object equivalent to the ruby result of
RUBY-CODE.  Reads input from the minibuffer unless an argument is
given."
  (interactive "Mruby> ")
  (let (result)
    (if (and relisp-emacs-master-p (not (relisp-slave-alive-p)))
	(relisp-start-slave))
    (relisp-log (concat "lisp" 
			(if return "?" "!")
			"> " ruby-code))
    (relisp-ruby-send (if return
			  (relisp-form-question ruby-code)
			(relisp-form-command ruby-code))
		      process)
    (setq result (if return (eval (read (relisp-ruby-receive process)))
		   (relisp-ruby-receive process)))
    (when (and return (interactive-p))
      (message (prin1-to-string result)))
    result))

(defun ruby-eval (ruby-code &optional process)
  "Have ruby evaluate RUBY-CODE and return the result."
  (ruby-exec ruby-code process t))

(defun relisp-exec (lisp-code &optional return)
  "Evaluate the LISP-CODE from ruby.
Send ruby the result if RETURN is non-nil."
  (relisp-log (concat (if return "ruby?> " "ruby! ")
		      lisp-code))
  (setq lisp-code (read lisp-code))
  (condition-case error-description
      (progn
	(set relisp-previous-result (eval lisp-code))
	(if (not return)
	    (relisp-ruby-send (relisp-form-answer nil))
	  (relisp-log (concat "lisp=> " 
			      (prin1-to-string (type-of (symbol-value relisp-previous-result)))))
	  (relisp-log (concat "    => " 
			      (prin1-to-string (symbol-value relisp-previous-result))))
	  (relisp-ruby-send relisp-begin-answer-code)
	  (relisp-ruby-send (prin1-to-string (type-of (symbol-value relisp-previous-result))))
	  (relisp-ruby-send (relisp-form-answer (symbol-value relisp-previous-result)))))
    (error (relisp-ruby-send 
	    (relisp-form-error
	     (error-message-string error-description))))))

(defun relisp-eval (lisp-code)
  "Evaluate the LISP-CODE from ruby and send ruby the result."
  (relisp-exec lisp-code t))

(defun relisp-get-constant nil
  "Return the next constant passed from ruby.
Intended to be called from relisp-get-constants."
  (relisp-ruby-send "(prompt)")
  (relisp-strip (relisp-ruby-read nil 
				  (if relisp-emacs-master-p 
				      "\n" 
				    "."))))

(defun relisp-get-constants nil
  "Sets all relisp constants shared between ruby and emacs.
Intended to be called from relisp-start-slave and
relisp-become-slave."
  (when (not relisp-emacs-master-p)
    (relisp-ruby-send "SEND CONSTANTS"))
  (setq relisp-question-code     (relisp-get-constant)
	relisp-command-code      (relisp-get-constant)
	relisp-begin-answer-code (relisp-get-constant)
	relisp-answer-code       (relisp-get-constant)
	relisp-error-code        (relisp-get-constant)
	relisp-begin-slave-code  (relisp-get-constant)
	relisp-previous-result   (read (relisp-get-constant)))
  (relisp-ruby-send "(got constants)")
  (setq relisp-endofmessage-regexp 
	(concat relisp-question-code "\\|" 
		relisp-command-code "\\|" 
		relisp-answer-code "\\|" 
		relisp-error-code)))

(defun relisp-start-slave (&optional slave-path new)
  "Start a ruby slave process to do emacs's bidding.
If SLAVE-PATH is given, then that Ruby file is read and the
Relisp::RubySlave object must be started in that file.  Otherwise
emacs starts a ruby process and starts a RubySlave on its own."
  (interactive)
  (setq relisp-emacs-master-p t)
  (unless new (relisp-stop-slave))
  (if (and slave-path 
	   (not (string= slave-path "")))
      (if (file-exists-p slave-path)
	  (setq relisp-slave-process (start-process relisp-slave-name nil "ruby" slave-path))
	(error (concat "Ruby slave does not exist: " slave-path)))
    (setq relisp-slave-process (start-process relisp-slave-name nil
					      "ruby" 
					      "-rubygems"
					      "-x"))
    (process-send-string relisp-slave-process
			 ; with `-x' ruby ignores everything until a
			 ; line with `#! ruby'.
			 (concat "#! ruby"
				 ;; something really weird is going on
				 ;; with the bindings here; if you
				 ;; comment out any of the next 3
				 ;; lines then $: isn't set
				 ;; properly. `$__RELISP__' is a
				 ;; throwaway variable.
				 "$:.unshift File.join(File.dirname('" (symbol-file 'relisp-slave-name) "'), '../lib')\n"
				 "$__RELISP__=$:\n"
				 "$__RELISP__.unshift File.join(File.dirname('" (symbol-file 'relisp-slave-name) "'), '../lib')\n"
				 "require 'relisp'\n" 
				 "Relisp::RubySlave.new.start\n"
				 "__END__\n")))
  (set-process-buffer relisp-slave-process 
		      (generate-new-buffer (concat " " (process-name relisp-slave-process))))
  (relisp-get-constants)
  (relisp-log (concat "started: " (prin1-to-string relisp-slave-process)))
  ; gobble up initial ruby messages and respond 
  (while (and (relisp-slave-alive-p) 
	      (not (string= relisp-begin-slave-code
			    (relisp-ruby-receive relisp-slave-process)))))
  (if (relisp-slave-alive-p)
      (relisp-log (concat "startup finished: " (prin1-to-string relisp-slave-process)))
    (relisp-log (concat "finished: " (prin1-to-string relisp-slave-process))))
  relisp-slave-process)

(defun relisp-become-slave nil
  "Convert the emacs process into a slave.  Only called by ruby."
  (setq relisp-emacs-master-p nil)
  (relisp-get-constants)
  (while t ;; loop is only a CL function, I guess
    (relisp-ruby-receive nil t)))

(defun relisp-stop-slave (&optional process)
  "Kill the ruby slave PROCESS."
  (interactive)
  (or process (if (boundp 'relisp-slave-process) 
		  (setq process relisp-slave-process)))
  (when (and process
	     (relisp-slave-alive-p process))
    (kill-buffer (process-buffer process))
    (relisp-log (concat "stopped: " (prin1-to-string process)))
    (delete-process process)))
      
(defun relisp-slave-alive-p (&optional process)
  "Return whether the ruby slave PROCESS is alive and well."
  (if process
      (equal (process-status process) 'run)
    (and (boundp 'relisp-slave-process)
	 (equal (process-status relisp-slave-process) 'run))))

(provide 'relisp)

;;; relisp.el ends here

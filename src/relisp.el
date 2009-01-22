;;; lisp-mnt.el --- minor mode for Emacs Lisp maintainers

;; Copyright (C) 2009 <don@ohspite.net>

;; Author: Eric S. Raymond <esr@snark.thyrsus.com>
;; Created: 14 Jul 1992
;; Version: 1.2
;; Keywords: docs

;; This file is part of GNU Emacs.

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


;; check for errors

;;   * When you encounter an error condition, call the function `error'
;;     (or `signal').  The function `error' does not return.  *Note
;;     Signaling Errors::.

;;   * An error message should start with a capital letter but should not
;;     end with a period.

;; make ruby-eval interactive and accept input from minibuffer




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

(defvar relisp-slave-name "relisp-slave")
(defvar relisp-buffer-name "*Relisp*")
(defvar relisp-emacs-master-p t)

(defun relisp-log (text)
  (if relisp-emacs-master-p
      (progn
	(get-buffer-create relisp-buffer-name)
	(unless (string-match (relisp-endofmessage-regexp) (relisp-strip text))
	  (set-buffer relisp-buffer-name)
	  (goto-char (point-max))
	  (insert text "\n")))))

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
	      (null (string-match (relisp-endofmessage-regexp)
				  relisp-ruby-output)))
    (accept-process-output))
  (let ((val relisp-ruby-output))
    (setq relisp-ruby-output "")
    val))

(defun relisp-accept-master-output nil
  (setq output "")
  (setq output-line "")
  (while (null (string-match (relisp-endofmessage-regexp) output-line))
     (setq output-line (read-from-minibuffer ""))
     (setq output (concat output output-line)))
  output)

(defun relisp-endofmessage-regexp nil
  (concat "\\(" relisp-question-code 
	  "\\|" relisp-answer-code 
	  "\\|" relisp-error-code
	  "\\)" 
	  "[[:space:]]*"))

(defun ruby-eval (ruby-code)
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
  (if (string-match (concat "\n?" relisp-error-code "[[:space:]]*") relisp-ruby-return)
      (concat "RUBY ERROR: " (replace-match "" nil t relisp-ruby-return))
    (eval (read (relisp-trim-trailing-whitespace relisp-ruby-return)))))

(defun relisp-answer-ruby (question)
  (setq question (relisp-strip (car (split-string question relisp-question-code))))
  (if relisp-emacs-master-p
      (relisp-log (concat "ruby?> " question)))
  (setq question (read question))
  (set relisp-previous-result (eval question))
  (if relisp-emacs-master-p
      (progn
	(relisp-log (concat "lisp=> " (prin1-to-string (type-of (eval relisp-previous-result)))))
	(relisp-log (concat " ...   " (prin1-to-string (eval relisp-previous-result))))))
  (relisp-write-to-ruby (prin1-to-string (type-of (eval relisp-previous-result))))
  (relisp-write-to-ruby (relisp-form-answer (eval relisp-previous-result))))

(defun relisp-form-question (code)
  (unless (stringp code)
    (setq code (prin1-to-string code)))
  (concat code "\n" relisp-question-code))

(defun relisp-form-answer (code)
;;  (unless (stringp code)
    (setq code (prin1-to-string code))
  (concat code "\n" relisp-answer-code))

(defun relisp-to-ruby (object)
  (let ((var (ruby-eval "new_elisp_variable")))
    (set var object)
    (concat "elisp_eval('" (prin1-to-string var) "')")))

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
  (while (equal 1 1) ;; loop is only a CL function, I guess
   (setq input "")
   (setq input-line "")
   (while (null (string-match relisp-question-code (relisp-strip input-line)))
     (setq input-line (read-from-minibuffer ""))
     (setq input (concat input input-line)))
   (relisp-answer-ruby input)))

(defun relisp-slave-output-filter (process output-line)
  (setq relisp-ruby-output (concat relisp-ruby-output output-line)))

(defun relisp-stop-slave nil
  (if (boundp 'relisp-slave-process)
      (delete-process relisp-slave-process)))

(defun relisp-slave-alive-p nil
  (and (boundp 'relisp-slave-process) 
       (equal (process-status relisp-slave-process) 'run)
       (boundp 'relisp-question-code)
       (boundp 'relisp-answer-code)
       (boundp 'relisp-error-code)))

(provide 'relisp)

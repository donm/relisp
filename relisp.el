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

;; relisp

(defvar relisp-transaction-list nil)

(defun relisp-controller-alive-p nil
  (and (boundp 'relisp-controller-process) 
       (equal (process-status relisp-controller-process) 'run)
       (boundp 'relisp-over-string)
       (boundp 'relisp-terminal-string)
       (boundp 'relisp-ruby-error-string)))

(defun relisp-ruby-controller (controller)
  (setq relisp-ruby-controller controller))

(defun relisp-start-controller nil
  (if (boundp 'relisp-tq)
      (tq-close relisp-tq))
  (setq relisp-transaction-list nil)
  (setq relisp-transaction-number 0)
  (setq relisp-controller-process 
	(start-process "relisp-controller" nil relisp-ruby-controller))
  (setq relisp-tq 
	(tq-create relisp-controller-process))
  (makunbound 'relisp-over-string)
  (makunbound 'relisp-terminal-string)
  (makunbound 'relisp-ruby-error-string)
  (tq-enqueue relisp-tq "\n" "\n" 'relisp-terminal-string   'relisp-start-controller-receiver)
  (tq-enqueue relisp-tq "\n" "\n" 'relisp-over-string       'relisp-start-controller-receiver)
  (tq-enqueue relisp-tq "\n" "\n" 'relisp-ruby-error-string 'relisp-start-controller-receiver)
  (while (and (equal (process-status relisp-controller-process) 'run)
	      (not (and (boundp 'relisp-over-string)
			(boundp 'relisp-terminal-string)
			(boundp 'relisp-ruby-error-string))))
    (accept-process-output))
  (relisp-update-endofmessage-regexp))

(defun relisp-start-controller-receiver (variable output)
  (set variable (chomp output)))

(defun relisp-update-endofmessage-regexp nil
  (setq relisp-endofmessage-regexp (concat "\\("      relisp-over-string 
					        "\\|" relisp-terminal-string 
					        "\\|" relisp-ruby-error-string
                                           "\\)" 
					   "[[:space:]]*")))

(defun relisp-new-transaction-number nil
  (if (boundp 'relisp-transaction-number) 
      (setq relisp-transaction-number (+ 1 relisp-transaction-number))
    (setq relisp-transaction-number 1)))

(defun ruby-eval (code)
  (unless (stringp code)
    (setq code (prin1-to-string code)))
  (unless (relisp-controller-alive-p)
    (relisp-start-controller))
  (setq code-to-ruby (concat code                   "\n" 
			     relisp-terminal-string "\n"))
  (if (relisp-controller-alive-p)
      (progn
	(let ((tq-num (relisp-new-transaction-number)))
	  (push tq-num relisp-transaction-list)
	  (tq-enqueue relisp-tq code-to-ruby relisp-endofmessage-regexp nil 'relisp-receiver)
	  (while (and (relisp-controller-alive-p) 
		      (member tq-num relisp-transaction-list))
	    (accept-process-output)))
	(if (boundp 'relisp-ruby-return)
	    (read relisp-ruby-return)
	  nil))
    nil))

(defun relisp-receiver (closure output)
  (makunbound 'relisp-ruby-return)
  (if (string-match relisp-over-string output)
      (progn
	(setq output (chomp (car (split-string output relisp-over-string))))
	(ruby-eval (eval (read output))))
    (setq return-val (chomp (car (split-string output relisp-terminal-string)))))
  (pop relisp-transaction-list)
  ;; if a deeper (recursive) level has set this, leave it alone
  (unless (boundp 'relisp-ruby-return)
    (setq relisp-ruby-return return-val)))

(defun relisp-reset-controller nil
  (tq-close relisp-tq))


(relisp-reset-controller)
(relisp-ruby-controller "/Users/don/src/relisp/test_relisp_controller")
;;(puts (+ 1 (ruby-eval "1 + 2 + 3")))
;;(puts (ruby-eval "'ruby sentence'"))
(puts (ruby-eval "relisp_sample_ruby_method"))


;; TODO:
;; convert lisp objects to ruby and back
;; catch emacs errors
;; check for ruby errors
;; send messages (both ways) to a buffer *relisp* or something
;; lock ruby variables
;; def variables in elisp
;; document variables and functions; interactive functions
;; catch warnings

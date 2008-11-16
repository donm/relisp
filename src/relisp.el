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

(defun relisp-controller-alive-p nil
  (and (boundp 'relisp-controller-process) 
       (equal (process-status relisp-controller-process) 'run)
       (boundp 'relisp-over-string)
       (boundp 'relisp-terminal-string)
       (boundp 'relisp-ruby-error-string)))

(defun relisp-ruby-controller-path (controller)
  (setq relisp-ruby-controller-path (expand-file-name controller)))

(defvar relisp-controller-name "relisp-controller")

(defun relisp-stop-controller nil
  (if (boundp 'relisp-controller-process)
      (delete-process relisp-controller-process)))

(defun relisp-start-controller nil
  (relisp-stop-controller)
  (setq relisp-transaction-list nil)
  (setq relisp-transaction-number 0)
  (setq relisp-controller-process 
	(start-process relisp-controller-name nil relisp-ruby-controller-path))
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
  (relisp-update-endofmessage-regexp)
  relisp-controller-process)

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
  (setq code-for-ruby (concat code                  "\n" 
			     relisp-terminal-string "\n"))
  (if (relisp-controller-alive-p)
      (progn
	(let ((tq-num (relisp-new-transaction-number)))
	  (push tq-num relisp-transaction-list)
	  (tq-enqueue relisp-tq code-for-ruby relisp-endofmessage-regexp nil 'relisp-receiver)
	  (while (and (relisp-controller-alive-p) 
		      (member tq-num relisp-transaction-list))
	    (accept-process-output)))
	(if (boundp 'relisp-ruby-return)
	    (if (string-match (concat "\n?" relisp-ruby-error-string "[[:space:]]*") relisp-ruby-return)
		(concat "RUBY ERROR: " (replace-match "" nil t relisp-ruby-return))
	      (if (relisp-controller-alive-p)
		  (read (trim-trailing-whitespace relisp-ruby-return))
		;; need to raise an error here
		(concat "relisp-controller is not alive. Code was:\n"
			code-for-ruby)))
	  "relisp-controller must have died"))
    nil))

(defun relisp-receiver (closure output)
  (makunbound 'relisp-ruby-return)
  (if (string-match relisp-over-string output)
      (progn
	(setq output (chomp (car (split-string output relisp-over-string))))
;;	(setq elisp-eval-result (prin1-to-string (eval (read output))))
	(setq elisp-eval-result (eval (read output)))
	(ruby-eval (prin1-to-string elisp-eval-result)))
    (setq return-val (chomp (car (split-string output relisp-terminal-string)))))
  (pop relisp-transaction-list)
  ;; if a deeper (recursive) level has set this, leave it alone
  (puts (boundp 'relisp-ruby-return))
  (unless (boundp 'relisp-ruby-return)
    (setq relisp-ruby-return return-val)))

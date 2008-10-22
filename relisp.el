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

(defconst relisp-over-string     "__>>>>>>>>>>>>>>>>>>>>__")
(defconst relisp-terminal-string "__xxxxxxxxxxxxxxxxxxxx__")
(defconst relisp-endofmessage-regexp (concat "\\(" relisp-over-string "\\|" relisp-terminal-string "\\)" 
					 "[[:space:]]*"))
(defvar relisp-transaction-list nil)

(defun relisp-controller-alive-p nil
  (and (boundp 'relisp-controller-process) (equal (process-status relisp-controller-process) 'run)))
  
(defun relisp-start-controller nil
  (if (boundp 'relisp-tq)
      (tq-close relisp-tq))
  (setq relisp-transaction-list nil)
  (setq relisp-transaction-number 0)
  (setq relisp-controller-process 
	(start-process "relisp-controller" nil "/Users/don/src/relisp/relisp_controller"))
  (setq relisp-tq 
	(tq-create relisp-controller-process)))

(defun relisp-new-transaction-number nil
  (if (boundp 'relisp-transaction-number) 
      (setq relisp-transaction-number (+ 1 relisp-transaction-number))
    (setq relisp-transaction-number 1)))

(defun ruby-eval (code)
  (unless (stringp code)
    (setq code (prin1-to-string code)))
  (unless (relisp-controller-alive-p)
    (relisp-start-controller))
  (setq code-to-ruby (concat relisp-terminal-string "\n" 
		     relisp-over-string     "\n" 
		     code                   "\n" 
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
	      relisp-ruby-return
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
  ;; if a deeper level has set this, leave it alone
  (unless (boundp 'relisp-ruby-return)
    (setq relisp-ruby-return return-val)))



;;(ruby-eval "puts 'ruby sentence'.reverse")
;;(ruby-eval "1 + 2")

(puts (ruby-eval "ruby_method"))


;; convert lisp to ruby and back

;; check for ruby errors
;; send messages (both ways) to a buffer *relisp* or something
;; lock ruby variables
;; have ruby give elisp the terminal and over strings

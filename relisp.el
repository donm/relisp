(defconst relisp-terminal-string "__terminal-string__")
(defconst relisp-terminal-regexp (concat relisp-terminal-string "[[:space:]]*"))

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

(defun relisp-receiver (closure output)
  (message (chomp (car (split-string output relisp-terminal-string))) t t))

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
			 code                   "\n" 
			 relisp-terminal-string "\n")))
    (if (relisp-controller-alive-p)
	(tq-enqueue relisp-tq message relisp-terminal-regexp nil 'relisp-receiver))))
  
(ruby-eval "2 + 3")
(ruby-eval "2 + 9")
(ruby-eval "2 + 1")

;; (defun relisp-filter-function (process output)
;;   (setq return (eval (read output)))
;;   (if (equal (process-status process) 'run)
;;       (process-send-string process (prin1-to-string return)))
;;   (message (prin1-to-string a))
;;   (accept-process-output relisp-controller-process))

;; (setq relisp-controller-process 
;;       (start-process "relisp-controller" nil "/home/don/src/relisp/relisp_controller" "some_script.rb"))
;; (set-process-filter    relisp-controller-process 'relisp-filter-function)
;; (accept-process-output relisp-controller-process)


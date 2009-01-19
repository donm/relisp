;; TODO:
;; catch emacs errors
;; lock ruby variables
;; def variables in elisp
;; document elisp variables and functions; interactive functions
;; catch warnings
;; check for more ruby errors
;; elisp_eval("read") hangs
;; check if ruby is alive before returning result
;; functions
;; tests

(defun puts (str)
  (message (prin1-to-string str)))

(progn
  (relisp-stop-slave)
  (relisp-ruby-slave-path "ruby_slave")
  (relisp-start-slave))

(puts (ruby-eval "relisp_sample_ruby_method1"))
(puts (ruby-eval "relisp_sample_ruby_method2"))
(puts (ruby-eval "relisp_sample_ruby_method3"))

(progn
  (relisp-stop-slave)
  (makunbound 'relisp-ruby-slave-path)
  (relisp-start-slave))

(puts (+ 1 (ruby-eval "1 + 2 + 3")))
(puts (ruby-eval "'ruby sentence'.reverse"))
(puts (ruby-eval "elisp_eval('(+ 1 2)')"))

(ruby-eval (concat (relisp-to-ruby 3) ".succ"))
(setq vect [1 2 3 4 5 1 2 3 4 1 23 4])
(puts (ruby-eval (relisp-to-ruby vect)))

(puts (ruby-eval "3"))
(puts (ruby-eval "elisp_eval('(ruby-eval \"3\")')"))

(setq str "a couple of words")
(puts (ruby-eval (concat (relisp-to-ruby str) ".split")))
(puts (ruby-eval "['a', 'couple', 'of', 'words']"))
(car (ruby-eval (concat (relisp-to-ruby str) ".split")))


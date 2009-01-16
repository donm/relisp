;; TODO:
;; when you to_elisp an array, to_elisp each element?
;; convert lisp objects to ruby and back

;; catch emacs errors
;; send messages (both ways) to a buffer *relisp* or something
;; lock ruby variables
;; def variables in elisp
;; document elisp variables and functions; interactive functions
;; catch warnings
;; check for more ruby errors
;; elisp_eval("read") hangs
;; check if ruby is alive before returning result
;; functions
;; tests

(progn
  (relisp-stop-slave)
  (relisp-ruby-slave-path "../examples/ruby_slave_example")
  (relisp-start-slave))

(puts (ruby-eval "relisp_sample_ruby_method1"))
(puts (ruby-eval "relisp_sample_ruby_method2"))

(puts (+ 1 (ruby-eval "1 + 2 + 3")))
(puts (ruby-eval "'ruby sentence'.reverse"))

(puts (ruby-eval "Relisp.concat('Don ', 'March')"))

(puts (ruby-eval "Relisp.+(1, 2)"))
(puts (ruby-eval "Relisp.elisp_eval('(+ 1 2)')"))



;;(ruby-eval (concat "Relisp.read " (prin1-to-string (prin1-to-string (ruby-eval "elisp_eval('a'.to_elisp.print)" )))))
;;(puts (ruby-eval "(Relisp.read elisp_eval('A'.to_elisp.print)).class" ))

;;(message (concat "Relisp.read " (ruby-eval "Relisp.elisp_eval('(create-file-buffer \"aaaa\" )')")))

;; (ruby-eval "a = [1, 2]")
;; (ruby-eval "a.elisp_type = Relisp::Array")
;; (puts (ruby-eval "a"))
;;(puts (elt (ruby-eval "a") 0))



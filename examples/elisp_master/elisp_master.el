;; You might want to look at the *Relisp* buffer to see what messages
;; are being passed.  Or you might not.  But it's there if you want it.

;; To evaluate each examples below, move point after the closing
;; parenthesis and press C-x C-e.

;; This is how to start the ruby slave without a file, but if you
;; don't `ruby-eval' and `ruby-exec' will start one for you. 
;; This stops the old slave first:
(relisp-start-slave) 

;; Note that return values are not strings but actual elisp data
;; types:
(ruby-eval "1 + 2 + 3")
(ruby-eval "'gnirts ybur'.reverse")

;; Variable persistence between calls:
(ruby-exec "a = 5")
(ruby-eval "a + 1")

;; Recursive calls:
(ruby-eval "elisp_eval('(+ 1 2)')")
(ruby-eval "elisp_eval('(ruby-eval \"1 + 2\")')")

(setq list '(3 5 2 6 4 1))
(ruby-eval "list = symbol_value(:list)")
(ruby-eval "list.class")
(ruby-eval "list.to_list.class")
(ruby-eval "list.to_list.sort")
(type-of (ruby-eval "list.to_list"))

;; Because of the method_missing stuff, getting the symbol value
;; directly isn't even necessary most of the time (it was for `list',
;; however, because it's a function and method_missing tries that
;; first.

(setq elisp-vector [1 2 3 4 5 6])
;; (ruby-eval "vect = symbol_value(:\"elisp-vector\")")
(ruby-eval "vect = self.elisp_vector")
;; or even
(ruby-eval "vect = elisp_vector")
(ruby-eval "vect.class")
(ruby-eval "vect.kind_of? Array")
(ruby-eval "vect.reverse")
(type-of (ruby-eval "vect.reverse"))
(ruby-eval "self.elisp_vector= vect.reverse")
(symbol-value 'elisp-vector)

(setq str "a couple of words")
(ruby-eval "self.str.split")

; If you need an elisp value in ruby but don't want it in an elisp
; variable, just elisp_eval it:
(ruby-exec "vect = elisp_eval '[1 2 3 4]'")
(ruby-eval "vect.reverse")

;; Running multiple slaves is possible by specifying `t' as a an
;; optional second argument to `relisp-start-slave'.

;; This stops the old slave first:
(setq p1 (relisp-start-slave))
;; This doesn't:
(setq p2 (relisp-start-slave nil t))
(ruby-exec "p=1" p1)
(ruby-exec "p=2" p2)
(ruby-eval "p" p1)
(ruby-eval "p" p2)
(relisp-stop-slave p1)
(relisp-slave-alive-p p1)
(relisp-stop-slave p2)

;; How to specify a file when running the slave.  This ruby file runs
;; some window stuff and then exits, i.e., it doesn't turn itself into
;; a slave.
(setq p (relisp-start-slave "ruby_slave2.rb"))
(relisp-slave-alive-p p)

;; This ruby file defines some methods, runs some frame stuff at
;; startup, and then starts itself as a slave.
(setq p (relisp-start-slave "ruby_slave.rb"))
(relisp-slave-alive-p p)
(ruby-exec "kill_the_new_buffer")
(ruby-eval "ruby_elisp_eval")
(ruby-eval "ruby_elisp_eval_ruby_eval")
(ruby-exec "frame_class")

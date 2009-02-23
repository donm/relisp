;; How to specify a file when running the slave.  The next few
;; `ruby-eval's call methods defined in the ruby slave file.

(progn
  (relisp-stop-slave)
  (setq relisp-ruby-slave-path "ruby_slave")
  (relisp-start-slave))

(ruby-eval "sample_ruby_method1")
(ruby-eval "sample_ruby_method2")

(member "ruby-created-buffer" (mapcar (lambda (a) (buffer-name a)) (buffer-list)))
(ruby-exec "sample_ruby_method3")
(member "ruby-created-buffer" (mapcar (lambda (a) (buffer-name a)) (buffer-list)))

(ruby-exec "sample_ruby_method4")

;; How to start the ruby slave without a file. The rest of the
;; commands will work fine with a slave started either way.

(progn
  (relisp-stop-slave)
  (makunbound 'relisp-ruby-slave-path)
  (relisp-start-slave))

;; Basic functionality--note that return values are not strings but
;; actual elisp data types:
(ruby-eval "1 + 2 + 3")
(ruby-eval "'ruby string'.reverse")

(setq vect [1 2 3 4 5 6])
(ruby-eval (concat (relisp-to-ruby vect) ".class"))
(ruby-eval (concat (relisp-to-ruby vect) ".kind_of?(Array)"))
(ruby-eval (concat (relisp-to-ruby vect) ".reverse"))
(type-of (ruby-eval (concat (relisp-to-ruby vect) ".reverse")))

(setq list '(3 5 2 6 4 1))
(ruby-eval (concat (relisp-to-ruby list) ".sort"))
(type-of (ruby-eval (concat (relisp-to-ruby list) ".sort")))

(setq str "a couple of words")
(ruby-eval (concat (relisp-to-ruby str) ".split"))

(type-of (ruby-eval "{:name => 'john'}"))

;; Recursive calls:
(ruby-eval "elisp_eval('(+ 1 2)')")
(ruby-eval "elisp_eval('(ruby-eval \"1 + 2\")')")

;; Variable persistence between calls:
(ruby-exec "a = 5")
(ruby-eval "a + 1")




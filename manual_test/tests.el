(progn
  (relisp-stop-slave)
  (setq relisp-ruby-slave-path "tests.rb")
  (relisp-start-slave))

;; bury current buffer
(ruby-eval "buffer_bury")

;; split this window vertically and set bottow window to
;; 'other-buffer'
(ruby-eval "buffer_window_equals")

;; split this window vertically then horizontally
(ruby-eval "window_split")








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

;; should move text up and return nil
(ruby-eval "window_scroll_up")

;; should move text down and return t
(ruby-eval "window_scroll_down")

;; create some windows and move them horizontally and vertically
(ruby-eval "window_shrink")

















































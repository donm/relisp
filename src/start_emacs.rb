#!/usr/bin/env ruby

system("emacs --batch -l relisp.el --eval '(relisp-ruby-slave-path \"../examples/ruby_example\")' --eval '(relisp-start-slave)' --eval '(ruby-eval \"1 + 2 + 3\")'")


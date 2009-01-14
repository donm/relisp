#!/usr/bin/env ruby

system("emacs --batch -l relisp.el --eval '(relisp-ruby-controller-path \"../examples/ruby_example\")' --eval '(relisp-start-controller)'")

#!/usr/bin/env ruby

$:.unshift File.join(File.dirname(__FILE__), "../../lib") 

require 'relisp'
require 'profile'

emacs = Relisp::ElispSlave.new

1000.times do
#  emacs.elisp_eval("(+ 1 2)").to_s
  emacs.elisp_eval("(ruby-eval \"elisp_eval '(+ 1 2)'\")").to_s
end


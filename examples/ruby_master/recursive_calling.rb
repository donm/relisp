#!/usr/bin/env ruby

$:.unshift File.join(File.dirname(__FILE__), "../../lib") 

require 'relisp'

emacs = Relisp::ElispSlave.new

puts "emacs.elisp_eval '(ruby-eval \"1 + 3\")'"
puts '=> ' + emacs.elisp_eval('(ruby-eval "1 + 3")').to_s


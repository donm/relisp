#!/usr/bin/env ruby

$:.unshift File.join(File.dirname(__FILE__), "../../lib") 

require 'relisp'

emacs = Relisp::ElispSlave.new

puts "elisp_eval '(+ 1 2)'"
puts '=> ' + emacs.elisp_eval("(+ 1 2)").to_s
puts
puts "elisp_exec '(+ 1 2)'"
puts '=> ' + emacs.elisp_exec("(+ 1 2)").to_s
puts "elisp_exec '(setq a (+ 1 2))'"
puts '=> ' + emacs.elisp_exec("(setq a (+ 1 2))").to_s
puts "elisp_eval 'a'"
puts '=> ' + emacs.elisp_eval("a").to_s




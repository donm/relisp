#!/usr/bin/env ruby

$:.unshift File.join(File.dirname(__FILE__), "../../lib") 

require 'relisp'

emacs = Relisp::ElispSlave.new

puts "emacs.elisp_eval '(concat \"two\" \"words\")"
puts '=> ' + (emacs.elisp_eval '(concat "two " "words")')
puts "emacs.concat \"two\", \"words\""
puts '=> ' + emacs.concat("two ", "words")

puts

puts 'emacs.set(:a, 3); emacs.symbol_value(:a)'
emacs.set(:a, 3).to_s
puts '=> ' + emacs.symbol_value(:a).to_s
puts 'emacs.a'
puts '=> ' + emacs.a.to_s
puts 'emacs.a= 5; emacs.a'
emacs.a= 5
puts '=> ' + emacs.a.to_s


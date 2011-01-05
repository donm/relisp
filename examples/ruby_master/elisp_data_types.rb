#!/usr/bin/env ruby

$:.unshift File.join(File.dirname(__FILE__), "../../lib") 

require 'relisp'

emacs = Relisp::ElispSlave.new

puts 'buffer = Relisp::Buffer.new("ruby-created-buffer"); buffer.to_s'
buffer = Relisp::Buffer.new("ruby-created-buffer")
puts '=> ' + buffer.to_s
puts "buffer.insert 'some text'; buffer.to_s"
buffer.insert 'some text'
puts '=> ' + buffer.to_s
puts 'buffer.name'
puts '=> ' + buffer.name.to_s
puts "buffer.name= 'fred'"
puts '=> ' + buffer.name= 'fred'.to_s
puts 'buffer.name'
puts '=> ' + buffer.name

puts

puts 'emacs.create_file_buffer("blah.txt").class'
puts '=> ' + emacs.create_file_buffer("blah.txt").class.to_s


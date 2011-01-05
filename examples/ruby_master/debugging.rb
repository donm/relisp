#!/usr/bin/env ruby

$:.unshift File.join(File.dirname(__FILE__), "../../lib") 

require 'relisp'

emacs = Relisp::ElispSlave.new


puts 'buffer = Relisp::Buffer.new("ruby-created-buffer"); buffer.to_s'
emacs.debugging do  
  @buffer = Relisp::Buffer.new("ruby-created-buffer")
end
puts '=> ' + @buffer.to_s

puts

puts "buffer.insert 'some text'; buffer.to_s"
emacs.debugging do  
  @buffer.insert 'some text'
end
puts '=> ' + @buffer.to_s

puts
puts "Hashes are currently the most complex thing to convert (press Enter to see it):"
gets

ruby_hash = ({ :time => "money",
       :money => "power" })
emacs.debugging do  
  emacs.emacs_hash= ruby_hash
end

$:.unshift File.join(File.dirname(__FILE__), "../../lib") 
require 'relisp'

slave = Relisp::RubySlave.new

def kill_the_new_buffer
  switch_to_buffer('*relisp_test_buffer*')
  kill_buffer
end

def ruby_elisp_eval
  elisp_eval("(+ 1 5)")
end

def ruby_elisp_eval_ruby_eval
  elisp_eval('(ruby-eval "(1 + 5)")')
end   

def frame_class
  frame = Relisp::Frame.new({:width => 80, :height => 20, :name => "ruby frame"})
  frame.height=30
  frame.width=60
  10.times { frame.width += 2 }
  10.times { frame.width -= 2 }
  10.times { frame.height += 2 }
  10.times { frame.height -= 2 }
  5.times { frame.height +=2; frame.width += 2 }
  5.times { frame.height -=2; frame.width -= 2 }
  frame.delete
end 

buffer = Relisp::Buffer.new('*relisp_test_buffer*')
slave.display_buffer buffer
buffer.insert "This buffer was created by the startup stuff in `ruby_slave.rb\'\n"
buffer.insert "It will go away when you run `kill_the_new_buffer'"

slave.start

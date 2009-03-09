$:.unshift File.join(File.dirname(__FILE__), "../lib") 

require 'relisp'

slave = Relisp::RubySlave.new

def buffer_bury
  b = current_buffer
  b.bury
end

def buffer_window_equals
  w = split_window_vertically
  b = other_buffer
  b.window = w
end

def window_split
  w = selected_window
  new_window = w.split_vertically
  new_window.split_horizontally
end

slave.start


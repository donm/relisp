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

def window_scroll_up
  selected_window.scroll_up(1)
  selected_window.visible?(1)
end

def window_scroll_down
  selected_window.scroll_down(1)
  selected_window.visible?(1)
end

def window_shrink
  w = selected_window
  w2 = w.split_vertically
  w3 = w2.split_horizontally
  10.times do
    w.shrink(1)
    redisplay
    sleep 0.01
  end
  10.times do
    w.enlarge(1)
    redisplay
    sleep 0.01
  end
  10.times do
    w2.shrink(0,1)
    redisplay
    sleep 0.01
  end
  10.times do
    w2.enlarge(0,1)
    redisplay
    sleep 0.01
  end
  
  w2.delete
  w3.delete
end

slave.start


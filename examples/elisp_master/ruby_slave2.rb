$:.unshift File.join(File.dirname(__FILE__), "../../lib") 
require 'relisp'

slave = Relisp::RubySlave.new

w = slave.selected_window
w2 = w.split_vertically
w3 = w2.split_horizontally
10.times do
  w.shrink(1)
  slave.redisplay
  sleep 0.01
end
10.times do
  w.enlarge(1)
  slave.redisplay
  sleep 0.01
end
10.times do
  w2.shrink(0,1)
  slave.redisplay
  sleep 0.01
end
10.times do
  w2.enlarge(0,1)
  slave.redisplay
  sleep 0.01
end

w2.delete
w3.delete



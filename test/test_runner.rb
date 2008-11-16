#!/usr/bin/env ruby

$:.unshift File.dirname(__FILE__) + "/../lib" 
require 'relisp'

def run_tests
#  load File.join(File.dirname(__FILE__), 'test_relisp.rb')
  return `ruby #{File.join(File.dirname(__FILE__), 'test_relisp.rb')}`
end

Relisp.start_controller

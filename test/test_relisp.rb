#!/usr/bin/env ruby

require 'test/unit'

$:.unshift File.dirname(__FILE__) + "/../bin"
$:.unshift File.dirname(__FILE__) + "/../lib" 
require 'relisp'

#Relisp.debug = true
Relisp.start_slave

class TestMyClass < Test::Unit::TestCase
  def setup
  end
  
  def teardown
  end
  
  def test_integer
    assert_equal 3, Relisp.elisp_eval( "(+ 1 2)" )
  end

  def test_string
    assert (Relisp.elisp_eval( '(concat "String " "test")')).kind_of? String
    assert_equal (Relisp.elisp_eval( '(concat "String " "test")')).class, Relisp::String
    assert_equal "String test", Relisp.elisp_eval( '(concat "String " "test")')
  end

end



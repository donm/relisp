#!/usr/bin/env ruby

require 'test/unit'

$:.unshift File.dirname(__FILE__) + "/../lib" 
require 'relisp'

class TestSlaves < Test::Unit::TestCase
  def setup
    @emacs = Relisp::ElispSlave.new
  end
  
  def teardown
  end
  
  def test_new_elisp_variable
    vars = Array.new
    size = 1000
    size.times do 
      vars << @emacs.new_elisp_variable
    end
    assert_equal size, vars.uniq.size
  end

  def test_method_missing
    assert_equal 6, @emacs.+(1, 2, 3)
  end

  def test_do
    assert_equal 3, @emacs.do("(+ 1 2)")
  end

  def test_provide
    array = [1, 2, 4]
    @emacs.provide(:array, binding)
    assert_equal array, @emacs.array
    array.pop
    assert_equal array, @emacs.array
  end

#   def test_emacs_as_master
#     puts @emacs.do '(relisp-start-slave)'
#     puts @emacs.do('(ruby-eval "1 + 1")')
#     puts @emacs.write_to_emacs('(ruby-eval "Relisp::ElispSlave.new")')
#   end


end


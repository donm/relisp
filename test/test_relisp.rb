#!/usr/bin/env ruby

require 'test/unit'

$:.unshift File.dirname(__FILE__) + "/../bin"
$:.unshift File.dirname(__FILE__) + "/../lib" 
require 'relisp'

#Relisp.debug = true

class TestRelisp < Test::Unit::TestCase
  def setup
    Relisp.start_slave
  end
  
  def teardown
  end
  
  def test_new_elisp_variable
    vars = Array.new
    size = 1000
    size.times do 
      vars << Relisp.new_elisp_variable
    end
    assert_equal size, vars.uniq.size
  end

  def test_method_missing
    assert_equal 6, Relisp.+('1', '2', '3')
  end

#   def test_emacs_as_slave
#   end

#   def test_ruby_as_slave
#   end

  def test_integer
    assert_equal 3, Relisp.elisp_eval( "(+ 1 2)" )
  end

  def test_float
    assert_equal 2.5, Relisp.elisp_eval( "(/ 5.0 2)" )
  end

  def test_symbol
    result = Relisp.elisp_eval( "'lambda" )
    assert_equal Symbol, result.class
  end

  def test_cons
    result = Relisp.elisp_eval( "'(1 \"string\" 3 [4 5] )" )
    assert result.kind_of?(Array)
    assert_equal Relisp::Cons, result.class
    assert_equal "string", result[1]
    assert_equal [4, 5], result[3]
  end

  def test_vector
    result = Relisp.elisp_eval( "[1 \"string\" 3 [\"sub\" \"array\" 5] ]" )
    assert result.kind_of?(Array) 
    assert_equal Relisp::Vector, result.class
    assert_equal "string", result[1]
    assert_equal ["sub", "array", 5], result[3]
  end

  def test_string
    assert (Relisp.elisp_eval( '(concat "String " "test")')).kind_of?(String)
    assert_equal (Relisp.elisp_eval( '(concat "String " "test")')).class, Relisp::String
    assert_equal "String test", Relisp.elisp_eval( '(concat "String " "test")')
  end

  def test_hash_table
    Relisp.elisp_eval( '(setq ht (make-hash-table))' )
    Relisp.elisp_eval( '(puthash \'first "kathryn" ht)' )
    Relisp.elisp_eval( '(puthash \'last "march" ht)' )
    Relisp.elisp_eval( '(setq subht (make-hash-table))' )
    Relisp.elisp_eval( '(puthash \'first "kathryn" subht)' )
    Relisp.elisp_eval( '(puthash \'last "march" subht)' )
    Relisp.elisp_eval( '(puthash \'sub subht ht)' )
    hash = Relisp.elisp_eval( 'ht' )
    ruby_hash = Hash.new
    ruby_hash[:first] = 'kathryn'
    ruby_hash[:last] = 'march'
    copy = ruby_hash.dup
    ruby_hash[:sub] = copy
    assert_equal ruby_hash, hash
  end

  def test_buffer
    test_buffer_name = "*relisp-test-buffer*"
    buffer = Relisp.elisp_eval( "(create-file-buffer \"#{test_buffer_name}\") " )
    assert_equal Relisp::Buffer, buffer.class
    buffer_names = Relisp.elisp_eval( '(buffer-list)' ).map { |buffer| buffer.name } 
    assert buffer_names.include?(test_buffer_name)
  end
  
  
end


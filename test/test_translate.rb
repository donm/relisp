#!/usr/bin/env ruby

require 'test/unit'

$:.unshift File.dirname(__FILE__) + "/../bin"
$:.unshift File.dirname(__FILE__) + "/../lib" 
require 'relisp'

class TestTranslate < Test::Unit::TestCase
  def setup
    @emacs = Relisp::ElispSlave.new
  end
  
  def teardown
  end

  def test_integer
    assert_equal 3, @emacs.elisp_eval( "(+ 1 2)" )
  end

  def test_float
    assert_equal 2.5, @emacs.elisp_eval( "(/ 5.0 2)" )
  end

  def test_symbol
    result = @emacs.elisp_eval( "'lambda" )
    assert_equal Symbol, result.class
  end

  def test_cons
    result = @emacs.elisp_eval( "'(1 \"string\" 3 [4 5] )" )
    assert result.kind_of?(Array)
    assert_equal Relisp::Cons, result.class
    assert_equal "string", result[1]
    assert_equal [4, 5], result[3]
  end

  def test_vector
    result = @emacs.elisp_eval( "[1 \"string\" 3 [\"sub\" \"array\" 5] ]" )
    assert result.kind_of?(Array) 
    assert_equal Relisp::Vector, result.class
    assert_equal "string", result[1]
    assert_equal ["sub", "array", 5], result[3]
  end

  def test_string
    assert (@emacs.elisp_eval( '(concat "String " "test")')).kind_of?(String)
    assert_equal (@emacs.elisp_eval( '(concat "String " "test")')).class, Relisp::String
    assert_equal "String test", @emacs.elisp_eval( '(concat "String " "test")')
  end

  def test_hash_table
    @emacs.elisp_eval( '(setq ht (make-hash-table))' )
    @emacs.elisp_eval( '(puthash \'first "kathryn" ht)' )
    @emacs.elisp_eval( '(puthash \'last "march" ht)' )
    @emacs.elisp_eval( '(setq subht (make-hash-table))' )
    @emacs.elisp_eval( '(puthash \'first "kathryn" subht)' )
    @emacs.elisp_eval( '(puthash \'last "march" subht)' )
    @emacs.elisp_eval( '(puthash \'sub subht ht)' )
    hash = @emacs.elisp_eval( 'ht' )
    ruby_hash = Hash.new
    ruby_hash[:first] = 'kathryn'
    ruby_hash[:last] = 'march'
    copy = ruby_hash.dup
    ruby_hash[:sub] = copy
    assert_equal ruby_hash, hash
  end

  def test_buffer
    test_buffer_name = "*relisp-test-buffer*"
    buffer = @emacs.elisp_eval( "(create-file-buffer \"#{test_buffer_name}\") " )
    assert_equal Relisp::Buffer, buffer.class
    buffer_names = @emacs.elisp_eval( '(buffer-list)' ).map { |buffer| buffer.name } 
    assert buffer_names.include?(test_buffer_name)
    assert_equal :buffer, @emacs.eval("(type-of #{buffer.to_elisp})")
  end
  
end  

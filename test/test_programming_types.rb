#!/usr/bin/env ruby

require 'test/unit'

$:.unshift File.dirname(__FILE__) + "/../lib" 
require 'relisp'

class TestProgrammingTypes < Test::Unit::TestCase
  def setup
    @emacs = Relisp::ElispSlave.new
  end
  
  def teardown
  end

  def test_integer
    emacs_num = @emacs.elisp_eval( "(+ 1 2)" )
    assert_equal 3, emacs_num
    assert_equal 4, @emacs.elisp_eval( "(+ 1 #{emacs_num.to_elisp})" )
  end

  def test_float
    emacs_num = @emacs.elisp_eval( "(/ 5.0 2)" )
    assert_equal 2.5, emacs_num
    assert_equal 7.5, @emacs.elisp_eval( "(* 3 #{emacs_num.to_elisp})" )
  end

  def test_symbol
    plus = @emacs.elisp_eval( "'+" )
    assert_equal :+, plus
    assert_equal 3, @emacs.elisp_eval( "(funcall #{plus.to_elisp} 1 2)" )
    assert_equal 3, @emacs.elisp_eval( "(#{plus} 1 2)" )

    nil_val = @emacs.eval( 'nil' )
    assert_nil nil_val
    assert @emacs.eval( "(null #{nil_val.to_elisp})" )

    t_val = @emacs.eval( '(equal 1 1)' )
    assert_equal true, t_val
    assert @emacs.eval( "(equal #{t_val.to_elisp} (equal 1 1))" )
  end

  def test_cons
    result = @emacs.elisp_eval( "'(1 \"string\" 3 [4 5] )" )
    assert result.kind_of?(Array)
    assert_equal Relisp::Cons, result.class
    assert_equal "string", result[1]
    assert_equal [4, 5], result[3]
    list = [1,2,'a',[4,5]]
    assert @emacs.elisp_eval( "(equal (list 1 2 \"a\" (list 4 5)) #{list.to_elisp})" )
    assert_equal 1, @emacs.elisp_eval( "(car #{list.to_elisp})" )
  end

  def test_string
    assert_equal "String test", @emacs.elisp_eval( '(concat "String " "test")')
    str = "a string"
    assert @emacs.elisp_eval( "(equal \"a string\" #{str.to_elisp}  )" )
  end

  def test_vector
    result = @emacs.elisp_eval( "[1 \"string\" 3 [\"sub\" \"array\" 5] ]" )
    assert result.kind_of?(Array) 
    assert_equal Relisp::Vector, result.class
    assert_equal "string", result[1]
    assert_equal ["sub", "array", 5], result[3]
    vect = [1,2,'a',[4,5]]
    vect.elisp_type=Relisp::Vector
    assert @emacs.elisp_eval( "(equal [1 2 \"a\" (list 4 5)] #{vect.to_elisp})" )
    Array.default_elisp_type=Relisp::Vector
    assert @emacs.elisp_eval( "(equal [1 2 \"a\" [4 5]] #{vect.to_elisp})" )
    
    vect = (1..100).to_a
    assert_equal vect, @emacs.elisp_eval( vect.to_elisp )
  end

  def test_hash_table
    @emacs.elisp_eval( '(setq ht (make-hash-table))' )
    @emacs.elisp_eval( '(puthash "first" "john" ht)' )
    @emacs.elisp_eval( '(puthash \'last "doe" ht)' )
    @emacs.elisp_eval( '(setq subht (make-hash-table))' )
    @emacs.elisp_eval( '(puthash "first" "john" subht)' )
    @emacs.elisp_eval( '(puthash \'last "doe" subht)' )
    @emacs.elisp_eval( '(puthash \'sub subht ht)' )
    hash = @emacs.elisp_eval( 'ht' )
    ruby_hash = Hash.new
    ruby_hash["first"] = 'john'
    ruby_hash[:last] = 'doe'
    copy = ruby_hash.dup
    ruby_hash[:sub] = copy
    assert_equal ruby_hash, hash
    assert_equal hash, @emacs.elisp_eval( hash.to_elisp )

    # this returns false, even though the above is true--I don't
    # completely understand how emacs is comparing them.
    # @emacs.elisp_eval( "(equal ht #{ruby_hash.to_elisp})" )
  end

end  



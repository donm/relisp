#!/usr/bin/env ruby

require 'test/unit'

$:.unshift File.dirname(__FILE__) + "/../lib" 
require 'relisp'

class TestEditingTypes < Test::Unit::TestCase
  def setup
    @emacs = Relisp::ElispSlave.new
  end
  
  def teardown
  end

  def test_buffer
    test_buffer_name = "*relisp-test-buffer*"
    buffer = @emacs.elisp_eval( "(create-file-buffer \"#{test_buffer_name}\") " )
    assert_equal Relisp::Buffer, buffer.class
    buffer_names = @emacs.elisp_eval( '(buffer-list)' ).map { |buffer| buffer.name } 
    assert buffer_names.include?(test_buffer_name)
    assert_equal :buffer, @emacs.elisp_eval("(type-of #{buffer.to_elisp})")
  end
  
end

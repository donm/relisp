# Code Generated by ZenTest v. 3.11.1

require 'test/unit' unless defined? $ZENTEST and $ZENTEST

$:.unshift File.dirname(__FILE__) + "/../lib" 
require 'relisp'

module TestRelisp
  class TestSlave < Test::Unit::TestCase
    # setup done in test_slaves.rb
    #     def setup
    #       @emacs = Relisp::ElispSlave.new
    #     end

    def test_save_excursion
      # TODO: test mark, mark-active
      start_point = @emacs.point
      start_buffer = @emacs.current_buffer
      @emacs.save_excursion do 
        @emacs.insert("move along")
        assert_not_equal start_point, @emacs.point
        buffer = Relisp::Buffer.new("--relisp--test--", @emacs)
        @emacs.switch_to_buffer(buffer)
        assert_not_equal start_buffer.name, @emacs.current_buffer.name
      end
      assert_equal start_point, @emacs.point
      assert_equal start_buffer.name, @emacs.current_buffer.name
    end

    def test_method_missing
      assert_equal 6, @emacs.+(1, 2, 3)
      assert_raise NameError do
        @emacs.utter_nonsense
      end
    end
  end
end



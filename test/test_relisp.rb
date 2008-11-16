require 'test/unit'

$:.unshift File.dirname(__FILE__) + "/../bin"
$:.unshift File.dirname(__FILE__) + "/../lib" 
require 'relisp'

class TestMyClass < Test::Unit::TestCase
  def setup
  end
  
  def teardown
  end
  
  def test_case
    3
  end
end



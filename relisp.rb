require 'relisp/translate'

# Emacs is great.  So is Ruby.  This is a collection of tools to
#   * call Ruby from Emacs (from elisp)
#   * call elisp from Ruby (once Ruby has invoked from elisp)
#   * manipulate Emacs without using elisp (Ruby wrappers around some elisp operations)
#   * contribute to flame wars and blogs entries titled "Ruby vs. lisp vs. scheme vs. haskell vs. ..."
#
module Relisp
  TERMINAL_STRING   = "__xxxxxOVERANDOUTxxxxx__"
  OVER_STRING       = "__>>>>>ROGEROVER>>>>>>__"
  RUBY_ERROR_STRING = "__RUBYERROR_HOWCANTHATBE__"

  def self.elisp_eval(code)
    puts code
    puts OVER_STRING

    elisp_return = String.new
    until gets.strip == TERMINAL_STRING
      elisp_return << $_
    end
    return elisp_return
  end

  def self.start_controller
    [TERMINAL_STRING, OVER_STRING, RUBY_ERROR_STRING].each do |constant|
      gets # wait for communication from emacs
      puts constant
    end

    begin
      local_binding = binding
    
      loop do
        code = String.new
        until gets.strip == TERMINAL_STRING
          code << $_
        end

        puts (eval code, local_binding).to_elisp
        puts TERMINAL_STRING
      end
    rescue => dag_yo
      puts dag_yo
      puts RUBY_ERROR_STRING
    end
  end 
end

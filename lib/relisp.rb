# Emacs is great.  So is Ruby.  This is a collection of tools to
#   * call Ruby from Emacs (from elisp)
#   * call elisp from Ruby (once Ruby has been invoked from elisp)
#   * manipulate Emacs without using elisp (Ruby wrappers around some elisp operations)
#   * contribute to flame wars and blogs entries titled "Ruby vs. lisp vs. scheme vs. haskell vs. ..."
#

require 'relisp/translate'

module Relisp
  TERMINAL_STRING   = "__xxxxxOVERANDOUTxxxxx__"
  OVER_STRING       = "__>>>>>ROGEROVER>>>>>>__"
  RUBY_ERROR_STRING = "__NO_THATSNOTTRUE_THATSIMPOSSIBLE__"

  def self.elisp_execute(code)
    puts code
    puts OVER_STRING

#    elisp_return = String.new
    elisp_return = ''.class.new
    until gets.strip == TERMINAL_STRING
      elisp_return << $_
    end

    elisp_return.gsub!(/\n\z/, '')
    return elisp_return
  end

  def self.start_controller
    [TERMINAL_STRING, OVER_STRING, RUBY_ERROR_STRING].each do |constant|
      gets
      puts constant
    end

    begin
      local_binding = binding

      loop do
        code = ""
        until gets.strip == TERMINAL_STRING
          code << $_
        end
        code.gsub!(/\n\z/, '')

        puts (eval code, local_binding).to_elisp.print
        puts TERMINAL_STRING
      end
    rescue => dag_yo
      puts dag_yo
      puts RUBY_ERROR_STRING
    end
  end 
  
end

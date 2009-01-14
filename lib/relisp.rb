#!/usr/bin/env ruby

# Emacs is great.  So is Ruby.  This is a collection of tools to
#   * call Ruby from Emacs (from elisp)
#   * call elisp from Ruby (once Ruby has been invoked from elisp)
#   * manipulate Emacs without using elisp (Ruby wrappers around some elisp operations)
#   * contribute to flame wars and blog entries titled "Ruby vs. lisp vs. scheme vs. haskell vs. ..."
#

require 'relisp/translate'

module Relisp
  ANSWER_CODE         = "__...ANSWER...__"
  QUESTION_CODE       = "__???QUESTION???__"
  ERROR_CODE          = "__!!!NO_THATSNOTTRUE_THATSIMPOSSIBLE!!!__"
  ENDOFMESSAGE_REGEXP = Regexp.new(ANSWER_CODE + "|" + QUESTION_CODE)

  @@local_binding = nil

  def self.elisp_eval(code)
    puts code
    puts QUESTION_CODE

    elisp_return = ''
    until gets.strip == ANSWER_CODE
      if $_ == QUESTION_CODE
        puts (eval elisp_return, @@local_binding).to_elisp.print
        puts ANSWER_CODE
        elisp_return = ''
      else
        elisp_return << $_
      end
    end

    elisp_return.gsub!(/\n\z/, '')
    return elisp_return
  end

  def self.pass_constants
    [ANSWER_CODE, QUESTION_CODE, ENDOFMESSAGE_REGEXP].each do |constant|
      gets
      puts constant
    end
  end

  def self.write_to_emacs(code)
  end

  def self.read_from_emacs
  end

  def self.become_slave
    pass_constants

    begin
      @@local_binding = binding

      loop do
        code = ''
        until gets.strip == QUESTION_CODE
          code << $_
        end
        code.gsub!(/\n\z/, '')

        puts (eval code, @@local_binding).to_elisp.print
        puts ANSWER_CODE
      end
    rescue => dag_yo
      puts dag_yo
      puts ENDOFMESSAGE_REGEXP
    end
  end 
  
end

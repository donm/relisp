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
  @@ruby_master = true

  def self.elisp_eval(code)
    write_to_emacs code
    write_to_emacs QUESTION_CODE

    elisp_return = ''
    until gets.strip == ANSWER_CODE
      if $_ == QUESTION_CODE
        write_to_emacs (eval elisp_return, @@local_binding).to_elisp.print
        write_to_emacs ANSWER_CODE
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
      write_to_emacs constant
    end
  end

  def self.write_to_emacs(code)
    if @@ruby_master
      puts code
    else
      @@emacs_pipe.puts code
    end
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

        write_to_emacs (eval code, @@local_binding).to_elisp.print
        write_to_emacs ANSWER_CODE
      end
    rescue => dag_yo
      write_to_emacs dag_yo
      write_to_emacs ENDOFMESSAGE_REGEXP
    end
  end 
  
  def self.start_slave
    @@ruby_master = false
    emacs_command = "emacs --batch -l ../src/relisp.el --eval '(relisp-become-slave)'"
    @@emacs_pipe = IO.popen(emacs_command, "w+")
    sleep 3
    pass_constants
  end

end

#!/usr/bin/env ruby

# Emacs is great.  So is Ruby.  This is a collection of tools to
#   * call Ruby from Emacs (from elisp)
#   * call elisp from Ruby (once Ruby has been invoked from elisp)
#   * manipulate Emacs without using elisp (Ruby wrappers around some elisp operations)
#   * contribute to flame wars and blog entries titled "Ruby vs. lisp vs. scheme vs. haskell vs. ..."
#

require 'relisp/translate'

module Relisp
  ANSWER_CODE         = "___FORTYTWO___"
  QUESTION_CODE       = "___TOBEORNOTTOBE___"
  ERROR_CODE          = "___NO_THATSNOTTRUE_THATSIMPOSSIBLE___"
  ENDOFMESSAGE_REGEXP = Regexp.new(ANSWER_CODE + "|" + QUESTION_CODE + "|" + ERROR_CODE)
  VARIABLE_PREFIX = '--reserved--relisp--variable--'

  @@local_binding = nil
  @@ruby_master = true
  @@debug = false
  @@current_elisp_variable_num = '0'

  def self.debug=(val)
    @@debug = val
  end

  def self.debug
    if block_given?
      begin
        @@debug = true
        puts
        puts "-----------------"
        yield
      ensure
        @@debug = false
        puts "-----------------"
      end
    else
      @@debug = ! @@debug
    end
  end

  def self.new_elisp_variable
    VARIABLE_PREFIX + @@current_elisp_variable_num.succ!
  end

  def self.elisp_eval(code)
    elisp_result = elisp_execute(code)
    elisp_object_variable = new_elisp_variable
    elisp_execute("(setq #{elisp_object_variable} relisp-eval-result)")
    read(elisp_result, elisp_object_variable)
  end

  def self.method_missing(function, *args)
    elisp_eval('(' + function.to_s + ' ' + args.map{|a| a.to_elisp.print}.join(' ') + ')')
  end

  def self.elisp_execute(code)
    write_to_emacs code
    write_to_emacs QUESTION_CODE

    input = ''
    input_line = read_from_emacs
    until input_line.strip =~ ENDOFMESSAGE_REGEXP
      if input == QUESTION_CODE
        write_to_emacs (eval input, @@local_binding).to_elisp.print
        write_to_emacs ANSWER_CODE
        input = ''
      else
        input << input_line
      end
      input_line = read_from_emacs
    end

    input.gsub!(/\n\z/, '')
    return input
  end
  
  def self.pass_constants
    [ANSWER_CODE, QUESTION_CODE, ERROR_CODE].each do |constant|
      read_from_emacs
      write_to_emacs constant
    end
  end

  def self.write_to_emacs(code)
    if @@ruby_master
      if @@debug
        puts "relisp> " + code unless code =~ ENDOFMESSAGE_REGEXP
      end
      @@emacs_pipe.puts code
    else
      puts code
    end
  end

  def self.read_from_emacs
    if @@ruby_master
      input = @@emacs_pipe.gets
      if @@debug
        puts "=> " + input unless input =~ ENDOFMESSAGE_REGEXP
      end
      return input
    else
      gets
    end
  end

  def self.become_slave
    @@ruby_master = false

    pass_constants

    begin
      @@local_binding = binding

      loop do
        code = ''
        input = read_from_emacs
        until input.strip == QUESTION_CODE
          code << input
          input = read_from_emacs
        end
        code.gsub!(/\n\z/, '')

        write_to_emacs (eval code, @@local_binding).to_elisp.print
        write_to_emacs ANSWER_CODE
      end
    rescue => dag_yo
      write_to_emacs dag_yo
      write_to_emacs ERROR_CODE
    end
  end 
  
  def self.start_slave
    emacs_command =    "emacs --batch "
    emacs_command << "-l ../src/relisp.el "
    emacs_command << "--eval '(relisp-become-slave)' "
    emacs_command << "--no-site-file "
    emacs_command << "2>&1"
    @@emacs_pipe = IO.popen(emacs_command, "w+")
    until read_from_emacs.strip == "SEND CONSTANTS"
    end
    pass_constants
  end

end

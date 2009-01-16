module Relisp

  class Slave
    ANSWER_CODE         = "___FORTYTWO___"
    QUESTION_CODE       = "___TOBEORNOTTOBE___"
    ERROR_CODE          = "___NO_THATSNOTTRUE_THATSIMPOSSIBLE___"
    ENDOFMESSAGE_REGEXP = Regexp.new(ANSWER_CODE + "|" + QUESTION_CODE + "|" + ERROR_CODE)
    VARIABLE_PREFIX = '--reserved--relisp--variable--'

    def initialize
      @local_binding = nil
      @current_elisp_variable_num = '0'
    end

    def new_elisp_variable
      VARIABLE_PREFIX + @current_elisp_variable_num.succ!
    end

    def elisp_eval(code)
      elisp_result = elisp_execute(code)
      elisp_object_variable = new_elisp_variable
      elisp_execute("(setq #{elisp_object_variable} relisp-eval-result)")
      read(elisp_result, elisp_object_variable)
    end

    def method_missing(function, *args)
      elisp_eval('(' + function.to_s + ' ' + args.map{|a| a.to_elisp.print}.join(' ') + ')')
    end

    def elisp_execute(code)
      write_to_emacs code
      write_to_emacs QUESTION_CODE

      input = ''
      input_line = read_from_emacs
      until input_line.strip =~ ENDOFMESSAGE_REGEXP
        if input == QUESTION_CODE
          write_to_emacs (eval input, @local_binding).to_elisp.print
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
    
    def pass_constants
      [ANSWER_CODE, QUESTION_CODE, ERROR_CODE].each do |constant|
        read_from_emacs
        write_to_emacs constant
      end
    end
  end

  class RubySlave < Slave
    def initialize
      super
      pass_constants

      if block_given?
        yield
        start
      end

    end
    
    def start
      begin
        @local_binding = binding

        loop do
          code = ''
          input = read_from_emacs
          until input.strip == QUESTION_CODE
            code << input
            input = read_from_emacs
          end
          code.gsub!(/\n\z/, '')

          write_to_emacs (eval code, @local_binding).to_elisp.print
          write_to_emacs ANSWER_CODE
        end
      rescue => dag_yo
        write_to_emacs dag_yo
        write_to_emacs ERROR_CODE
      end
    end
    
    def write_to_emacs(code)
      puts code
    end

    def read_from_emacs
      gets
    end
  end

  class ElispSlave < Slave
    alias eval elisp_eval

    def initialize
      super
      emacs_command =    "emacs --batch "
      emacs_command << "-l ../src/relisp.el "
      emacs_command << "--eval '(relisp-become-slave)' "
      emacs_command << "--no-site-file "
      emacs_command << "2>&1"
      @emacs_pipe = IO.popen(emacs_command, "w+")
      until read_from_emacs.strip == "SEND CONSTANTS"
      end
      pass_constants
    end

    def write_to_emacs(code)
      if @debug
        puts "relisp> " + code unless code =~ ENDOFMESSAGE_REGEXP
      end
      @emacs_pipe.puts code
    end
    
    def read_from_emacs
      input = @emacs_pipe.gets
      if @debug
        puts "=> " + input unless input =~ ENDOFMESSAGE_REGEXP
      end
      return input
    end

    def debug=(val)
      @debug = val
    end

    def debug
      if block_given?
        debug_start_val = @debug
        begin
          @debug = true
          puts
          puts "-----------------"
          yield
        ensure
          @debug = debug_start_val
          puts "-----------------"
        end
      else
        @debug = ! @debug
      end
    end
  end

end

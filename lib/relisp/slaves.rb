module Relisp

  # This is the base class for RubySlave and ElispSlave--the Slave
  # class isn't meant to be used itself.
  class Slave
    ANSWER_CODE         = "___FORTYTWO___"
    QUESTION_CODE       = "___TOBEORNOTTOBE___"
    ERROR_CODE          = "___NO_THATSNOTTRUE_THATSIMPOSSIBLE___"
    ENDOFMESSAGE_REGEXP = Regexp.new(ANSWER_CODE + "|" + QUESTION_CODE + "|" + ERROR_CODE)
    PREVIOUS_ELISP_RESULT = '--relisp--previous--result'
    VARIABLE_PREFIX = '--relisp--variable--'

    def initialize
      @local_binding = nil
      @current_elisp_variable_num = '0'
    end

    attr_reader :local_binding

    def new_elisp_variable
      (VARIABLE_PREFIX + @current_elisp_variable_num.succ!).to_sym
    end

    def get_permament_variable(old_variable)
      permament_variable = new_elisp_variable
      elisp_execute( "(setq #{permament_variable} #{old_variable})" )
      return permament_variable
    end

    # Pass an elisp evaluation result to the appropriate Relisp class
    # for translation.  The first line of _result_string_ is the
    # 'type-of' the elisp object.  The line(s) after that are the text
    # version of the object.  In case the string representation isn't
    # enough information to translate the object, the result needs to
    # be kept (in emacs) in the variable +PREVIOUS_ELISP_RESULT+.
    def read_elisp(result_string)
      result_string = result_string.split("\n")
      elisp_type    = result_string.reverse!.pop
      object_string = result_string.reverse!.join("\n")

      object_info = {
        :string   => object_string,
        :variable => PREVIOUS_ELISP_RESULT,
        :slave    => self, 
      }

      # Just one more reason to love Ruby.  Call the Relisp class
      # formed by rubyizing the 'type-of' the result (i.e., hash-table
      # becomes HashTable).
      (eval elisp_type.split("-").map { |a| a.capitalize }.join).from_elisp(object_info)
    end

    def elisp_eval(code)
      result_string = elisp_execute(code)
      read_elisp(result_string)
    end

    def elisp_execute(code)
      write_to_emacs code
      write_to_emacs QUESTION_CODE

      output = ''
      output_line = read_from_emacs
      until output_line.strip == ANSWER_CODE
        if output_line.strip == QUESTION_CODE
          write_to_emacs((eval(output, @local_binding)).to_elisp)
          write_to_emacs ANSWER_CODE
          output = ''
        elsif output_line.strip == ERROR_CODE
          # do something here
          fail
        else
          output << output_line
        end
      output_line = read_from_emacs
      end

      output.gsub!(/\n\z/, '')
      return output
    end
    
    def method_missing(function, *args)
      elisp_eval('(' + function.to_s + ' ' + args.map{|a| a.to_elisp}.join(' ') + ')')
    end

    # The ruby and elisp code are tied to one another so closely that
    # I don't know if it matters, but it seemed like a good idea to
    # not hard code the constants in both places.
    def pass_constants
      [ANSWER_CODE, QUESTION_CODE, ERROR_CODE, PREVIOUS_ELISP_RESULT].each do |constant|
        read_from_emacs
        write_to_emacs constant
      end
    end

    def make_available(symbol, binding)
      eval "@__#{symbol.to_s}__binding = binding"

      instance_eval <<-endstr
           def #{symbol.to_s}
             eval("#{symbol.to_s}", @__#{symbol.to_s}__binding)
           end
      endstr
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

          write_to_emacs (eval code, @local_binding).to_elisp
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
    alias do elisp_eval

    def initialize
      super
      elisp_path = File.expand_path(File.join(File.dirname(__FILE__), '../../src/relisp.el'))

      @local_binding = binding

      emacs_command =  "emacs --batch"
      emacs_command << " --no-site-file"
      emacs_command << " -l #{elisp_path}"
      emacs_command << " --eval '(relisp-become-slave)'"
      emacs_command << " 2>&1"
      @emacs_pipe = IO.popen(emacs_command, "w+")
      until read_from_emacs.strip == "SEND CONSTANTS"
      end
      pass_constants
    end

    def write_to_emacs(code)
      if @debug
        puts "ruby> " + code unless code =~ ENDOFMESSAGE_REGEXP
      end
      @emacs_pipe.puts code
    end
    
    def read_from_emacs
      output = @emacs_pipe.gets
      if @debug
        puts "lisp> " + output unless output =~ ENDOFMESSAGE_REGEXP
      end
      return output
    end

    def debug=(val)
      @debug = val
    end

    def debug
      if block_given?
        debug_original_val = @debug
        begin
          @debug = true
          puts
          puts "-----------------"
          yield
        ensure
          @debug = debug_original_val
          puts "-----------------"
        end
      else
        @debug = ! @debug
      end
    end
  end

end

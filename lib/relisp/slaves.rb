#--
# Copyright (C) 2009, 2010 Don March
#
# This file is part of Relisp.
#
# Relisp is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#  
# Relisp is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see
# <http://www.gnu.org/licenses/>.
#++
#

# TODO: maybe catch Errno::EPIPE to see if slave died

module Relisp

  @default_slave = nil
  
  def self.default_slave
    @default_slave
  end

  def self.default_slave=(slave)
    unless slave.kind_of?(Slave)
      raise ArgumentError, "#{slave} is not a Slave"
    end
    @default_slave = slave
  end

  # This is the base class for RubySlave and ElispSlave--the Slave
  # class isn't meant to be used itself.
  #
  class Slave

    # Ruby and elisp use these strings to terminate messages sent to
    # each other.  This ruby library and the elisp code are tied to
    # one another so closely that I don't know if it matters, but it
    # still seemed like a bad idea to hard code the constants in both
    # places.  Just make sure that the order in the elisp function
    # <tt>relisp-get-constants</tt> matches the ruby method
    # <tt>send_constants</tt>.
    CONSTANTS = Array.new
    CONSTANTS << QUESTION_CODE     = '___TOBEORNOTTOBE___'
    CONSTANTS << COMMAND_CODE      = '___THOUSHALT___'
    CONSTANTS << BEGIN_ANSWER_CODE = '___WHATIS___'
    CONSTANTS << ANSWER_CODE       = '___FORTYTWO___'
    CONSTANTS << ERROR_CODE        = '___NO_THATSNOTTRUE_THATSIMPOSSIBLE___'
    CONSTANTS << BEGIN_SLAVE_CODE  = '___LETSDOTHISLIKEBRUTUS___'
    TRANSMISSION_CODES_REGEXP   = Regexp.new(CONSTANTS.join('|'))
                                       
    # Every time ruby asks elisp to evaluate an expression, the result
    # is saved in this variable so ruby can access it if necessary.
    PREVIOUS_ELISP_RESULT = :"--relisp--previous--result" 
    CONSTANTS << PREVIOUS_ELISP_RESULT
    # A prefix for elisp variables created by ruby.
    VARIABLE_PREFIX        = '--relisp--variable--'

    def initialize
      # Whenever ruby calls 'eval' on some code at the request of
      # elisp it is in a context where any new variables set will drop
      # out of scope immediately.  The @local_binding is a way of
      # allowing these variables to persist through multiple calls.
      @local_binding = binding
      @current_elisp_variable_num = '0'
      @debug = nil
      Relisp.default_slave = self
    end

    # Return a symbol which corresponds to an unused variable in
    # elisp.
    #
    def new_elisp_variable
      (VARIABLE_PREFIX + @current_elisp_variable_num.succ!).to_sym
    end

    # Return a symbol corresponding to a new elisp variable which hold
    # the same information as _old_variable_. Intended primarily for
    # use in the +from_elisp+ method in various classes.
    #
    def get_permanent_variable(old_variable)
      permanent_variable = new_elisp_variable
      elisp_exec( "(setq #{permanent_variable} #{old_variable})" )
      return permanent_variable
    end

    # Run _code_ in the elisp process.
    #
    def elisp_exec(code)
      code = code.to_s # maybe code is nil or something
      write_to_emacs code
      write_to_emacs COMMAND_CODE
      receive_answer
    end
    
    # Run _code_ in the elisp process and return the result as the
    # corresponding ruby object.  If the ruby object is not going to
    # be used, use elisp_exec instead.
    #
    def elisp_eval(code)
      code = code.to_s # maybe code is a symbol or something
      write_to_emacs code
      write_to_emacs QUESTION_CODE
      to_ruby(receive_answer)
    end

    private 

    # Handle messages from emacs after <tt>elisp_exec</tt> or
    # <tt>elisp_eval</tt> are called.
    #
    def receive_answer
      output = ''
      output_line = read_from_emacs
      until output_line.strip == ANSWER_CODE
        if output_line.strip == QUESTION_CODE
          write_to_emacs((eval(output, @local_binding)).to_elisp)
          write_to_emacs ANSWER_CODE
          output = ''
        elsif output_line.strip == COMMAND_CODE
          eval output, @local_binding
          write_to_emacs ANSWER_CODE
          output = ''
        elsif output_line.strip == ERROR_CODE
          raise Relisp::ElispError, (eval output)
        else
          output << output_line
        end
      output_line = read_from_emacs
      end

      output.gsub!(/\n\z/, '')
      return output
    end

    # Pass an elisp evaluation result to the appropriate Relisp class
    # for translation.  The first line of _result_ is the
    # 'type-of' the elisp object.  The line(s) after that are the text
    # version of the object.  In case the string representation isn't
    # enough information to translate the object, the result needs to
    # be kept (in emacs) in the variable +PREVIOUS_ELISP_RESULT+.
    #
    def to_ruby(result)

      # The result might have junk at the beginning caused by
      # whatever emacs sends to the echo area while evaluating ruby's
      # instructions (it doesn't seem possible to get around this
      # using with-output-to-string because the output from
      # save-buffer and other functions is 'message'd).  It is also
      # not possible to just pull off the last two elements, because
      # sometimes the string representation of the result has newlines
      # in it (Buffer#to_s, for example).

      result = result.split("\n")
      start_index = result.index(BEGIN_ANSWER_CODE) + 1
      result = result[start_index..(result.size-1)]
      
      elisp_type = result.shift
      object_string = result.join("\n")

      object_info = {
        :string   => object_string,
        :variable => PREVIOUS_ELISP_RESULT,
        :slave    => self, 
      }

      # Just one more reason to love Ruby.  Call the Relisp class
      # formed by rubyizing the 'type-of' the result (i.e., hash-table
      # becomes HashTable).
      ruby_type = Relisp.const_get(elisp_type.split("-").map { |a| a.capitalize }.join)
      unless ruby_type.kind_of? Class
        raise "#{ruby_type} not implemented" 
      end

      ruby_type.from_elisp(object_info)
    end

    # Send the constants that ruby and elisp need to share.
    #
    def send_constants 
      CONSTANTS.each do |constant|
        read_from_emacs
        write_to_emacs constant.to_s + "\n"
      end
      read_from_emacs
    end

    public
    
    # Creates a method in the slave that is a reference to the
    # variable given by _symbol_ in the scope of _binding_. This is
    # probably only useful when calling elisp in ruby where the elisp
    # code itself calls ruby again. When the elisp process calls
    # +ruby_eval+ the code is executed inside the loop of the slave
    # object, so the variables in the scope of the original call to
    # elisp aren't normally available.
    #
    #    emacs = Relisp::ElispSlave.new
    #    number = 5
    #    emacs.elisp_eval('(ruby-eval "number")')  #  => [error]
    #
    #    emacs = Relisp::ElispSlave.new
    #    number = 5
    #    local_binding = binding
    #    emacs.provide(:number, local_binding)
    #    emacs.elisp_eval('(ruby-eval "number")')  #  => 5
    #
    def provide(symbol, binding)
      instance_variable_set "@__#{symbol.to_s}__binding".to_sym, binding

      instance_eval <<-endstr
        def #{symbol.to_s}
          eval("#{symbol.to_s}", @__#{symbol.to_s}__binding)
        end
      endstr
    end
  end
     

  # This class dedicates the ruby process to responding to queries
  # from the emacs process that started it.  See Relisp::Slave.
  #
  class RubySlave < Slave

    # Creates a new RubySlave and immediately starts it.
    #
    def self.start
      self.new.start
    end

    # Can be provided with a block, in which case the block is run in
    # the context of the slave.  This makes slave methods available to
    # the block without specifying an explicit receiver, and variables
    # and functions defined in the block are in scope when requests
    # from elisp are later evaluated.
    #
    def initialize
      super
      send_constants

      if block_given?
        yield self
        # start
      end
    end

    # Begin the main listening loop.
    #
    def start
      write_to_emacs BEGIN_SLAVE_CODE
      write_to_emacs ANSWER_CODE

      begin
        loop do
          code = ''
          input = read_from_emacs
          until input.strip == QUESTION_CODE || input.strip == COMMAND_CODE
            code << input
            input = read_from_emacs
          end
          code.gsub!(/\n\z/, '')
          
          if input.strip == QUESTION_CODE
            write_to_emacs((eval code, @local_binding).to_elisp)
          else
            eval(code, @local_binding)
          end
          write_to_emacs ANSWER_CODE
        end
      rescue => dag_yo
        msg = dag_yo.message + "\n"
        dag_yo.backtrace.each do |b|
          msg << "  " + b + "\n" 
        end
        write_to_emacs msg.chomp
        #write_to_emacs(dag_yo.message)
        write_to_emacs ERROR_CODE
        retry
      end
    end
    
    private
    
    # Emacs sends ruby's stdout to the filter function designated for
    # the ruby process.
    #
    def write_to_emacs(code)
#      puts code
      print code
    end
    
    # Messages appear on ruby's stdin after emacs sends them to ruby
    # process.
    #
    def read_from_emacs
      gets
    end
  end

  # Provides an interface to an instance of emacs started as an IO
  # object.  See Relisp::Slave.
  #
  class ElispSlave < Slave
    alias do elisp_eval

    # Start an emacs process, load the relisp library, and force the
    # process to become a slave to ruby's bidding.  The string
    # _cli_options_ specifies arguments to pass to emacs on the
    # command line, and _load_files_ is array of files to load (with
    # the '-l' command line option) after the relisp.el library.
    #
    def initialize(cli_options = "--no-site-file --no-init-file", load_files = [])
      super()
      # leave off '.el' to load relisp.elc if available
      elisp_path = File.expand_path(File.join(File.dirname(__FILE__), '../../src/relisp'))

      emacs_command = if RUBY_PLATFORM.downcase.include?('mswin')
                        "start emacs --batch "
                      else
                        "emacs --batch "
                      end
      emacs_command << cli_options
      emacs_command << " -l \"#{elisp_path}\""
      load_files.each do |file|
        emacs_command << " -l \"#{file}\""
      end
      emacs_command << " --eval '(relisp-become-slave)'"
      # In batch mode, emacs sends its normal output to stderr for
      # some reason.  I'm sure it's a good one...
      emacs_command << " 2>&1"
      @emacs_pipe = IO.popen(emacs_command, "w+")

      # gobble whatever output until emacs reports for duty
      until read_from_emacs.strip == "SEND CONSTANTS"; end
      send_constants
    end

    attr_accessor :debug

    # When given a block, runs the block with debugging turned on and
    # then restores the former status of debug messages.  Otherwise,
    # toggles the status of debug messages. 
    #
    def debugging
      if block_given?
        debug_original_val = @debug
        begin
          @debug = true
          puts
          puts "-----------------"
          result = yield
        ensure
          @debug = debug_original_val
          puts "-----------------"
        end
        return result
      else
        @debug = ! @debug
      end
    end

    private

    # Emacs reads from stdin and makes the input available to the
    # mini-buffer.
    #
    def write_to_emacs(code)
      if @debug
        puts "ruby> " + code.to_s unless code =~ TRANSMISSION_CODES_REGEXP
      end
      @emacs_pipe.puts code
    end
    
    # Emacs sends whatever it outputs by way of 'message' to stderr,
    # which is redirected to stdout when the emacs process is started
    # in initialize.
    #
    def read_from_emacs
      output = @emacs_pipe.gets
      if @debug
        puts "lisp> " + output unless output =~ TRANSMISSION_CODES_REGEXP
      end
      return output
    end
  end

end

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
#

module Relisp
  class Slave

    # Save point, mark, and current buffer; execute a block of code;
    # restore those things.
    #
    # This does not simply call the <tt>save-excursion</tt> function
    # in elisp, it is a rewrite to accept a ruby block.
    #
    def save_excursion 
      raise ArgumentError unless block_given?
      begin
        start_point  = point()
        start_mark   = mark()
        start_buffer = current_buffer()
        start_active = elisp_eval( "mark-active" )
        yield
      ensure
        set_buffer(start_buffer)
        set(:"mark-active", start_active)
        goto_char(start_point)
        set_mark(start_mark)
      end
    end

    # Save the current buffer; execute a block of code;
    # restore the current buffer.
    #
    # This does not simply call the <tt>with-current-buffer</tt>
    # function in elisp, it is a rewrite to accept a ruby block.
    #
    def with_current_buffer
      raise ArgumentError unless block_given?
      begin
        start_buffer = current_buffer()
        yield
      ensure
        set_buffer(start_buffer)
      end
    end

    # Create a temporary buffer, and evaluate a block of code there.
    #
    # This does not simply call the <tt>with-temp-buffer</tt> function
    # in elisp, it is a rewrite to accept a ruby block.
    #
    def with_temp_buffer
      raise ArgumentError unless block_given?
      begin
        start_buffer = current_buffer()
        temp_buffer = Relisp::Buffer.new("*temp--relisp--buffer*", self)
        yield
      ensure
        set_buffer(start_buffer)
        temp_buffer.kill
      end
    end

    # TODO:    save_selected_window
    # TODO:    with_selected_window

    private

    # Forward any missing method to elisp.  
    #
    # If a function with that name exists, write the rubyized (swap _
    # for -) function and arguments in prefix notation (calling the
    # +to_elisp+ method of each of the _args_).
    #
    # If a symbol with that name exists, return its value.  
    #
    # If the last char of the missing method is `=', set the symbol
    # formed by the characters before the `=' to the first argument.
    #
    # For example:
    #
    #   emacs = Relisp::ElispSlave.new
    #   puts emacs.concat "two", "words"
    #   emacs.a= 5
    #   puts emacs.a
    #
    # This automatically allows access to a large portion of elisp
    # in a rubyish way.  
    #
    def method_missing(function, *args) # :doc:
      lisp_name = function.to_s.gsub('_', '-')

      if elisp_eval "(functionp '#{lisp_name})"
        elisp_eval "(#{lisp_name} #{args.map{|a| a.to_elisp}.join(' ')})"
      elsif elisp_eval("(boundp '#{lisp_name})") && args.empty? # if there are args, it was meant to be a function
        elisp_eval "#{lisp_name}"
      elsif lisp_name[lisp_name.size-1..lisp_name.size-1] == "="
        elisp_eval "(setq #{lisp_name[0..lisp_name.size-2]} #{args[0].to_elisp})"
      else
        raise NameError, "#{function} is undefined variable/method/function in Ruby and Elisp"
      end
#      elisp_eval("(if (functionp '#{function}) (#{function} #{args.map{|a| a.to_elisp}.join(' ')}) #{function})")
    end
    
  end
end

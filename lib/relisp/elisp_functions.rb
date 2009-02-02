#--
# Copyright (C) 2009 <don@ohspite.net>
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
        start_active = elisp_execute( "mark-active" )
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

    # Forward any missing method to elisp, writing the function and
    # arguments in prefix notation (calling the +to_elisp+ method of
    # each of the _args_).  
    #
    # This automatically allows access to a large portion of elisp
    # functions a rubyish way.  
    #
    def method_missing(function, *args) # :doc:
      function = function.to_s.gsub('_', '-')
      unless elisp_eval("(functionp '#{function})")
        raise NameError, "#{function} is not an elisp function"
      end

      elisp_eval('(' + 
                 function + ' ' + 
                 args.map{|a| a.to_elisp}.join(' ') +
                 ')')
    end
    
  end
end

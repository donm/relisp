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
# See the documentation in <tt>programming_types.rb</tt>.
#
# Elisp editing types as described in the elisp info manual:
#
#   Editing Types
#
#   Buffer Type::         The basic object of editing.
#   Marker Type::         A position in a buffer.
#   Window Type::         Buffers are displayed in windows.
#   Frame Type::	    Windows subdivide frames.
#   Window Configuration Type::   Recording the way a frame is subdivided.
#   Frame Configuration Type::    Recording the status of all frames.
#   Process Type::        A process running on the underlying OS.
#   Stream Type::         Receive or send characters.
#   Keymap Type::         What function a keystroke invokes.
#   Overlay Type::        How an overlay is represented.  
#
# Unlike with elisp programming data types, ruby does not translate
# the editing data types to an equivalent ruby object; instead, the
# class corresponding to each data type is just a proxy that acts on
# the original object in elisp.

module Relisp

  # A Buffer object just holds onto the Emacs buffer in an elisp
  # variable and acts as a proxy.
  #
  class Buffer
    def self.from_elisp(object)
      new(object[:variable], object[:slave])
    end

    # When _var_or_name_ is a symbol it is considered to be the name
    # of a pre-existing buffer in the _slave_ process.  If
    # _var_or_name_ is a string, a new buffer is created with a name
    # based on that string. 
    def initialize(var_or_name, slave = Relisp.default_slave)
      @slave = slave

      if var_or_name.kind_of?( Symbol )
        @elisp_variable = @slave.get_permanent_variable(var_or_name)

        unless @slave.elisp_eval("(type-of #{@elisp_variable})") == :buffer
          raise ArgumentError, "#{@elisp_variable} isn't a buffer"
        end
      elsif var_or_name.kind_of?( String )
        @slave.elisp_execute("(generate-new-buffer #{var_or_name.to_elisp})")
        @elisp_variable = @slave.get_permanent_variable(Relisp::Slave::PREVIOUS_ELISP_RESULT)
      else
        raise ArgumentError
      end
    end

    def to_elisp
      @elisp_variable
    end

    # Return the name of the buffer.
    #
    def name
      @slave.elisp_eval "(buffer-name #{@elisp_variable})"
    end
  end

end


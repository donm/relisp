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

  class Proxy
    def self.from_elisp(object)
      new(object[:variable], object[:slave])
    end

    def initialize(*args)
      @slave = if args.last.kind_of?(Relisp::Slave)
                 args.pop
               else
                 Relisp.default_slave
               end

      if args[0].kind_of?(Symbol)
        @elisp_variable = @slave.get_permanent_variable(args[0])
        elisp_type= ""
        self.class.to_s.split("::").last.split(//).each_with_index do |char, index|
          elisp_type << char.downcase
          unless index==0 || char == char.downcase
            elisp_type << "-"
          end
        end

        unless @slave.elisp_eval("(type-of #{@elisp_variable})") == elisp_type.to_sym
          raise ArgumentError, "#{@elisp_variable} isn't a #{elisp_type}"
        end
      else
        @elisp_variable = @slave.new_elisp_variable
        yield args
      end
    end
  
    attr_reader :slave, :elisp_variable

    def to_elisp
      @elisp_variable
    end
  end

  # A Buffer object is a proxy to an Emacs buffer.
  #
  class Buffer < Proxy

    # _args_ can be any of these forms:
    # * (_symbol_, <em>slave = Relisp.default_slave</em>)
    # * (_string_, <em>slave = Relisp.default_slave</em>)
    # * (<em>slave = Relisp.default_slave</em>)
    #
    # When a _symbol_ is given it is considered to be the name of a
    # pre-existing bufer in the _slave_ process.  Otherwise a new,
    # buffer is created (<tt>generate-new-buffer</tt>).  The name is
    # _string_, if given, and a variant of "relisp-buffer" otherwise.
    #
    def initialize(*args)
      super do |args|
        name = args[0] ? args[0] : "relisp-buffer"
        raise ArgumentError unless name.kind_of?(String)
        @slave.elisp_execute( "(setq #{@elisp_variable} (generate-new-buffer #{name.to_elisp}))" )      
      end
    end

    # Return the name of Buffer, as a string (<tt>buffer-name</tt>).
    def name
      @slave.buffer_name(@elisp_variable.value)
    end

    # Save the buffer in its visited file, if it has been modified.
    #
    def save
      @slave.elisp_eval <<-EOM
        (save-excursion
          (set-buffer #{@elisp_variable})
          (save-buffer))
      EOM
    end
  end

  # A Marker object is a proxy to an Emacs marker.
  #
  class Marker < Proxy
    def self.from_elisp(object)
      new(object[:variable], object[:slave])
    end

    attr_reader :slave, :elisp_variable

    # _args_ can be any of these forms:
    # * (_symbol_, <em>slave = Relisp.default_slave</em>)
    # * (<em>slave = Relisp.default_slave</em>)
    #
    # When a _symbol_ is given it is considered to be the name of a
    # pre-existing marker in the _slave_ process.  Otherwise a new,
    # empty marker is created (<tt>make-marker</tt>).
    #
    def initialize(*args)
      super do 
        @slave.elisp_execute( "(setq #{@elisp_variable} (make-marker))" )
      end
    end

    # Return a newly allocated marker which does not point at any
    # position (<tt>make-marker</tt>).
    #
    def self.make(slave = Relisp.default_slave)
      slave.make_marker
    end

    def to_elisp
      @elisp_variable
    end
  end
end


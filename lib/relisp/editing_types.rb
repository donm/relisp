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

  # Proxy contains the code that creates a wrapper around a variable
  # in emacs.
  #
  class Proxy
    def self.from_elisp(object)
      new(object[:variable], object[:slave])
    end

    # If the last argument is a Relisp::Slave, it gets pulled off and
    # used as the slave; otherwise Relisp.default_slave is used.  If
    # the first argument is a Symbol, it is assumed to be the name of
    # an elisp variable which needs a proxy.  If the first argument
    # isn't a Symbol, all of the arguments (except the last, if it was
    # a Slave) are send off to the child to handle.
    #
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
          unless index==0 || char == char.downcase
            elisp_type << "-"
          end
          elisp_type << char.downcase
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

  # A proxy to an Emacs buffer.
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

  # A  proxy to an Emacs marker.
  #
  class Marker < Proxy

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

  end

  # A  proxy to an Emacs window
  #
  class Window < Proxy

    # _args_ must be of the form (_symbol_, <em>slave =
    # Relisp.default_slave</em>)
    #
    # The _symbol_ argument is considered to be the name of a
    # pre-existing window in the _slave_ process.  
    #
    def initialize(*args)
      super do 
        raise ArgumentError, "Cannot create Window using 'new' method."
      end
    end
  end

  # A proxy to an Emacs frame
  #
  class Frame < Proxy

    # _args_ can be any of these forms:
    # * (_symbol_, <em>slave = Relisp.default_slave</em>)
    # * (<em>option_hash = {}</em>, <em>slave = Relisp.default_slave</em>)
    # * (<em>slave = Relisp.default_slave</em>)
    #
    # When a _symbol_ is given it is considered to be the name of a
    # pre-existing frame in the _slave_ process.  Otherwise a new,
    # frame is created using any options in (<tt>new-frame</tt>).
    #
    # The _option_hash_ can specify the following
    # [<tt>:name =></tt> _string_] 
    #      The frame should be named _string_.
    # [<tt>:width =></tt> _fixnum_] 
    #      The frame should be _fixnum_ characters in width.
    # [<tt>:height =></tt> _fixnum_] 
    #      The frame should be _fixnum_ text lines high.
    #
    # You cannot specify either :width or :height, you must use neither or both.
    #
    # [<tt>:minibuffer => true</tt>]
    #      The frame should have a minibuffer.
    # [<tt>:minibuffer => nil</tt>]
    #      The frame should have no minibuffer.
    # [<tt>:minibuffer => :only</tt>] 
    #      The frame should contain only a minibuffer.
    # [<tt>:minibuffer =></tt> _window_] 
    #      The frame should use _window_ as its minibuffer window.
    #
    # [<tt>:"window-system" => nil</tt>] 
    #      The frame should be displayed on a terminal device.
    # [<tt>:"window-system" => :x</tt>] 
    #      The frame should be displayed in an X window.
    # [<tt>:terminal =></tt> _id_] 
    #      The frame should use the terminal identified by _id_.
    #
    def initialize(*args)
      super do |args|
        hash = args[0]
        alist = ""
        if hash && hash.size > 1
          alist << "'("
          hash.each_pair do |key, val|
            val = if val.kind_of?(Symbol)
                    val.value.to_elisp
                  else
                    val.to_elisp
                  end
            alist << "(#{key} . #{val.to_s}) "
          end
          alist << ")"
        end

        @slave.elisp_execute( "(setq #{@elisp_variable} (new-frame #{alist}))" )
      end
    end

  end

  # A  proxy to an Emacs window-configuration
  #
  class WindowConfiguration < Proxy

    # _args_ must be of the form (_symbol_, <em>slave =
    # Relisp.default_slave</em>)
    #
    # The _symbol_ argument is considered to be the name of a
    # pre-existing window-configuration in the _slave_ process.  
    #
    def initialize(*args)
      super do 
        raise ArgumentError, "Cannot create WindowConfiguration using 'new' method."
      end
    end
  end

  # A  proxy to an Emacs frame-configuration
  #
  class FrameConfiguration < Proxy

    # _args_ must be of the form (_symbol_, <em>slave =
    # Relisp.default_slave</em>)
    #
    # The _symbol_ argument is considered to be the name of a
    # pre-existing frame-configuration in the _slave_ process.  
    #
    def initialize(*args)
      @elisp_type = :cons
      @elisp_test_type_function = "frame-configuration-p"

      super do 
        raise ArgumentError, "Cannot create FrameConfiguration using 'new' method."
      end
    end
  end


#   # A  proxy to an Emacs [OBJECT]
#   #
#   class [CLASS] < Proxy

#     # _args_ can be any of these forms:
#     # * (_symbol_, <em>slave = Relisp.default_slave</em>)
#     # * (<em>slave = Relisp.default_slave</em>)
#     #
#     # When a _symbol_ is given it is considered to be the name of a
#     # pre-existing [TYPE] in the _slave_ process.  Otherwise a new,
#     # empty [TYPE] is created (<tt>[FUNCTION]</tt>).
#     #
#     def initialize(*args)
#       super do 
#         @slave.elisp_execute( "(setq #{@elisp_variable} (FUNCTION))" )
#       end
#     end
#   end

end


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

    def initialize(old_elisp_variable, slave)
      @elisp_variable = slave.get_permanent_variable(old_elisp_variable)

      unless slave.elisp_eval("(type-of #{@elisp_variable})") == :buffer
        raise ArgumentError, "#{@elisp_variable} isn't a buffer"
      end

      @slave = slave
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


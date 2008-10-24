class Object
  def to_elisp
    to_s
  end
end

module Relisp
  class List < Array
    def to_elisp
      '(' + join(' ') + ')'
    end
  end

  class Array < Array
    def to_elisp
      '[' + join(' ') + ']'
    end
  end
end

class String
  def to_ruby_from_elisp
  end
end

class Float
  def to_elisp
    to_s
  end
end

class String
  def to_elisp
    '"' + self + '"'
  end
end

class Integer # base of Bignum and Fixnum 
  # ruby ints are 31 bit (but move to Bignum after that), emacs are 29 and wrap
  def to_elisp
    to_s
  end
end

class Array
  @@default_elisp_type = Relisp::List

  def self.default_elisp_type=(type)
    @@default_elisp_type = type
  end

  def elisp_type
    @elisp_type ||= @@default_elisp_type
  end

  def elisp_type=(type)
    @elisp_type = type
  end

  def to_elisp
    elisp_type.new(self).to_elisp
  end
end

### Programming Types
## Integer Type::        Numbers without fractional parts.
## Floating Point Type:: Numbers with fractional parts and with a large range.
# Character Type::      The representation of letters, numbers and
#  control characters.
# Symbol Type::         A multi-use object that refers to a function,
#                       variable, or property list, and has a unique identity.
# Sequence Type::       Both lists and arrays are classified as sequences.
# Cons Cell Type::      Cons cells, and lists (which are made from cons cells).
# Array Type::          Arrays include strings and vectors.
## String Type::         An (efficient) array of characters.
# Vector Type::         One-dimensional arrays.
# Char-Table Type::     One-dimensional sparse arrays indexed by characters.
# Bool-Vector Type::    One-dimensional arrays of `t' or `nil'.
# Hash Table Type::     Super-fast lookup tables.
# Function Type::       A piece of executable code you can call from elsewhere.
# Macro Type::          A method of expanding an expression into another
#                           expression, more fundamental but less pretty.
# Primitive Function Type::     A function written in C, callable from Lisp.
# Byte-Code Type::      A function written in Lisp, then compiled.
# Autoload Type::       A type used for automatically loading seldom-used
#                         functions.

### Editing Types
# Buffer Type::         The basic object of editing.
# Marker Type::         A position in a buffer.
# Window Type::         Buffers are displayed in windows.
# Frame Type::		Windows subdivide frames.
# Window Configuration Type::   Recording the way a frame is subdivided.
# Frame Configuration Type::    Recording the status of all frames.
# Process Type::        A process running on the underlying OS.
# Stream Type::         Receive or send characters.
# Keymap Type::         What function a keystroke invokes.
# Overlay Type::        How an overlay is represented.



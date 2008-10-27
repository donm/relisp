module Relisp
  def self.method_missing(function, *args)
    elisp_eval('(' + function.to_s + ' ' + args.map{|a| a.print}.join(' ') + ')')
  end

  VARIABLE_PREFIX = '--reserved--relisp--variable--'
  @@current_elisp_variable = '0'

  def self.new_elisp_variable
    VARIABLE_PREFIX + @@current_elisp_variable.succ!
  end

  def self.elisp_eval(code)
    elisp_result = elisp_execute(code)
    elisp_object_variable = new_elisp_variable
    elisp_execute("(setq #{elisp_object_variable} elisp-eval-result)")

    read(elisp_result, elisp_object_variable)
  end

    ### Programming Types
    ### Integer Type::        Numbers without fractional parts.
    ### Floating Point Type:: Numbers with fractional parts and with a large range.
    # Character Type::      The representation of letters, numbers and
    #  control characters.
    # Symbol Type::         A multi-use object that refers to a function,
    #                       variable, or property list, and has a unique identity.
    # Sequence Type::       Both lists and arrays are classified as sequences.
    # Cons Cell Type::      Cons cells, and lists (which are made from cons cells).
    # Array Type::          Arrays include strings and vectors.
    ### String Type::         An (efficient) array of characters.
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
    ### Buffer Type::         The basic object of editing.
    # Marker Type::         A position in a buffer.
    # Window Type::         Buffers are displayed in windows.
    # Frame Type::		Windows subdivide frames.
    # Window Configuration Type::   Recording the way a frame is subdivided.
    # Frame Configuration Type::    Recording the status of all frames.
    # Process Type::        A process running on the underlying OS.
    # Stream Type::         Receive or send characters.
    # Keymap Type::         What function a keystroke invokes.
    # Overlay Type::        How an overlay is represented.    case elisp_type
    
  def self.read(object_string, object_variable = nil)
    if object_variable
      elisp_type = elisp_execute "(type-of #{object_variable})"
      object_string = elisp_execute(object_variable)
    else
      elisp_type = elisp_execute "(type-of #{object_string})"
    end 

    case elisp_type
    when 'integer'
      object_string.to_i
    when 'float'
      object_string.to_f
#    when 'vector'
#      Relisp::Vector.new(eval object_string)
    when 'string'
      Relisp::String.new(object_string)
    when 'buffer'
      Relisp::Buffer.new(object_variable)
    end
  end

class Buffer
  def initialize(elisp_variable)
    @elisp_variable = elisp_variable
  end

  def to_s
  end

  def name
    Relisp.elisp_eval "(buffer-name #{@elisp_variable})"
  end

end

class List < Array
  def print
    '(' + join(' ') + ')'
  end
end

class Vector < Array
  def print
    '[' + join(' ') + ']'
  end
end

class String < String
  def print
    self
  end
end

class Character < String
  def print
    self[0].to_s
  end
end

Float   = (3.14159).class
Integer = 42.class
end

### Programming Types
# Integer Type::        Numbers without fractional parts.
# Floating Point Type:: Numbers with fractional parts and with a large range.
# Character Type::      The representation of letters, numbers and
#  control characters.
# Symbol Type::         A multi-use object that refers to a function,
#                       variable, or property list, and has a unique identity.
# Sequence Type::       Both lists and arrays are classified as sequences.
# Cons Cell Type::      Cons cells, and lists (which are made from cons cells).
# Array Type::          Arrays include strings and vectors.
# String Type::         An (efficient) array of characters.
# Vector Type::         One-dimensional arrays.
# Char-Table Type::     One-dimensional sparse arrays indexed by characters.
# Bool-Vector Type::    One-dimensional arrays of `t' or `nil'.
# Hash Table Type::     Super-fast lookup tables.
# Function Type::       A piece of executable code you can call from elsewhere.
# Macro Type::          A method of expanding an expression into another
#                           expression, more fundamental but less pretty.
# Primitive Function Type::     A function written in C, callable from Lisp.

class String
  def to_elisp
    Relisp::String.new(self)
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
    elisp_type.new(self)
  end
end

class Object
  def print
    to_s
  end

  def to_elisp
    self
  end
end





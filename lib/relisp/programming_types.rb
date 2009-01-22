module Relisp
  ### Programming Types

  ### Integer Type::        Numbers without fractional parts.
  ### Floating Point Type:: Numbers with fractional parts and with a large range.
  #int# Character Type::      The representation of letters, numbers and
  #           control characters.
  ### Symbol Type::         A multi-use object that refers to a function,
  #                       variable, or property list, and has a unique identity.
  ### (Sequence Type)::       Both lists and arrays are classified as sequences.
  ### Cons Cell Type::      Cons cells, and lists (which are made from cons cells).
  ### (Array Type)::          Arrays include strings and vectors.
  ###   String Type::         An (efficient) array of characters.
  ###   Vector Type::         One-dimensional arrays.
  # Char-Table Type::     One-dimensional sparse arrays indexed by characters.
  # Bool-Vector Type::    One-dimensional arrays of `t' or `nil'.
  ### Hash Table Type::     Super-fast lookup tables.
  #cons# Function Type::       A piece of executable code you can call from elsewhere.
  #cons# Macro Type::          A method of expanding an expression into another
  #                           expression, more fundamental but less pretty.
  #xxx Primitive Function Type::     A function written in C, callable from Lisp.
  #xxx Byte-Code Type::      A function written in Lisp, then compiled.
  # Autoload Type::       A type used for automatically loading seldom-used
  #                         functions.

  Integer = 42.class
  class Integer
    def self.from_elisp(object)
      object[:string].to_i
    end
  end

  Float = (3.14).class
  class Float
    def self.from_elisp(object)
      object[:string].to_f
    end
  end

  Symbol = :flag.class
  class Symbol
    def self.from_elisp(object)
      if object[:string] == 'nil'
        nil
      elsif object[:string] == 't'
        true
      else
        object[:string].to_sym
      end
    end

    def to_elisp
#      self.to_s
      "'" + self.to_s
    end
  end

  class Cons < Array
    def self.from_elisp(object)
      new(super(object))
    end

    def to_elisp
      print_string = '(list '
      each do |elt|
        print_string << elt.to_elisp << ' '
      end
      print_string << ')'
    end
  end

  String = "words, words, words".class
  class String
    def self.from_elisp(object)
      new(eval(object[:string]))
    end

    def to_elisp
      self.dump
    end
  end

  class Vector < Array
    def self.from_elisp(object)
      new(super(object))
    end

    def to_elisp
      print_string = '[ '
      each do |elt|
        print_string << elt.to_elisp << ' '
      end
      print_string << ']'
    end
  end

  HashTable = {:money => "power"}.class
  class HashTable
    def self.from_elisp(object)
      slave = object[:slave]
      object_variable = slave.get_permament_variable(object[:variable])

      keys_var = slave.new_elisp_variable
      vals_var = slave.new_elisp_variable
      slave.elisp_execute( "(setq #{keys_var} nil)" )
      slave.elisp_execute( "(setq #{vals_var} nil)" )
      slave.elisp_execute( "(maphash (lambda (key val)
                              (setq #{keys_var} (append #{keys_var} (list key)))
                              (setq #{vals_var} (append #{vals_var} (list val)))) #{object_variable})" )
      keys = slave.elisp_eval( keys_var )
      vals = slave.elisp_eval( vals_var )
      keys ||= []
      hash = Hash.new
      keys.each_index do |i|
        hash[keys[i]] = vals[i]
      end

      slave.makunbound(object_variable)
      slave.makunbound(keys_var)
      slave.makunbound(vals_var)

      return hash
    end

    def to_elisp
      lisp =  "(progn \n"
      lisp << "  (let ((--temp--relisp--variable (make-hash-table))) \n"
      each_pair do |key, val|
        lisp << "    (puthash #{key.to_elisp} #{val.to_elisp} --temp--relisp--variable) \n"
      end
      lisp << "--temp--relisp--variable))"
    end
  end

end


class Object
  def to_elisp
    self.to_s
  end

  def from_elisp(*args)
    nil
  end
end


class NilClass
  def to_elisp
    "nil"
  end
end

class TrueClass
  def to_elisp
    "t"
  end
end

class FalseClass
  def to_elisp
    nil.to_elisp
  end
end

# Modify the normal Array class so that an array can be converted to
# either Relisp::Cons or Relisp::Vector.
class Array
  @@default_elisp_type = Relisp::Cons

  def self.from_elisp(object)
    object_variable = object[:slave].get_permament_variable(object[:variable])
    size = object[:slave].elisp_eval( "(length #{object_variable})" )
    object_array = new
    size.times do |i|
      object_array << object[:slave].elisp_eval( "(elt #{object_variable} #{i.to_elisp})" )
    end

    object[:slave].elisp_execute( "(makunbound #{object_variable.to_elisp})" )
    return object_array
  end

  # Set the type of the 
  def self.default_elisp_type=(type)
#    fail unless type.kind_of?(Array)
    @@default_elisp_type = type
  end

  def elisp_type
    @elisp_type || @@default_elisp_type
  end

  def elisp_type=(type)
    @elisp_type = type
  end

  def to_elisp
    elisp_type.new(self).to_elisp
  end
end



# (setq bar (lambda nil
#            (+ 1 2)))
# => (lambda nil (+ 1 2))

# (type-of (car '(+ 1 2)))
# => symbol

# (type-of (car bar))
# => symbol

# (functionp bar)
# => t

# (functionp 'bar)
# => nil

# (funcall bar)
# => 3

# (prin1-to-string bar)
# => "(lambda nil (+ 1 2))"

# (defun foo nil
#  (+ 1 2))
# => foo

# (functionp 'foo)
# => t

# (type-of (symbol-function 'foo))
# => cons

# (type-of (symbol-function 'car))
# => subr


# Straight from the elisp info manual:
#
#   Programming Types
#
#   Integer Type::        Numbers without fractional parts.
#   Floating Point Type:: Numbers with fractional parts and with a large range.
#   Symbol Type::         A multi-use object that refers to a function,
#                           variable, or property list, and has a unique identity.
#   (Sequence Type)::     Both lists and arrays are classified as sequences.
#     Cons Cell Type::    Cons cells, and lists (which are made from cons cells).
#   (Array Type)::        Arrays include strings and vectors.
#     String Type::       An (efficient) array of characters.
#     Vector Type::       One-dimensional arrays.
#   Hash Table Type::     Super-fast lookup tables.
#
# These are also elisp programming types, but Relisp doesn't
# explicitly deal with them.  Characters are basically just integers,
# and functions and macros are cons.
#
#   Character Type::      The representation of letters, numbers and
#                           control characters.
#   Function Type::       A piece of executable code you can call from elsewhere.
#   Macro Type::          A method of expanding an expression into another
#                             expression, more fundamental but less pretty.
#
# And then these elisp types are ignored completely.
#
#   Char-Table Type::     One-dimensional sparse arrays indexed by characters.
#   Bool-Vector Type::    One-dimensional arrays of `t' or `nil'.
#   Primitive Function Type::     A function written in C, callable from Lisp.
#   Byte-Code Type::      A function written in Lisp, then compiled.
#   Autoload Type::       A type used for automatically loading seldom-used
#                           functions.
#
# Every type in the first group obviously matches a ruby class except
# for 'Cons' and 'Vector'.  The types that have an analogous ruby
# class are mapped directly to that class.  'Cons' and 'Vector' are
# mapped to a corresponding subclass of Array; the only difference
# between the two is how they are translated back to elisp.
#
# Every ruby class that corresponds to a elisp data type is duplicated
# inside the Relisp module with the rubyized version of the elisp
# name; for example, Relisp::Integer is exactly the same as Fixnum and
# Relisp::HashTable is the same as Hash.  
#
# Every class needs to have a +from_elisp+ method that accepts a hash
# with the object's string representation, variable name (in the elisp
# process) and the Slave instance that created the object.  The method
# returns a ruby object analogous to the elisp value.
#
# Every object needs a +to_elisp+ method that creates a string which
# results in a elisp value equivalent to the ruby object when read and
# evaluated in elisp (i.e. <tt>elisp_eval "(eval (read
# #{obj.to_elisp}))"</tt>).  For most objects this basically amounts
# to a variation on the +to_s+ method.  However, for objects that
# don't have a string representation (hashes, buffers, frames, ...)
# the +to_elisp+ method actually results in elisp code that, when run,
# returns the appropriate object.

module Relisp

  Integer = 1.class
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
      object_variable = slave.get_permanent_variable(object[:variable])

      unless slave.elisp_eval("(type-of #{object_variable})") == "hash-table".to_sym
        raise ArgumentError, "#{object_variable} isn't a hash-table"
      end
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


# Every object is given a default +to_elisp+ method, and classes get a
# dummy +from_elisp+ method.
#
class Object
  def to_elisp
    self.to_s
  end

  def self.from_elisp(*args)
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
  # Falseness in elisp is represented by 'nil'.
  def to_elisp
    nil.to_elisp
  end
end


# The normal Array class is modified so that an array can be converted
# to either Relisp::Cons or Relisp::Vector.
#
class Array
  @@default_elisp_type = Relisp::Cons

  # Converts either a 'cons' or 'vector' to a ruby array.
  def self.from_elisp(object)
    object_variable = object[:slave].get_permanent_variable(object[:variable])
    size = object[:slave].elisp_eval( "(length #{object_variable})" )
    object_array = new
    size.times do |i|
      object_array << object[:slave].elisp_eval( "(elt #{object_variable} #{i.to_elisp})" )
    end

    object[:slave].elisp_execute( "(makunbound #{object_variable.to_elisp})" )
    return object_array
  end

  # Set the elisp type that ruby arrays are converted to by default.
  def self.default_elisp_type=(type)
    unless type.ancestors.include?(Array)
      raise ArgumentError, "#{type} is not a kind of Array" 
    end

    @@default_elisp_type = type
  end

  def self.default_elisp_type
    @@default_elisp_type
  end

  # The elisp type that this array will be converted to by to_elisp.
  def elisp_type
    @elisp_type || @@default_elisp_type
  end

  def elisp_type=(type)
    unless type.ancestors.include?(Array)
      raise ArgumentError, "#{type} is not a kind of Array" 
    end

    @elisp_type = type
  end

  def to_elisp
    elisp_type.new(self).to_elisp
  end
end

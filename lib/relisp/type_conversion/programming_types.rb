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
# And then these elisp types are ignored completely, at least for now.
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
# class are mapped directly to that class.  'Cons' is a kept in a
# proxy object in the same way as the editting data types.  'Vector'
# is mapped to a corresponding subclass of Array.
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

Relisp::Integer   = Integer
Relisp::Float     = Float
Relisp::Symbol    = Symbol
Relisp::String    = String
Relisp::HashTable = Hash

module Relisp

  class Integer
    def self.from_elisp(object)
      object[:string].to_i
    end
  end

  class Float
    def self.from_elisp(object)
      object[:string].to_f
    end
  end

  class Symbol
    def self.from_elisp(object)
      if object[:string] == 'nil'
        nil
      elsif object[:string] == 't'
        true
      else
#        object[:string].gsub('-', '_').to_sym
        object[:string].to_sym
      end
    end

    def to_elisp
#      "'" + self.to_s.gsub('_', '-')
      "'" + self.to_s
    end

    def value
      VariableValue.new(self)
    end
  end

  # This exists so that the value of a Relisp::Symbol type can be
  # conveniently passed to elisp.
  #   
  #   @slave.buffer_name(@elisp_variable)
  #      => Relisp::ElispError: Wrong type argument: bufferp, --relisp--variable--1
  #   @slave.buffer_name(@elisp_variable.value)
  #      => "my buffer"
  #
  class VariableValue
    def initialize(variable)
      @variable = variable
    end

    def to_elisp
      @variable.to_s
    end
  end

  # A proxy to an Emacs cons. If the cons is actually a list, the
  # to_list method will convert it a subclass of Array so that all of
  # the ruby Array stuff is available.
  #
  class Cons < Proxy

    # _args_ can be any of these forms:
    # * (_symbol_, <em>slave = Relisp.default_slave</em>)
    # * (_car, _cdr_, <em>slave = Relisp.default_slave</em>)
    #
    # When a _symbol_ is given it is considered to be the name of a
    # pre-existing cons in the _slave_ process.  Otherwise a new cons
    # is created with the given _car_ and _cdr
    # (<tt>cons</tt>).
    #
    def initialize(*args)
      super do |car, cdr|
        @slave.elisp_exec( "(setq #{@elisp_variable} (cons #{car.to_elisp} #{cdr.to_elisp}))")
      end
    end

    # Set the +car+ of Cons to be _newcar_ (+setcar+).
    #
    def car=(newcar)
      @slave.setcar(@elisp_variable.value, newcar)
    end

    # Set the +cdr+ of Cons to be _newcdr_. (+setcdr+).
    #
    def cdr=(newcdr)
      @slave.setcdr(@elisp_variable.value, newcdr)
    end

    ##
    # :method: car
    # car, cdr, cadr, etc. are taken care of by Proxy.method_missing.

    ##
    # :method: cdr

    ##
    # :method: cadr

    ##
    # :method: cdar

    # This function will NOT return true whenever the elisp function
    # <tt>listp</tt> is true.  The elisp function is true whenever the
    # object is a cons cell, but this method is true only when the
    # cons can be unambiguously translated to an array; this condition
    # is satisfied when the object in question could have been written
    # using the elisp function +list+.
    #
    def list?
      # current_cdr = cdr
      # while current_cdr.kind_of?(Cons)
      #   current_cdr = current_cdr.cdr
      # end
      # return ! current_cdr.nil?

      current_cdr_var = @slave.new_elisp_variable
      @slave.elisp_exec "(setq #{current_cdr_var} (cdr #{to_elisp}))"
      while @slave.elisp_eval "(consp #{current_cdr_var})"
        @slave.elisp_exec "(setq #{current_cdr_var} (cdr #{current_cdr_var}))"
      end
      cdr = @slave.elisp_eval "#{current_cdr_var}"
      @slave.elisp_exec "(makunbound '#{current_cdr_var})"

      return ! cdr
    end

    # Translate the cons cell into a Relisp::List, a subclass of
    # Array.  See list? for when this can be done.
    #
    def to_list
      list_array = []

      # list_array << @slave.elisp_eval("(car #{to_elisp})")
      # current_cdr = @slave.elisp_eval("(cdr #{to_elisp})")
      # while current_cdr.kind_of?(Cons)
      #   list_array << @slave.elisp_eval("(car #{current_cdr.to_elisp})")
      #   current_cdr = @slave.elisp_eval("(cdr #{current_cdr.to_elisp})")
      # end

      # if current_cdr.nil?
      #   return Relisp::List.new(list_array)
      # else
      #   return false
      # end

      current_cdr_var = @slave.new_elisp_variable

      list_array << @slave.elisp_eval("(car #{to_elisp})")
      @slave.elisp_exec "(setq #{current_cdr_var} (cdr #{to_elisp}))"
      while @slave.elisp_eval "(consp #{current_cdr_var})"
        list_array << @slave.elisp_eval("(car #{current_cdr_var})")
        @slave.elisp_exec "(setq #{current_cdr_var} (cdr #{current_cdr_var}))"
      end

      cdr = @slave.elisp_eval "#{current_cdr_var}"

      @slave.elisp_exec "(makunbound '#{current_cdr_var})"

      if cdr
        raise "Not a list: #{cdr}"
      end

      return Relisp::List.new(list_array)
    end

    alias to_a to_list
  end

  class List < Array
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

  class String
    def self.from_elisp(object)
      new(eval(object[:string]))
    end

    def to_elisp
      # convert "\u{0000}" to "\u0000"
      self.dump.gsub(/\\u\{([0-9a-e]+)\}/) { |m| sprintf("\\u%04x", $1.to_i(16)) }
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

  class HashTable
    def self.from_elisp(object)
      slave = object[:slave]
      object_variable = slave.get_permanent_variable(object[:variable])

      unless slave.elisp_eval("(type-of #{object_variable})") == "hash-table".to_sym
        raise ArgumentError, "#{object_variable} isn't a hash-table"
      end
      keys_var = slave.new_elisp_variable
      vals_var = slave.new_elisp_variable
      slave.elisp_exec( "(setq #{keys_var} nil)" )
      slave.elisp_exec( "(setq #{vals_var} nil)" )
      slave.elisp_exec( "(maphash (lambda (key val)
                 (setq #{keys_var} (append #{keys_var} (list key)))
                 (setq #{vals_var} (append #{vals_var} (list val)))) 
               #{object_variable})" )
      keys = Cons.new(keys_var, slave)
      vals = Cons.new(vals_var, slave)
      keys = keys.to_list
      vals = vals.to_list
      hash = Hash.new
      keys.each_index do |i|
        hash[keys[i]] = vals[i]
      end

      [:object_variable, :keys_var, :vals_var]. each do |var|
        slave.elisp_exec "(makunbound '#{var})"
      end

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

class Class
  # Convert classes to the symbol form of their name
  def to_elisp
    self.to_s.to_sym.to_elisp
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
  # Because falseness in elisp is represented by 'nil'.
  def to_elisp
    nil.to_elisp
  end
end

# The normal Array class is modified so that an array can be converted
# to either Relisp::List or Relisp::Vector.
#
class Array
  @@default_elisp_type = Relisp::List

  # Converts either a 'cons' or 'vector' to a ruby array.
  def self.from_elisp(object)
    object_variable = object[:slave].get_permanent_variable(object[:variable])
    size = object[:slave].elisp_eval( "(length #{object_variable})" )
    object_array = new
    size.times do |i|
      object_array << object[:slave].elisp_eval( "(elt #{object_variable} #{i.to_elisp})" )
    end

    object[:slave].elisp_exec( "(makunbound #{object_variable.to_elisp})" )
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
    @elisp_type = nil unless defined?(@elisp_type) #to avoid uninitialized warning
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



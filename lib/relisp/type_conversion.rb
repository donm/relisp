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
#

module Relisp

  # Proxy contains the code that creates a wrapper around a variable
  # in emacs.
  #
  class Proxy
    def self.from_elisp(object)
      slave = object[:slave]
      new(slave.get_permanent_variable(object[:variable]), slave)
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

      if args[0].kind_of?(Symbol) && args[1].nil?
#        @elisp_variable = @slave.get_permanent_variable(args[0])
        @elisp_variable = args[0]
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

    def makunbound
      @slave.makunbound @elisp_variable
    end

    def self.elisp_alias(ruby_name, elisp_name)
      class_eval <<-EOM
        def #{ruby_name}
          call_on_self :#{elisp_name}
        end
      EOM
    end

    private 
    
    def call_on_self(function, *args)
      @slave.send(function, @elisp_variable.value, *args)
    end

    def method_missing(function, *args)
      call_on_self(function, *args)
    end
  end

end

require 'relisp/type_conversion/editing_types'
require 'relisp/type_conversion/programming_types'


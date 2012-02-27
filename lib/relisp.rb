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

module Relisp
  VERSION = '1.1.1'

  class ElispError < RuntimeError; end
end

$:.unshift File.dirname(__FILE__)

require 'relisp/type_conversion'
require 'relisp/elisp_functions'
require 'relisp/slaves'


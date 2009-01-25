# Emacs is great.  So is Ruby.  This purpose of this library is to:
# * call Ruby from Emacs
# * call Elisp from Ruby
# * manipulate Emacs without using Elisp to some extent (Ruby wrappers
#   around some Elisp functions and objects)
# * reduce the number of blog entries titled "Is Ruby an acceptable
#   lisp?" and flame wars under the title "ruby vs. lisp vs. scheme
#   vs. haskell vs. ..."
#
# See the examples directory to get started.
#
# TODO:
#  implement Buffers, Frames, etc.
#
#  figure out how to install (for emacs and ruby, independently)
#  copyright notices and stuff
#  package it and submit it
#  write a blog entry
#
module Relisp
  class ElispError < RuntimeError; end
end

require 'relisp/slaves'
require 'relisp/programming_types'
require 'relisp/editing_types'


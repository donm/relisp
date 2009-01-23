# Emacs is great.  So is Ruby.  This goal of this library it to:
# * call Ruby from Emacs
# * call Elisp from Ruby
# * manipulate Emacs without using elisp (Ruby wrappers around some
#   Elisp functions and objects)
# * reduce the number of blog entries titled "Is Ruby an acceptable
#   lisp?" and flame wars under the title "Ruby vs. lisp vs. scheme
#   vs. haskell vs. ..."
#
# TODO:
#  more documentation on this page:
#    examples and use cases
#  Buffers, Frames, etc.
#  relisp.rb:
#    documentation
#    defvars
#    let instead of setq
#    catch errors
#    start ruby without warnings?
#  unit tests
#  figure out how to install (for emacs and ruby, independently)
#  finalize method names, class names, etc.
#  copyright stuff
#  package it and submit it
#  write a blog entry
#
module Relisp
  class ElispError < RuntimeError; end
end

require 'relisp/slaves'
require 'relisp/programming_types'
require 'relisp/editing_types'


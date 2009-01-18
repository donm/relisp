#!/usr/bin/env ruby

require 'test/unit'

Dir['**/*test*.rb'].each { |tc| require tc }

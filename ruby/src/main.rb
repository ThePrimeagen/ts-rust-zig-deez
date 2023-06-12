# frozen_string_literal: true

require 'etc'
require_relative '../lib/repl'

def main
  user = Etc.getlogin
  puts "Hello #{user}! This is the Monkey programming language!"
  puts "Feel free to type in commands"

  Repl.new.start
end

main

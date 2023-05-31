require_relative 'src/repl'
require 'etc'

user = Etc.getlogin
puts "Hello #{user}! This is the Monkey programming language!"
puts "Feel free to type in commands"

repl = Repl.new
repl.start
if defined?(PryByebug)
  Pry.commands.alias_command 's', 'step'
  Pry.commands.alias_command 'n', 'next'
  Pry.commands.alias_command 'f', 'finish'
  Pry.commands.alias_command 'c', 'continue'
  Pry.commands.alias_command 'b', 'break'
  Pry.commands.alias_command 'bd', 'break --disable-all'
end

if defined?(PryStackExplorer)
  Pry.commands.alias_command 'st', 'show-stack'
  Pry.commands.alias_command 'u', 'up'
  Pry.commands.alias_command 'd', 'down'
  Pry.commands.alias_command 'f', 'frame'
end

Pry.commands.alias_command 'e', 'exit'

if defined?(Rails)
  Pry.commands.alias_command 'sr', 'show-routes'
end

# Hit Enter to repeat last command
Pry::Commands.command /^$/, "repeat last command" do
  pry_instance.run_command Pry.history.to_a.last
end

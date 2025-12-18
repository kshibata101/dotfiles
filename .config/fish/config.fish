# disable greeting
set fish_greeting

if [ -d $HOME/.env/enabled ]
    set files (find $HOME/.env/enabled -maxdepth 1 -type f -o -type l)
    for file in $files
        source $file
    end
end
if [ -d $HOME/.abbr/enabled ]
    set files (find $HOME/.abbr/enabled -maxdepth 1 -type f -o -type l)
    for file in $files
        source $file
    end
end

if status is-interactive
    # Commands to run in interactive sessions can go here
end

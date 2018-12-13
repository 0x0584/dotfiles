alias grep='grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn} --exclude="*.pyc"'

alias h='fc -li 1000'
# alias rm='rm -i'
alias nano='nano -c'

# suffix aliases http://zshwiki.org/home/examples/aliassuffix
# alias -s log=nano
# alias -s conf=nano

alias ls='ls --color=auto --classify'
alias dir='dir --color=auto'
alias vdir='vdir --color=auto'

alias vimcat="$HOME/.bin/vimcat"
alias vcat="$HOME/.bin/vimcat"
alias vc="$HOME/.bin/vimcat"

alias memclean="sudo bleachbit -c --preset"
alias ggdb="gdb -x gdb.cfg"

alias tem="emacsclient -t"
alias cem="emacsclient -c"

alias ld="ls -d */"
alias retval="echo -e '\nthe return value was:\t$?'"
alias wmget='wget -mcpkEq --show-progress'
alias sysctl='systemctl'
alias mariadb='mysql'
alias valg='valgrind --leak-check=full --show-leak-kinds=all --show-reachable=yes'
alias valk='valkyrie'

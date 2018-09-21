# To enable the settings / commands in this file for login shells as well,
# this file has to be sourced in /etc/profile.

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, overwrite the one in /etc/profile)
# but only if not SUDOing and have SUDO_PS1 set; then assume smart user.
if ! [ -n "${SUDO_USER}" -a -n "${SUDO_PS1}" ]; then
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi

# Commented out, don't overwrite xterm -T "title" -n "icontitle" by default.
# If this is an xterm set the title to user@host:dir
#case "$TERM" in
#xterm*|rxvt*)
#    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD}\007"'
#    ;;
#*)
#    ;;
#esac

# enable bash completion in interactive shells
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
	. /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
	. /etc/bash_completion
    fi
fi

# sudo hint
if [ ! -e "$HOME/.sudo_as_admin_successful" ] && [ ! -e "$HOME/.hushlogin" ] ; then
    case " $(groups) " in *\ admin\ *|*\ sudo\ *)
			      if [ -x /usr/bin/sudo ]; then
				  cat <<-EOF
	To run a command as administrator (user "root"), use "sudo <command>".
	See "man sudo_root" for details.
	
	EOF
			      fi
    esac
fi

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# if the command-not-found package is installed, use it
# if [ -x /usr/lib/command-not-found -o -x /usr/share/command-not-found/command-not-found ]; then
#     function command_not_found_handle {
# 	# check because c-n-f could've been removed in the meantime
#         if [ -x /usr/lib/command-not-found ]; then
# 	    /usr/lib/command-not-found -- "$1"
#             return $?
#         elif [ -x /usr/share/command-not-found/command-not-found ]; then
# 	    /usr/share/command-not-found/command-not-found -- "$1"
#             return $?
# 	else
# 	    printf "%s: command not found\n" "$1" >&2
# 	    return 127
# 	fi
#     }
# fi

# Aliases
alias memclean="sudo bleachbit -c --preset"
alias tem="emacsclient -t"
alias cem="emacsclient -c"
alias ld="ls -d */"
alias retval="echo -e '\nthe return value was:\t$?'"
alias wget-mirror='wget -mcpkEq --show-progress'
alias sysctl='systemctl'
alias mariadb='mysql'
alias valg='valgrind --leak-check=full --show-leak-kinds=all --show-reachable=yes'
alias valk='valkyrie'
EW='~/workspace/'
JTST='~/Workspace/jtest/'
mkcdir () {
    mkdir -p -- "$1" &&
      cd -P -- "$1"
}
export EDITOR='emacsclient -t'
export VISUAL='emacsclient -c'

## ref: https://bash.cyberciti.biz/guide/Changing_bash_prompt
bash_prompt_command() {
    # How many characters of the $PWD should be kept
    local pwdmaxlen=25
    # Indicate that there has been dir truncation
    local trunc_symbol=".."
    local dir=${PWD##*/}
    pwdmaxlen=$(( ( pwdmaxlen < ${#dir} ) ? ${#dir} : pwdmaxlen ))
    NEW_PWD=${PWD/#$HOME/\~}
    local pwdoffset=$(( ${#NEW_PWD} - pwdmaxlen ))
    if [ ${pwdoffset} -gt "0" ]
    then
        NEW_PWD=${NEW_PWD:$pwdoffset:$pwdmaxlen}
        NEW_PWD=${trunc_symbol}/${NEW_PWD#*/}
    fi
    echo ""
}

bash_prompt() {
    case $TERM in
     xterm*|rxvt*)
         local TITLEBAR='\[\033]0;\u:${NEW_PWD}\007\]'
          ;;
     *)
         local TITLEBAR=""
          ;;
    esac
    local NONE="\[\033[0m\]"    # unsets color to term's fg color
    
    # regular colors
    local K="\[\033[0;30m\]"    # black
    local R="\[\033[0;31m\]"    # red
    local G="\[\033[0;32m\]"    # green
    local Y="\[\033[0;33m\]"    # yellow
    local B="\[\033[0;34m\]"    # blue
    local M="\[\033[0;35m\]"    # magenta
    local C="\[\033[0;36m\]"    # cyan
    local W="\[\033[0;37m\]"    # white
    
    # emphasized (bolded) colors
    local EMK="\[\033[1;30m\]"
    local EMR="\[\033[1;31m\]"
    local EMG="\[\033[1;32m\]"
    local EMY="\[\033[1;33m\]"
    local EMB="\[\033[1;34m\]"
    local EMM="\[\033[1;35m\]"
    local EMC="\[\033[1;36m\]"
    local EMW="\[\033[1;37m\]"
    
    # background colors
    local BGK="\[\033[40m\]"
    local BGR="\[\033[41m\]"
    local BGG="\[\033[42m\]"
    local BGY="\[\033[43m\]"
    local BGB="\[\033[44m\]"
    local BGM="\[\033[45m\]"
    local BGC="\[\033[46m\]"
    local BGW="\[\033[47m\]"
    
    local UC=$W                 # user's color
    local PROMP="%"

    [ $UID -eq "0" ] && UC=$R   # root's color
    [ $UID -eq "0" ] && PROMP="#"

    PS1="${TITLEBAR}${BGR}\T${UC} ${EMG}:: ${EMK}${UC}\u${EMK}@${UC}\h ${EMG}::${NONE} (\j \#)\n[ ${EMB}\${NEW_PWD} ${UC}]${UC} ${PROMP}${NONE} "
    # without colors: PS1="[\u@\h \${NEW_PWD}]\\$ "
    # extra backslash in front of \$ to make bash colorize the prompt
}

## init it by setting PROMPT_COMMAND
PROMPT_COMMAND=bash_prompt_command
bash_prompt
alias ag='apt-get install -y'
alias vimcat="$HOME/.bin/vimcat"
alias vcat="$HOME/.bin/vimcat"
alias vc="vcat"

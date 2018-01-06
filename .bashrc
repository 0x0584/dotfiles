## .bashrc
#
## Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi
#
RED='\033[0;31m'
ORANGE='\033[0;33m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
VIOLET='\033[0;35m'
NC='\033[0m' # No Color
#
echo -e "                                       S&"
echo -e "                                      @RSk"
echo -e "                                     S!!!M&"
echo -e "                                    @?~~~!Sk"
echo -e "                                   9!!~ ~~!MX"
echo -e "                                  @X~~    ~!Sk"
echo -e "                                 9!!      ~~!SX"
echo -e "                                dR!~        ~!S>"
echo -e "                               XR!~          ~!Sk:"
echo -e "                              tR!~        ::~~!!${RED}MMXXHHHH!!<:.${NC}"
echo -e "                             <S!xxiXX!!!~~~~~~~!!${RED}MMMMMMMMMMMMMMMXXXXx::${NC}"
echo -e "                        .:X@NSSSRMMX!!!!~~~~~~~~!!${ORANGE}MMMM@@MMMMMMMMMMMMXMSMMtHHHX!${NC}"
echo -e "                  :xiM# ~  <S!~   ~~~!~~~~~~~~~~~!!${ORANGE}MX!!!!!??#RR888MMMMMMMMMMMHH${NC}"
echo -e "           ..XH@!~        XS!~                  ~~~!${GREEN}MX!!!!!!!!!!!!?MMR@@SMMMMMM${NC}"
echo -e "     :xiM# ~             <S!~                      ~!${GREEN}MM??MMX!!!!!!!!!!!!!!??#RS${NC}"
echo -e "XH@M!                   :S!!                        ~!R:   ${BLUE} ~!MM!XH!!!!!!!!!!!!${NC}"
echo -e "                       <S!~                          !!M:        ${BLUE} ~ ??HHX!!!!!!${NC}"
echo -e "                      :S!~                            !!R:              ${VIOLET} !!MMMH${NC}"
echo -e "                      S!~                              ~!8:                  ${VIOLET} ~${NC}"
echo -e "                    :S!!        I'll see you on        ~!!N:"
echo -e "                    S!~          the dark side           !!N:"
echo -e "                  .S!!~             of  the              ~~!&>"
echo -e "                  @!!~               modem                 ~!MN"
echo -e "                .S!!~                                     ~~!M&>"
echo -e "               :@SMHHHHHHHHH!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!H>$N"

## User specific aliases and functions
PATH="$PATH:$HOME/bin/usr/bin"
export RSENSE_HOME=/opt/rsense-0.3/
export PATH=${PATH}:~/android-sdks/tools
export PATH=${PATH}:~/android-sdks/platform-tools
XTERM=xterm-256color

# Aliases
alias memclean="su -c 'bleachbit -c --preset' && exit"
alias tem="emacsclient -t"
alias cem="emacsclient -c"
alias ld="ls -d */"
alias retval="echo -e '\nthe return value was:\t$?'"
alias wget-mirror='wget -mcpkEq --show-progress'
alias sysctl='systemctl'
alias mariadb='mysql'
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
unset bash_prompt
# source ~/.bash-powerline.sh

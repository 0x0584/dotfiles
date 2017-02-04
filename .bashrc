# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

RED='\033[0;31m'
ORANGE='\033[0;33m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
VIOLET='\033[0;35m'
NC='\033[0m' # No Color
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
echo -e "                  :xiM# ~  <S!~   ~~~!~~~~~~~~~~~!!${YELLOW}MX!!!!!??#RR888MMMMMMMMMMMHH${NC}"
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
echo ""
export PATH=$PATH:/opt/gitkraken

# User specific aliases and functions

#PS1="(\j) \@ % "

if [ $(id -u) -eq 0 ]; 
then
	$SHELL="/usr/bin/bash"
fi	

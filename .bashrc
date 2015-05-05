# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

alias ls='/usr/local/bin/ls'
alias ll='ls -alh'
alias ncview='ncview -no1d'

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=5000
HISTFILESIZE=5000

# vapor setup
#. /usr/local/vapor-2.3.0/bin/vapor-setup.sh

export GPGKEY=1DEDB5C3
export PYTHONPATH=
export TERM=xterm
export CDPATH=.:~:~/ROMS/runs/:~/ROMS/runs/eddyshelf/:~/ROMS/runs/eddyshelf/scripts/:~/ROMS/runs/eddyshelf/topoeddy/:~/ROMS/
export PATH=/home/deepak/:/usr/local/texlive/2014/bin/x86_64-linux/:$PATH
export MANPATH=/usr/local/texlive/2014/texmf-dist/doc/man:$MANPATH
export INFOPATH=/usr/local/texlive/2014/texmf-dist/doc/man:$INFOPATH
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH # vapor-setup mucks up netcdf and hdf library locations
export PIPELIGHT_GPUACCELERATION=2

# pretty ls colors?
eval `dircolors ~/.dir_colors`

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

if [ -f ~/.bash_common ]; then
    . ~/.bash_common
fi

if [ -f /etc/infinality-settings.sh ]; then
    . /etc/infinality-settings.sh
fi

# Reset
Color_Off="\[\033[0m\]"       # Text Reset

# Regular Colors
Black="\[\033[0;30m\]"        # Black
Red="\[\033[0;31m\]"          # Red
Green="\[\033[0;32m\]"        # Green
Yellow="\[\033[0;33m\]"       # Yellow
Blue="\[\033[0;34m\]"         # Blue
Purple="\[\033[0;35m\]"       # Purple
Cyan="\[\033[0;36m\]"         # Cyan
White="\[\033[0;37m\]"        # White

# Bold
BBlack="\[\033[1;30m\]"       # Black
BRed="\[\033[1;31m\]"         # Red
BGreen="\[\033[1;32m\]"       # Green
BYellow="\[\033[1;33m\]"      # Yellow
BBlue="\[\033[1;34m\]"        # Blue
BPurple="\[\033[1;35m\]"      # Purple
BCyan="\[\033[1;36m\]"        # Cyan
BWhite="\[\033[1;37m\]"       # White

# Underline
UBlack="\[\033[4;30m\]"       # Black
URed="\[\033[4;31m\]"         # Red
UGreen="\[\033[4;32m\]"       # Green
UYellow="\[\033[4;33m\]"      # Yellow
UBlue="\[\033[4;34m\]"        # Blue
UPurple="\[\033[4;35m\]"      # Purple
UCyan="\[\033[4;36m\]"        # Cyan
UWhite="\[\033[4;37m\]"       # White

# Background
On_Black="\[\033[40m\]"       # Black
On_Red="\[\033[41m\]"         # Red
On_Green="\[\033[42m\]"       # Green
On_Yellow="\[\033[43m\]"      # Yellow
On_Blue="\[\033[44m\]"        # Blue
On_Purple="\[\033[45m\]"      # Purple
On_Cyan="\[\033[46m\]"        # Cyan
On_White="\[\033[47m\]"       # White

# High Intensty
IBlack="\[\033[0;90m\]"       # Black
IRed="\[\033[0;91m\]"         # Red
IGreen="\[\033[0;92m\]"       # Green
IYellow="\[\033[0;93m\]"      # Yellow
IBlue="\[\033[0;94m\]"        # Blue
IPurple="\[\033[0;95m\]"      # Purple
ICyan="\[\033[0;96m\]"        # Cyan
IWhite="\[\033[0;97m\]"       # White

# Bold High Intensty
BIBlack="\[\033[1;90m\]"      # Black
BIRed="\[\033[1;91m\]"        # Red
BIGreen="\[\033[1;92m\]"      # Green
BIYellow="\[\033[1;93m\]"     # Yellow
BIBlue="\[\033[1;94m\]"       # Blue
BIPurple="\[\033[1;95m\]"     # Purple
BICyan="\[\033[1;96m\]"       # Cyan
BIWhite="\[\033[1;97m\]"      # White

# High Intensty backgrounds
On_IBlack="\[\033[0;100m\]"   # Black
On_IRed="\[\033[0;101m\]"     # Red
On_IGreen="\[\033[0;102m\]"   # Green
On_IYellow="\[\033[0;103m\]"  # Yellow
On_IBlue="\[\033[0;104m\]"    # Blue
On_IPurple="\[\033[10;95m\]"  # Purple
On_ICyan="\[\033[0;106m\]"    # Cyan
On_IWhite="\[\033[0;107m\]"   # White

# Various variables you might want for your PS1 prompt instead
Time12h="\T"
Time12a="\@"
Time24h="\t"
PathShort="\w"
PathFull="\W"
NewLine="\n"
Jobs="\j"
User="\u"
Host="\h"

# export TERM=xterm-256color

# bash prompt colors
#PS1='\[\e[0;32m\]\u\[\e[m\]@\h: \[\e[0;34m\]\w\[\e[m\] \[\e[0;32m\]$(__git_ps1) \$\[\e[m\] '
export PS1="┌──"$White$Time24h$Color_Off\ $Green$User$Color_Off@$Host'$(git branch &>/dev/null;\
if [ $? -eq 0 ]; then \
  echo "$(echo `git status` | egrep "nothing (added )?to commit" > /dev/null 2>&1; \
  if [ "$?" -eq "0" ]; then \
    # @4 - Clean repository - nothing to commit
    echo "'$Green'"$(__git_ps1 " (%s)"); \
  else \
    # @5 - Changes to working tree
    echo "'$Red'"$(__git_ps1 " {%s}"); \
  fi) '$Blue$PathShort$Green$Color_Off'\$ "; \
else \
  # @2 - Prompt when not in GIT repo
  echo " '$Blue$PathShort$Green$Color_Off'\$ "; \
fi)'"\n└──────> "

# make screen update DISPLAY to required value?
VARS_TO_UPDATE="DISPLAY DBUS_SESSION_BUS_ADDRESS SESSION_MANAGER GPG_AGENT_INFO"
screen_pushenv () {
  screen-sendenv.py -t screen $VARS_TO_UPDATE
}
tmux_pushenv () {
  screen-sendenv.py -t tmux $VARS_TO_UPDATE
}
screen_pullenv () {
  tempfile=$(mktemp -q) && {
    for var in $VARS_TO_UPDATE; do
      screen sh -c "echo export $var=\$$var >> \"$tempfile\""
    done
    . "$tempfile"
    rm -f "$tempfile"
  }
}
tmux_pullenv () {
  for var in $VARS_TO_UPDATE; do
    expr="$(tmux showenv | grep "^$var=")"
    if [ -n "$expr" ]; then
      export "$expr"
    fi
  done
}

# added by Anaconda 2.1.0 installer
#export PATH="/media/data/anaconda/bin:$PATH"

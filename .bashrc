alias emacs='emacsclient -t -nw'
alias poison='ssh -X poison.whoi.edu'
alias aces='ssh -X dcherian@login.acesgrid.org'
alias scylla='ssh scylla'
alias mohawk='ssh mohawk'

alias ll='ls -alh'
alias ncview='ncview -no1d'
alias ls='ls -h --color'

alias ipython='/usr/local/bin/ipython'

if [ $UID -ne 0 ]; then
    alias reboot='sudo reboot'
    alias apt-get='sudo apt-get'
    alias tlmgr='sudo /usr/local/texlive/2013/bin/x86_64-linux//tlmgr'
fi

export GPGKEY=2EAEF83B

# vapor setup
#. /usr/local/vapor-2.3.0/bin/vapor-setup.sh

# export TERM=xterm-256color
export CDPATH=.:~:/media/data/Work/:/media/data/Work/eddyshelf/:/media/data/Work/eddyshelf/runs/:/media/data/Work/ROMS/trunk/ROMS/
export PATH=/home/deepak/:/usr/local/texlive/2013/bin/x86_64-linux/:$PATH
export MANPATH=/usr/local/texlive/2013/texmf-dist/doc/man:$MANPATH
export INFOPATH=/usr/local/texlive/2013/texmf-dist/doc/man:$INFOPATH
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH # vapor-setup mucks up netcdf and hdf library locations
export PIPELIGHT_GPUACCELERATION=2

# pretty ls colors?
eval `dircolors ~/.dir_colors`

# bash prompt colors
PS1='\[\e[0;32m\]\u\[\e[m\]@\h: \[\e[0;34m\]\w\[\e[m\] \[\e[0;32m\]\$\[\e[m\] '

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

alias emacs='emacs -nw --no-desktop'
#export ALTERNATE_EDITOR='/usr/bin/emacs --daemon'
#export EDITOR='emacsclient -t'
#export VISUAL='emacsclient -t'

alias aces='ssh aces'
alias scylla='ssh scylla'
alias mohawk='ssh mohawk'

# matlab setup
alias matlab='matlab -nosplash -nodesktop'
export _JAVA_OPTIONS="-Dsun.java2d.pmoffscreen=false"

alias ll='ls -alh'
alias ncview='ncview -no1d'
alias ls='ls -h --color'

eval `dircolors ~/.dir_colors`

alias ipython='/usr/local/bin/ipython'

if [ $UID -ne 0 ]; then
    alias reboot='sudo reboot'
    alias apt-get='sudo apt-get'
    alias tlmgr='sudo /usr/local/texlive/2013/bin/x86_64-linux//tlmgr'
fi

export TERM=xterm-256color
export CDPATH=.:~:~/tools/:~/ROMS/runs/eddyshelf/:~/ROMS/:~/ROMS/runs//eddyshelf/topoeddy/:~/ROMS/trunk/ROMS/
export PATH=/home/deepak/:/usr/local/texlive/2013/bin/x86_64-linux/:$PATH
export MANPATH=/usr/local/texlive/2013/texmf-dist/doc/man:$MANPATH
export INFOPATH=/usr/local/texlive/2013/texmf-dist/doc/man:$INFOPATH
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH # vapor-setup mucks up netcdf and hdf library locations

# make screen update DISPLAY to required value?
VARS_TO_UPDATE="DISPLAY DBUS_SESSION_BUS_ADDRESS SESSION_MANAGER GPG_AGENT_INFO"
screen_pushenv () {
  ~/screen-sendenv.py -t screen $VARS_TO_UPDATE
}
tmux_pushenv () {
  ~/screen-sendenv.py -t tmux $VARS_TO_UPDATE
}

screen_pullenv () {
    tempfile = $(mktemp -q) && {
        for vars in $VARS_TO_UPDATE; do
            screen sh -c "echo export $var=\$$var >> \"$tempfile\""
        done
        . "$tempfile"
        rm -f "$tempfile"
    }
}

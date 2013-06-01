export TUXDIR=tux_TUXDIR
export CARMSYS=/opt/Carmen/rave_release_13.1.0_CARMSYS
export APPDIR=tux_APPDIR
export LANG=C
export CARMUSR=$APPDIR/ba_rave
alias sbsunloadcf=tmunloadcf

ulimit -c unlimited

# Add tuxedo to PATH and LIBPATH

# add tuxedo to PATH
PATH="$PATH:$TUXDIR/bin"

# add Maestro rave scripts (maestro_get_preferences and compile-rave)
# to the PATH
PATH="$PATH:$APPDIR/rls/rave"

# add tuxedo and rave libraries to lib path
if [ -z "$LD_LIBRARY_PATH" ] ; then
  LD_LIBRARY_PATH="$TUXDIR/lib:$CARMSYS/lib/i386_linux"
else
  LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$TUXDIR/lib:$CARMSYS/lib/i386_linux"
fi

# Set environment variables based on APPDIR

export TUXCONFIG="$APPDIR/conf/tuxconfig"
export QMCONFIG="$APPDIR/conf/QUE"
export BDMCONFIG="$APPDIR/conf/bdmconfig"

export GVWHEREEX="$APPDIR/exports"
export GVIMPORTDIR="$APPDIR/imports"
export GVWHEREIM="$APPDIR/imports"
export GVEXPORTDIR="$APPDIR/exports"
export GVWHERE08="$APPDIR/flifodir"

find_maestro_rls_dir() {
  local config
  config=`tmunloadcf |grep ENVF`
  if [ -z "$config" ] ; then
    echo No envfile found 1>&2
    return 1
  fi
  config=`echo $config|perl -pe 's/.*ENVFILE=\"(.+)".*/\1/'`
  config=${config##*=}
  if [ -z "$config" ] ; then
    echo No envfile found 1>&2
    return 1
  fi
  if [ ! -r $config ] ; then
    echo Cannot find $config 1>&2
    return 2
  fi
  config=`cat $config |grep '^PATH='`
  if [ -z "$config" ] ; then
    echo Cannot find path in envfile 1>&2
    return 3
  fi
  config=${config##*=}
  if [ -z "$config" ] ; then
    echo No path found 1>&2
    return 4
  fi
  maestro_rls_dir=$config
}

# function to quickly CD to the appropriate directory
goto () {
  case $1 in
   appdir) cd "$APPDIR" ;;
   ulog) cd "$APPDIR/ulogdir" ;;
   ex) cd "$GVEXPORTDIR" ;;
   im) cd "$GVIMPORTDIR" ;;
   flifo) cd "$GVWHERE08" ;;
   rls) 
        find_maestro_rls_dir
        if [ -n "$maestro_rls_dir" ] ; then
          cd "$maestro_rls_dir"
        fi
        ;;
   *) echo "Unkwown goto keyword $1" ;;
  esac
}

# function to shutdown and boot a server #
# (after recompile or config change etc)
bounce () {
  if [ -z "$1" ] ; then
    echo "Usage: bounce <server>"
  else
    tmshutdown -w3 -s $1
    tmboot -s $1
  fi
}

# function to show servers which are currently executing requests
showpsr () {
  while /bin/true ; do
    echo psr | tmadmin | grep -v IDLE
    sleep 1 
  done
}

# tail -f the latest ULOG file
uuu () {
  tail -f $APPDIR/ulogdir/`ls -t $APPDIR/ulogdir|head -1`
}
. $APPDIR/rls/viewfiles


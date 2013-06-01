#
# Collection of shell utility functions that deal with processes
#


# Check if process pid is running (linux only)
function process_pid_running () {
    if [ -z "$1" ] ; then
        echo "Usage: process_pid_running <pid>"
        return 1
    fi
    [ -d /proc/$1 ]
}

#
# Get a list of pids running matching a string
# arg1 - string
# arg2 - optional user name
declare -a pids
function process_pids_matching () {
    unset pids
    if [ -z "$1" ] ; then
        echo "Usage: process_pids_matching <match> [<username>]"
        return 1
    fi
    local cmd="ps -ewwo pid,args" pid program first
    if [ ! -z "$2" ] ; then
        cmd="$cmd -u $2"
    fi
    first=1
    $cmd | while read pid program ; do
        if [ $first = 0 ] ; then
            #echo Here program = $program 1>&2
            case $program in
                $1) echo $pid ;;
            esac
        fi
        first=0
    done > /tmp/.tmp$$
    pids=(`cat /tmp/.tmp$$`)
}


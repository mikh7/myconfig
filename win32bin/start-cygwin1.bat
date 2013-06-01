if %SESSIONNAME% EQU "RDP-Tcp#1" goto display1

set DISPLAY=127.0.0.1:1.0

goto run

:display1

set DISPLAY=127.0.0.1:1.0

:run

run XWin -multiwindow -clipboard -silent-dup-error -nounixkill :1
run /bin/zsh ~/myconfig/X11/cygwin-xinitrc








set DISPLAY=127.0.0.1:0.0
cd %USERPROFILE%
run XWin -multiwindow -clipboard -silent-dup-error -nounixkill -dpi 99
run /bin/zsh ~/myconfig/X11/cygwin-xinitrc 
echo done







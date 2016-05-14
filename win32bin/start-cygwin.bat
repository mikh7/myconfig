PATH=%PATH%;c:\cygwin64\bin
set DISPLAY=127.0.0.1:0.0
cd %USERPROFILE%
run XWin -multiwindow -clipboard -silent-dup-error -nounixkill -emulate3buttons
run /bin/zsh ~/myconfig/X11/cygwin-xinitrc 
echo done







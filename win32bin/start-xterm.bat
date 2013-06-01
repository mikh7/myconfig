@echo on
if %SESSIONNAME% EQU RDP-Tcp#1 goto display1
if %SESSIONNAME% EQU RDP-Tcp#2 goto display1

set DISPLAY=:0.0

goto run

:display1

set DISPLAY=:0.1

:run

run C:\cygwin\bin\run.exe /bin/zsh -c "exec xterm -ls"

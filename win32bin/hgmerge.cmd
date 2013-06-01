@echo on
setlocal
:: Look in the registry for KDiff3 location
set KDiff3Path=C:\Program Files\KDiff3
:kdiff3
"%KDiff3Path%\kdiff3.exe" --auto --L1 Base --L2 Local --L3 Other %2 %1 %3 -o %1
if not errorlevel 0 (exit /b 1) else (exit /b 0)

:notfound
echo hgmerge: cannot find KDiff3 location in the registry.
exit /b 1
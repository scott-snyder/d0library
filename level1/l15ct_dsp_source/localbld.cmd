ECHO OFF
REM    This is an OS/2 Command File which builds the Local DSP Program boot
REM    loader readable image.
REM    It takes one parameter which should be either  a member of the set
REM          {A1, A2, A3, A4, B1, B3, B4, C1, C2, C3, C4}   
REM    or the keyword
REM          all
REM    If the parameter is "all" then it builds all Local DSP programs.
REM    Otherwise it builds only the specified Local DSP program.

REM Determine which modules to assemble
IF  %1 == all goto BLDALL

:BLDONE
ECHO ON
REM Build only one Local DSP program
hex30   local_%1.x40    -o local_%1.blx -map local_%1.mxp    localcmn.bld
GOTO   END

:BLDALL
ECHO ON
REM  Build all Local DSP programs
FOR %%f IN (A1 A2 A3 A4 B1 B3 B4 C1 C2 C3 C4) DO hex30 local_%%f.x40 -o local_%%f.blx -map local_%%f.mxp localcmn.bld

:END

ECHO OFF
REM    This is an OS/2 Command File which links the Local DSP Program
REM    It takes one parameter which should be either  a member of the set
REM          {A1, A2, A3, A4, B1, B3, B4, C1, C2, C3, C4}   
REM    or the keyword
REM          all
REM    If the parameter is "all" then it links all Local DSP programs.
REM    Otherwise it links only the specified Local DSP program

REM Determine which modules to assemble
IF  %1 == all goto LNKALL

:LNKONE
ECHO ON
REM Link only one Local DSP program
Lnk30 -m Local_%1.Map  Local_%1.Lnk    LocalCmn.Lnk 
GOTO   END

:LNKALL
ECHO ON
REM  Link all Local DSP programs
FOR %%f IN (A1 A2 A3 A4 B1 B3 B4 C1 C2 C3 C4) DO Lnk30 -m Local_%%f.Map Local_%%f.Lnk LocalCmn.Lnk 

:END

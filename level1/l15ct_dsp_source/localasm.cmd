ECHO OFF
REM    This is an OS/2 Command File which assembles the Local DSP Program
REM    It takes one parameter which should be either  a member of the set
REM          {A1, A2, A3, A4, B1, B3, B4, C1, C2, C3, C4}   
REM    or the keyword
REM          all
REM    If the parameter is "all" then it assembles all Local DSP modules
REM    (both node-specific and common across all nodes).  Otherwise
REM    it assembles only the specified node-specific module

REM Determine which modules to assemble
IF  %1 == all goto ASMALL

:ASMONE
ECHO ON
REM Assemble only one node-specific module
Asm30   L_DSP_%1.A40     L_DSP_%1.o40     -v40 -l -q -s -DVC40II=1
GOTO   END

:ASMALL
ECHO ON
REM  Assemble all modules
Asm30  L_Init.A40       L_Init.o40      -v40    -l   -q   -s   -DVC40II=1
Asm30  L_Params.A40     L_Params.o40    -v40    -l   -q   -s   -DVC40II=1
Asm30  L_Tl_Ini.A40     L_Tl_Ini.o40    -v40    -l   -q   -s   -DVC40II=1
Asm30  L_Data.A40       L_Data.o40      -v40    -l   -q   -s   -DVC40II=1
Asm30  L_Scan.A40       L_Scan.o40      -v40    -l   -q   -s   -DVC40II=1
Asm30  L_Tl_Np2.A40     L_Tl_Np2.o40    -v40    -l   -q   -s   -DVC40II=1
Asm30  L_Tl_Np3.A40     L_Tl_Np3.o40    -v40    -l   -q   -s   -DVC40II=1
Asm30  L_Tl_Np4.A40     L_Tl_Np4.o40    -v40    -l   -q   -s   -DVC40II=1
Asm30  L_Tl_Np5.A40     L_Tl_Np5.o40    -v40    -l   -q   -s   -DVC40II=1
Asm30  L_ISR.A40        L_ISR.o40       -v40    -l   -q   -s   -DVC40II=1

FOR %%f IN (A1 A2 A3 A4 B1 B3 B4 C1 C2 C3 C4) DO Asm30 L_DSP_%%f.A40 L_DSP_%%f.o40 -v40 -l -q -s -DVC40II=1

:END

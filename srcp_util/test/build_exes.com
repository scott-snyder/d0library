$!========================================================================
$!
$! Name      : BUILD_EXES
$!
$! Purpose   : Create some RCP exes
$!
$! Arguments :
$!
$! Created  27-SEP-1990   Harrison B. Prosper
$! Modified 23-APR-1991   Harrison B. Prosper 
$!      Add RCPCHECK
$! Modified 27-JUN-1991   Harrison B. Prosper 
$!      Add EZ_DISPATCH_BUILDER
$! Modified 12-MAY-1992   Harrison B. Prosper 
$!      Use UTIL4 throughout
$! Modified  8-JUL-1992   Harrison B. Prosper 
$!      Add CERNP symbol
$! Modified  9-JUN-1993   Alan M. Jonckheere 
$!      Remove UTIL4.OPT, need only GENERAL.OLB
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$
$   SET VERIFY
$   cernp := D0$CERNLIB:PACKLIB/LIB,MATHLIB/LIB,KERNLIB/LIB, -
        sys$library:vaxcrtl/lib
$   
$   LINK/EXE=D0$SRCP_UTIL:RCP_TO_FZ.EXE  -
        D0$GENERAL:GENERAL/INCLUDE=(RCP_TO_FZ), -
        D0$GENERAL:GENERAL/LIB, -
        'CERNP'
$
$   LINK/EXE=D0$SRCP_UTIL:RCPSIZE.EXE  -
        D0$GENERAL:GENERAL/INCLUDE=(EZFSIZ), -
        D0$UTIL:UTIL/LIB,-
        D0$GENERAL:GENERAL/LIB, -
        'CERNP'
$
$   LINK/EXE=D0$SRCP_UTIL:RCPLIST.EXE  -
        D0$GENERAL:GENERAL/INCLUDE=(EZFLIS), -
        D0$GENERAL:GENERAL/LIB, -
        'CERNP'
$
$   LINK/EXE=D0$SRCP_UTIL:RCPTEST.EXE  -
        D0$GENERAL:GENERAL/INCLUDE=(TESTRCP), -
        D0$GENERAL:GENERAL/LIB, -
        'CERNP'
$
$   LINK/EXE=D0$SRCP_UTIL:RCPCHECK.EXE  -
        D0$GENERAL:GENERAL/INCLUDE=(RCPCHECK), -
        D0$GENERAL:GENERAL/LIB, -
        'CERNP'
$
$   LINK/EXE=D0$SRCP_UTIL:EZ_DISPATCH_BUILDER.EXE  -
        D0$GENERAL:GENERAL/INCLUDE=(EZ_DISPATCH_BUILDER), -
        D0$GENERAL:GENERAL/LIB, -
        'CERNP'
$   
$EXIT:
$   SET NOVERIFY
$   EXIT

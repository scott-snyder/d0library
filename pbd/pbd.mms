!------------------------------------------------------------
!
! Name      : PBD.MMS
!
! Purpose   : Update target: PBD.TSP
!
! Created  17-JUL-97   21:01:58  USERLIB V6.00 18-FEB-1991
!------------------------------------------------------------
!

 
.FIRST
  @ D0$WARNING == 0
  @ D0$ERROR == 0
  @ D0$FATAL == 0
  @ ON WARNING THEN GOSUB ERROR_COUNT
 
.LAST
  @ EXIT
  @ ERROR_COUNT:
  @ IF $SEVERITY .EQ. 0 THEN D0$WARNING == D0$WARNING + 1
  @ IF $SEVERITY .EQ. 2 THEN D0$ERROR == D0$ERROR + 1
  @ IF $SEVERITY .EQ. 4 THEN D0$FATAL == D0$FATAL + 1
  @ ON WARNING THEN GOSUB ERROR_COUNT
  @ RETURN
  
.IFDEF DO_PRE
NOTHING.NL : SYS$LOGIN:LOGIN.COM
  @ WRITE SYS$OUTPUT "Executing Pre-processing Commands"
.ENDIF
  
  
 
.IFDEF DO_INTER
NOTHING.NL : SYS$LOGIN:LOGIN.COM
  @ WRITE SYS$OUTPUT "Executing Intermediate-processing Commands"
.ENDIF
  
  
 
.IFDEF DO_POST
NOTHING.NL : SYS$LOGIN:LOGIN.COM
  @ WRITE SYS$OUTPUT "Executing Post-processing Commands"
.ENDIF
 
 
.IFDEF DO_OFFICIAL
NOTHING.NL : SYS$LOGIN:LOGIN.COM
  @ WRITE SYS$OUTPUT "Executing Officializing Commands"
.ENDIF
 
.IFDEF D0LIB_RELEASE
 
NOTHING.NL : SYS$LOGIN:LOGIN.COM
  @ WRITE SYS$OUTPUT "%RELEASE-I-No main processing to be executed
 
.ENDIF

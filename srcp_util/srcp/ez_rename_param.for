      SUBROUTINE EZ_RENAME_PARAM(OLD_NAME,NEW_NAME,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Assign a new name to an existing RCP 
C-   parameter.
C-
C-   Inputs  : OLD_NAME [C*]    Old parameter name
C-   Outputs : NEW_NAME [C*]    New parameter name
C-             IER      [I]     0 - OK
C-   Controls: 
C-
C-   Created  31-MAY-1991   LUPE HOWELL, Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) OLD_NAME, NEW_NAME
      INTEGER IER
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      CHARACTER*32 STRING
      INTEGER IPTI,IPTO,IPTV,IPTT,ID,IPNT,WRDIDS
C----------------------------------------------------------------------
C
C ****  Get pointer to OLD_NAME
C
      CALL EZGETI(OLD_NAME(1:LEN(OLD_NAME)),ID,IER)
      IF ( IER .NE. 0 ) GOTO 999
C
C ****  Get absolute pointer into RCP-bank
C
      LSRCP = KSRCP(ISRCP)
      CALL EZZGPT (LSRCP,IPTI,IPTO,IPTV,IPTT)
      WRDIDS = IC(LSRCP+JJNWRD)        ! Number of words/record
C
C ****  Put in new name
C
      IPNT = IPTI + WRDIDS*(ID-1)
      STRING = NEW_NAME(1:LEN(NEW_NAME))
      CALL DCTOH(NUMCHR,STRING,IC(IPNT+1))
C
C ****  Resort RCP parameter names
C
      CALL EZEND
  999 RETURN
      END

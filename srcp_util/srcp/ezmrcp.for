      SUBROUTINE EZMRCP(LBANK,IER) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Copy an RCP bank with link LBANK from the run 
C-                         division in ZEBCOM to SRCP.
C-
C-   Inputs  : LBANK : Supporting link of bank to be copied to STP
C-   Outputs : IER   : 0=ok, -1=not successfull
C-
C-   Created  20-JUN-1990   Marcel Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER  LBANK,LSRCP,IER
      CHARACTER*45 MSG
C----------------------------------------------------------------------
      IF(LBANK.LE.0) GOTO 999
C
      CALL MZCOPY (IXDVR,LBANK,IDVSTP,LSRCP,1,' ')
      CALL EZNAME(' ',LSRCP,0)
      IF(LSRCP.LE.0) GOTO 999
      RETURN
C
  999 WRITE(MSG,10) LBANK
   10 FORMAT(1X,' Bank with link',I9,' not copied to SRCP')
      CALL ERRMSG('SRCP','EZMSRCP',MSG,'W')
C
      RETURN
      END

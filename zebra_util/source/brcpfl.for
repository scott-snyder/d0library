      SUBROUTINE BRCPFL(RCP_BANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : COPY RCP_BANK TO BEGIN RUN DIVISION
C-
C-   Inputs  : RCP_BANK - RCP BANK TO COPY
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created  28-JUN-1990   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER LRCP, LBRCP, ND
      CHARACTER*(*) RCP_BANK
      CHARACTER*40 MSG
C----------------------------------------------------------------------
C ****  GET RCP BANK ADDRESS & BANK SIZE
C
      CALL EZLOC(RCP_BANK,LRCP)
      IF( LRCP.LE.0) THEN
        WRITE(MSG,1001)RCP_BANK
        CALL ERRMSG('BRCPFL','RCP',MSG,'W')
        GOTO 999 
      END IF
      ND = IC(LRCP-1)
C
C ****  BOOK BRCP BANK
C
      CALL BKBRCP(ND,LBRCP) 
C
C ****  FILL BRCP BANK WITH RCP DATA
C
      CALL UCOPY (IC(LRCP+1), IQ(LBRCP+1), ND)
C
C----------------------------------------------------------------------
  999 RETURN
 1001 FORMAT(' BANK ',A16,' NOT IN STP ')
      END

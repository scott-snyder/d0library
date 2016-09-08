      SUBROUTINE BKCSRV(JCSRV)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : BOOK 'CSRV' BANK
C-
C-   Inputs  :
C-   Outputs :        JCSRV        pointer to CSRV bank
C-   Controls:
C-
C-   Created   4-MAR-1991   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CALOR_SURVEY.INC'
      INTEGER JCSRV, MCSRV(5)
C
      DATA MCSRV / 4HCSRV, 1, 1, 10, 2/
C
      IF( JCSRV .EQ.0 ) THEN
        CALL MZLIFT(IDVSURV, JCSRV, LSURV, 1, MCSRV,0)
      ELSE
        CALL MZLIFT(IDVSURV, JCSRV, JCSRV, 0, MCSRV,0)
      END IF
C----------------------------------------------------------------------
  999 RETURN
      END

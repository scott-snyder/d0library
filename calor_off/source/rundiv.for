      SUBROUTINE RUNDIV (BANK,RUNDV)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get run division number from specified SRCP
C-                         bank.
C-
C-   Inputs  : BANK        Name of SRCP bank
C-
C-   Outputs : RUNDV       Run division
C-
C-   Controls: None
C-
C-   Created   7-MAR-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) BANK
      INTEGER RUNDV,L
C----------------------------------------------------------------------
      L = LEN (BANK)
      CALL SLSRCP (BANK(1:L))
      CALL GTSRCP ('ZEBRA/RUN_DIVISION',RUNDV,1)
      CALL RSSRCP
  999 RETURN
      END

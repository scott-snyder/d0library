      SUBROUTINE BKCMDL(JCMDL, NMSR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : BOOK 'CMDL' BANK.
C-
C-   Inputs  :
C-   Outputs :         JCMDL      pointer to CMDL bank
C-   Controls:
C-
C-   Created   4-MAR-1991   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CALOR_SURVEY.INC'
      INCLUDE 'D0$LINKS:IZCMDL.LINK'
      INTEGER JCMDL, MCMDL(5), IOCMDL, NMSR
C
      DATA MCMDL / 'CMDL', 2, 0, 39, 9 /
C
      CALL MZFORM('CMDL','1H7I-F',IOCMDL)
      MCMDL(5) = IOCMDL
      IF( NMSR .NE. 0) THEN
        MCMDL(4) = NMSR*NMWDS + ISNILX-1
      END IF
      CALL MZLIFT(IDVSURV, JCMDL, LCSYL, -IZCMDL, MCMDL, 0)
C----------------------------------------------------------------------
  999 RETURN
      END

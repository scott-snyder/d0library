      SUBROUTINE BKCTHE(JCTHE, NMSR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : BOOK 'CTHE' BANK.
C-
C-   Inputs  :         NMSR       number of mearsurements to be stored
C-   Outputs :         JCTHE      pointer to CTHE bank
C-   Controls:
C-
C-   Created   4-MAR-1991   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CALOR_SURVEY.INC'
      INCLUDE 'D0$LINKS:IZCTHE.LINK'
      INTEGER JCTHE, MCTHE(5), IOCTHE, NMSR
C
      DATA MCTHE / 'CTHE', 2, 0, 39, 9 /
C
      CALL MZFORM('CTHE','1H7I-F',IOCTHE)
      MCTHE(5) = IOCTHE
      IF( NMSR .NE. 0) THEN
        MCTHE(4) = NMSR*NMWDS + ISNILX-1
      END IF
      CALL MZLIFT(IDVSURV, JCTHE, LCSYL, -IZCTHE, MCTHE, 0)
C----------------------------------------------------------------------
  999 RETURN
      END

      SUBROUTINE BKCSYL(JCSYL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : BOOK 'CSYL' BANK.
C-
C-   Inputs  :
C-   Outputs :         JCSYL      pointer to CSYL bank
C-   Controls:
C-
C-   Created   4-MAR-1991   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CALOR_SURVEY.INC'
      INCLUDE 'D0$LINKS:IZCSYL.LINK'
      INTEGER JCSYL, MCSYL(5)
C
      DATA MCSYL / 'CSYL', 3, 2, 10, 2 /
C
      CALL MZLIFT(IDVSURV, JCSYL, LCSRV, -IZCSYL, MCSYL, 0)
C----------------------------------------------------------------------
  999 RETURN
      END

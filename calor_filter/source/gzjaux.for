      FUNCTION GZJAUX()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get link to Aux JETS bank (L2 filter)
C-
C-   Returned value  : Zebra link that supports bank: 0 if not found
C-   Inputs  :
C-   Outputs :
C-   Controls:RUN_TB is true if we are running on testbeam data
C-
C-   Created   3-AUG-1990   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZJAUX
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZJAUX.LINK'
      INTEGER GZFRES,LSUP
C----------------------------------------------------------------------
      GZJAUX = 0
      LSUP = GZFRES()
      IF (LSUP .LE. 0) RETURN

      GZJAUX = LQ(LSUP-IZJAUX)

  999 RETURN
      END

      FUNCTION GZRMAS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get link to INV MASS bank (L2 filter)
C-
C-   Returned value  : Zebra link that supports bank: 0 if not found
C-   Inputs  :
C-   Outputs :
C-
C-   Created   15-DEC-1993  KATHY FATYGA
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZRMAS
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZRMAS.LINK'
      INTEGER GZFRES,LSUP
C----------------------------------------------------------------------
      GZRMAS = 0
      LSUP = GZFRES()
      IF (LSUP .LE. 0) RETURN

      GZRMAS = LQ(LSUP-IZRMAS)

  999 RETURN
      END

      INTEGER FUNCTION GZZDST()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return link to ZDST bank
C-
C-   Returned value  :  link to ZDST
C-   Inputs  :          NONE
C-   Outputs :          NONE
C-   Controls:          NONE
C-
C-   Created  27-Sep-1994   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZZDST.LINK'
C----------------------------------------------------------------------
      IF (LHEAD.EQ.0)THEN
        GZZDST=0
        GO TO 999
      ENDIF
      GZZDST=LQ(LHEAD-IZZDST)
      IF(GZZDST.EQ.0)GO TO 999
C-
C- Verify hollerith bank identifier.
C-
      IF(IQ(GZZDST-4).NE.4HZDST)GZZDST = 0
  999 RETURN
      END

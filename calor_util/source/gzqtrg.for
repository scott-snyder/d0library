      INTEGER FUNCTION GZQTRG()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-JUN-1992   Andrew J. Milder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZQTRG.LINK'
      INTEGER LANLS,GZANLS
C----------------------------------------------------------------------
      GZQTRG = 0
      LANLS = GZANLS()
      IF (LANLS.EQ.0) THEN
        GOTO 999
      ENDIF
      GZQTRG = LQ(LANLS-IZQTRG)
C
  999 RETURN
      END

      INTEGER FUNCTION GZERMG()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to first ERMG bank
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  11-APR-1992   Andrew J. Milder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZERMG.LINK'
      INTEGER LHSTR,GZHSTR
C----------------------------------------------------------------------
      GZERMG = 0
      LHSTR = GZHSTR()
      IF (LHSTR .GT. 0) GZERMG = LQ(LHSTR-IZERMG)
  999 RETURN
      END

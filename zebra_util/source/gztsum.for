      FUNCTION GZTSUM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  20-MAR-1992   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZTSUM
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZTSUM.LINK'
      INTEGER GZHSUM,LHSUM,LTSUM
C----------------------------------------------------------------------
C
      LHSUM=GZHSUM()
      LTSUM=0
      IF(LHSUM.NE.0)  LTSUM=LQ(LHSUM-IZTSUM)
      GZTSUM=LTSUM
C
  999 RETURN
      END

      SUBROUTINE MDST_ZVERT(ZV,DZ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : RETURNS Z-VERTEX AND ERROR FROM MDST BANK
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  31-MAR-1992   Andrew J. Milder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER GZMDST,LMDST
      REAL ZV,DZ
C----------------------------------------------------------------------
      LMDST = GZMDST()
      ZV = Q(LMDST+20)
      DZ = Q(LMDST+21)
C
  999 RETURN
      END

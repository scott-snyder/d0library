      SUBROUTINE GTFHLF(HALF,NHIT)
C-----------------------------------------------------------------
C
C  Fetch contents of Zebra bank FHLF
C
C  Input:  HALF
C  Output: NHIT  = number of hits in one half of Forward Drift Chamber
C
C-   Created  ??-DEC-1988   Daria Zieminska 
C-   Updated  26-FEB-1990   Jeffrey Bantly  cleanup 
C
C-----------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER HALF,NHIT,LKFHLF
      INTEGER GZFHLF
C-----------------------------------------------------------------
      NHIT=0
      LKFHLF=GZFHLF(HALF)
      IF(LKFHLF.NE.0) NHIT=IQ(LKFHLF+1)
C------------------------------------------------------------------
 1000 RETURN
      END

      SUBROUTINE GTFDUN(HALF,UNIT,NHIT)
C-----------------------------------------------------------------
C
C  Fetch contents of Zebra bank FTHE(UNIT=0) or FPHI(UNIT=1)
C
C  Input:  HALF,UNIT
C  Output: NHIT  = number of hits in one half of Forward Drift Chamber
C
C-   Created  ??-DEC-1988   Daria Zieminska 
C-   Updated  26-FEB-1990   Jeffrey Bantly  use logical format 
C
C-----------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER HALF,UNIT,NHIT,LKFDUN
      INTEGER GZFDUN
C------------------------------------------------------------------
      NHIT=0
      LKFDUN=GZFDUN(HALF,UNIT)
      IF(LKFDUN.NE.0) NHIT=IQ(LKFDUN+1)
C------------------------------------------------------------------
 1000 RETURN
      END

      SUBROUTINE GTFTQD(HALF,QUAD,NHIT)
C-----------------------------------------------------------------
C
C  Fetch contents of Zebra bank FTQD
C
C  Input:  HALF,QUAD
C  Output: NHIT  = number of hits in one half of Forward Drift Chamber
C
C  Daria Zieminska Dec., 1988
C-   Updated   5-FEB-1990   Jeffrey Bantly  uses GZFTQD, all paths
C-   Updated  26-FEB-1990   Jeffrey Bantly  use logical format
C
C-----------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER HALF,QUAD,NHIT,LKFTQD
      INTEGER GZFTQD
C-----------------------------------------------------------------
      NHIT=0
      LKFTQD=GZFTQD(HALF,QUAD)
      IF(LKFTQD.NE.0) NHIT=IQ(LKFTQD+1)
C-----------------------------------------------------------------
 1000 RETURN
      END

      SUBROUTINE GTFPHI(HALF,NHIT)
C-----------------------------------------------------------------
C
C  Fetch contents of Zebra bank FPHI
C
C  Input:  HALF
C  Output: NHIT  = number of hits in one half of Forward Drift Chamber
C
C  Daria Zieminska Dec., 1988
C
C-----------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER HALF,NHIT,LKFPHI
      INTEGER GZFPHI
C------------------------------------------------------------------
      LKFPHI=GZFPHI(HALF)
      IF(LKFPHI.EQ.0) THEN
        NHIT=0
        GOTO 1000
      ENDIF
      NHIT=IQ(LKFPHI+1)
C------------------------------------------------------------------
 1000 RETURN
      END

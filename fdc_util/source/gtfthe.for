      SUBROUTINE GTFTHE(HALF,UNIT,NHIT)
C-----------------------------------------------------------------
C
C  Fetch contents of Zebra bank FTHE
C
C  Input:  HALF,UNIT
C  Output: NHIT  = number of hits in one half of Forward Drift Chamber
C
C  Daria Zieminska Dec., 1988
C
C-----------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER HALF,UNIT,NHIT,LKFTHE
      INTEGER GZFTHE
C------------------------------------------------------------------
      LKFTHE=GZFTHE(HALF,UNIT)
      IF(LKFTHE.EQ.0) THEN
        NHIT=0
        GOTO 1000
      ENDIF
      NHIT=IQ(LKFTHE+1)
C------------------------------------------------------------------
 1000 RETURN
      END

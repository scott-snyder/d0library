      FUNCTION FCHEKL(HALF,LADDER)
C------------------------------------------------------------------------  
C
C  Flag used FDC track segments (segments on tracks)
C
C-   Created  xx-FEB-1989   Daria Zieminska 
C-   Updated  28-FEB-1990   Jeffrey Bantly  use logical format, no paths 
C-   Updated  26-APR-1991   Jeffrey Bantly  cleanup 
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C
C------------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INTEGER HALF,LADDER(0:2),STAT,MODULE,LAYER,LOC
      INTEGER LKFTRH,LSEGM,IER
      INTEGER GZFTRH,LZFIND
C
      LOGICAL FCHEKL
      LOGICAL FIRST
C
      SAVE FIRST
      DATA FIRST/.TRUE./
C
C------------------------------------------------------------------------
C
      LKFTRH=GZFTRH()
      FCHEKL=.FALSE.
      IF (FIRST) THEN
C        CALL EZPICK('FTRAKS_RCP')
C        CALL EZRSET
        FIRST=.FALSE.
      END IF
      DO 100 LAYER=0,2
        IF (LADDER(LAYER).EQ.0) GO TO 100
        MODULE=HALF*3+LAYER
        LSEGM=LQ(LKFTRH-3-MODULE)
        LOC=LZFIND(IXCOM,LSEGM,LADDER(LAYER),-5)
        IF (BTEST(IQ(LOC),IUSED)) THEN
          FCHEKL=.TRUE.
          GO TO 999
        END IF
  100 CONTINUE
C-----------------------------------------------------------------------
  999 RETURN
      END

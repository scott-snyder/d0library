      SUBROUTINE FUSDSG(HALF,LADDER)
C------------------------------------------------------------------------
C
C  Purpose : Flag used FDC track segments (segments on tracks)
C
C-   Created  xx-FEB-1989   Daria Zieminska 
C-   Updated  28-FEB-1990   Jeffrey Bantly  clean up 
C-   Updated  29-APR-1991   Jeffrey Bantly  use new RCP,PARAMS files 
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C
C------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER HALF,LADDER(0:2),STAT,MODULE,LAYER,LOC,LSEGM
      INTEGER IER,ICALL
      INTEGER GZFSEG,LZFIND
C
      SAVE ICALL
      DATA ICALL/0/
C------------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
C        CALL EZPICK('FTRAKS_RCP')
C        CALL EZRSET
        ICALL=1
      END IF
      DO 200 LAYER=0,2
        IF(LADDER(LAYER).LE.0) GOTO 200
        LSEGM=GZFSEG(HALF,LAYER)
        LOC=LZFIND(IXCOM,LSEGM,LADDER(LAYER),-5)
        IF(LOC.LE.0) GOTO 200
        STAT=IQ(LOC)
        IQ(LOC)=IBSET(STAT,IUSED)
  200 CONTINUE
C--------------------------------------------------------------------------
  999 RETURN
      END

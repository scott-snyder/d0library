      LOGICAL FUNCTION VCHEKL(LADDER)
C------------------------------------------------------------------------  
C
C  Returns .TRUE. if a segment on ladder has been used 
C
C  Daria Zieminska Feb. 1989
C-   Updated   4-NOV-1991   Peter M. Grudberg  Add EZRSET, fix PATH 
C
C------------------------------------------------------------------------
      IMPLICIT NONE           
      INTEGER LADDER(0:2),LAYER,LOC,IUSED,LSEGM,LZFIND
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INTEGER LVTRH,GZVTRH
      INTEGER IER,ICALL
      DATA ICALL/0/
C-----------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('IUSED',IUSED,IER)
        CALL EZRSET
        ICALL=1
      END IF
      VCHEKL=.FALSE.
      LVTRH=GZVTRH()
      DO 100 LAYER=0,2
        IF (LADDER(LAYER).EQ.0) GO TO 100
        LSEGM=LQ(LVTRH-3-LAYER)
        LOC=LZFIND(0,LSEGM,LADDER(LAYER),-5)
        IF (BTEST(IQ(LOC),IUSED)) THEN
          VCHEKL=.TRUE.
          GO TO 1000
        END IF
  100 CONTINUE
 1000 RETURN
      END

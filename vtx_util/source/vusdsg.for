      SUBROUTINE VUSDSG(LADDER)
C------------------------------------------------------------------------  
C
C  Flag used VTX track segments (segments on tracks)
C
C  Daria Zieminska Feb. 1989
C-   Updated   4-NOV-1991   Peter M. Grudberg  Add EZRSET, fix PATH 
C    Updated  23_APR_1993   L.Chen change IQ(LSEG+1) from VSGs bank # to trk #
C
C------------------------------------------------------------------------
      IMPLICIT NONE           
      INTEGER LADDER(0:2),STAT,LAYER,LOC,IUSED,LSEGM,LZFIND
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INTEGER LVTRH,GZVTRH,ICALL,IER
      DATA ICALL/0/
C------------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        ICALL=1
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('IUSED',IUSED,IER)
        CALL EZRSET
      END IF
      LVTRH=GZVTRH()
      DO 200 LAYER=0,2
        IF (LADDER(LAYER).EQ.0) GO TO 200
        LSEGM=LQ(LVTRH-3-LAYER)
        LOC=LZFIND(0,LSEGM,LADDER(LAYER),-5)
        STAT=IQ(LOC)
        IQ(LOC)=IBSET(STAT,IUSED)
        IQ(LOC+1)=IQ(LVTRH+2)  !the track number to which the SEG is associated
  200 CONTINUE
 1000 RETURN
      END

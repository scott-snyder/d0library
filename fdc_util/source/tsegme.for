      SUBROUTINE TSEGME(HALF,NSEGML)
C------------------------------------------------------------------
C
C  Routine for finding track segments in FDC Theta units
C
C  Input:  HALF
C
C  Output: NSEGML(0:1,0:2) = number of segments in halves,layers
C
C-   Created  xx-NOV-1988   Daria Zieminska
C-   Updated  20-MAR-1990   Jeffrey Bantly  use logical format
C-   Updated   3-MAY-1990   Jeffrey Bantly  tineff->theta only inefficiencies
C-   Updated  15-JUN-1990   Jeffrey Bantly  add cross-sector segments
C-   Updated  29-APR-1991   Jeffrey Bantly  use new RCP,PARAMS
C-   Updated  28-FEB-1992   Susan K. Blessing   Remove option for 
C-    cross sector segments.  Always look for them.  Remove option for
C-    using link and tree.  
C-   Updated  29-JUN-1993   Susan K. Blessing  Use stand alone bank FLOC
C-    rather than USER bank.
C-   Updated  25-OCT-1993   Robert E. Avery  Clean up: 
C-              Remove remnents of link and tree. 
C-              Don't need full tracking flag anymore. 
C
C------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FLOCAL.INC/LIST'
C
      INTEGER HALF
      INTEGER NSEGML(0:1,0:2)
C
      INTEGER QUAD,SECTOR,LAYER
      INTEGER INEFF,ICALL,IER
      INTEGER NHIT,NHUSED
      INTEGER LSEGM
      INTEGER NHITXTRA(0:5)
      INTEGER GZFSEG,NZBANK
      INTEGER LFTSC, GZFTSC
      INTEGER STAT
C
      SAVE ICALL,INEFF,NHITXTRA
      DATA ICALL,NHITXTRA /0,6*0/
C--------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('TINEFF',INEFF,IER)
        CALL EZRSET
        ICALL=1
      END IF
C
C  Loop over quadrants and sectors in a half to find theta track segments
C
      CALL GTFDUN(HALF,0,NHIT)
      IF (NHIT.LT.NBTSEN-INEFF) GO TO 999
C
      DO QUAD=0,7
        LAYER=QUAD/4
        CALL GTFTQD(HALF,QUAD,NHIT)
        IF (NHIT.GE.NBTSEN-INEFF) THEN
C
          DO SECTOR=0,MXSECT
            LFTSC = GZFTSC(HALF,QUAD,SECTOR)
            IF ( LFTSC.GT.0  ) THEN
              NHIT = IQ(LFTSC+1)
              IF (NHIT.GT.1) THEN
                STAT = IQ(LFTSC)
                IF ( .NOT.BTEST(STAT,IDONE) 
     &               .AND.BTEST(STAT,ION)  )  THEN
                  IQ(LFTSC)=IBSET(STAT,IDONE)
                  IQ(LFLOC+2)=0
                  IF (NHIT.GE.NBTSEN-INEFF) THEN
C Use road method.            
                    CALL TSEGRD(HALF,QUAD,SECTOR)
C
                  ENDIF
                  NHUSED = IQ(LFLOC+2)
                  IF ( (NHIT-NHUSED).GT.1 ) THEN
                    NHITXTRA(SECTOR) = NHIT-NHUSED
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDDO                                ! End of sector loop
C
C Cross sector segments
          DO SECTOR=0,MXSECT
            IF ( NHITXTRA(SECTOR).GT.0 ) THEN
              CALL FXTSEG(HALF,QUAD,SECTOR)
              NHITXTRA(SECTOR) = 0
            ENDIF
          END DO
        ENDIF
      ENDDO                                  ! End of quadrant loop
C
      DO LAYER=0,1
        NSEGML(HALF,LAYER)=0                 ! Number of segments in layer
        LSEGM=GZFSEG(HALF,LAYER)
        IF(LSEGM.GT.5) NSEGML(HALF,LAYER)=NZBANK(IXCOM,LSEGM)
      ENDDO
C--------------------------------------------------------------------------
  999 RETURN
      END

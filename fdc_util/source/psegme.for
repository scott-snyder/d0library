      SUBROUTINE PSEGME(HALF,NSEGML)
C------------------------------------------------------------------
C
C  Purpose and Methods : Find track segments in FDC Phi units.
C
C  Inputs : HALF  = FDC Half
C  Output:  NSEGML(0:1,0:2) = number of segments in halves,layers
C
C-   Created  xx-NOV-1988   Daria Zieminska
C-   Updated  27-FEB-1990   Jeffrey Bantly  remove PATH, use logical
C-   Updated   3-MAY-1990   Jeffrey Bantly  pineff->phi only inefficiencies
C-   Updated  15-JUN-1990   Jeffrey Bantly  cross-sector segments
C-   Updated  20-MAR-1991   Robert E. Avery count cross-sector hits correctly
C-   Updated  29-APR-1991   Jeffrey Bantly  use new RCP,PARAMS files 
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
      INTEGER SECTOR,LAYER,ICALL,IER
      INTEGER INEFF
      INTEGER NHIT
      INTEGER LSEGM
      INTEGER NHUSED
      INTEGER LFPSC ,GZFPSC
      INTEGER GZFSEG,NZBANK
      INTEGER NHITXTRA(0:35)
      INTEGER STAT
C
      SAVE ICALL,INEFF,NHITXTRA
      DATA ICALL,NHITXTRA /0,36*0/
C-------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('PINEFF',INEFF,IER)
        CALL EZRSET
        ICALL=1
      END IF
C
C  Loop over sectors in a Phi chamber and find track segments
C
      CALL GTFDUN(HALF,1,NHIT)
      IF (NHIT.LT.NBPSEN-INEFF) GO TO 999
C
      DO SECTOR=0,MXSECP
        LFPSC = GZFPSC(HALF,SECTOR)
        IF ( LFPSC.GT.0  ) THEN
          NHIT = IQ(LFPSC+1)
          IF (NHIT.GT.1) THEN
            STAT = IQ(LFPSC)
            IF ( .NOT.BTEST(STAT,IDONE) 
     &           .AND.BTEST(STAT,ION)  )  THEN
              IQ(LFPSC)=IBSET(STAT,IDONE)
              IQ(LFLOC+2)=0
              IF (NHIT.GE.NBPSEN-INEFF) THEN
C Use road method.            
                CALL PSEGRD(HALF,SECTOR)
C
              ENDIF
              NHUSED = IQ(LFLOC+2)
              IF ((NHIT-NHUSED).GT.1) THEN
                NHITXTRA(SECTOR) = NHIT-NHUSED     ! Store remaining hits
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO      ! End of sector loop
C
C  Find cross-sector segments 
C
      DO SECTOR=0,MXSECP
        IF ( NHITXTRA(SECTOR).GT.0 ) THEN
          CALL FXPSEG(HALF,SECTOR)
          NHITXTRA(SECTOR) = 0
        ENDIF
      END DO
C
C  Finish up.
C
      LAYER=2
      NSEGML(HALF,LAYER)=0              ! Number of segments in LAYER
      LSEGM=GZFSEG(HALF,LAYER)
      IF(LSEGM.GT.5) NSEGML(HALF,LAYER)=NZBANK(IXCOM,LSEGM)
C------------------------------------------------------------------------
  999 RETURN
      END

      SUBROUTINE CDHFIL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill standards histograms
C-
C-   Inputs  :  HISTON(HIST) set in SRCP file 
C-                      = 0    no hist
C-                        1    histo 1st hit in bank
C-                        2          all hits
C-                        3          hits on segments in cell
C-                        4          hits on full tracks
C-
C-
C-   Outputs :  NOTE:  -  multiplicity/wire for all hits
C-                     -  track multiplicity/cell for all tracks
C-                     -  delayline resolution only if acceptable
C-                        triplet found in CDGETZ
C-
C-   Updated  10-AUG-1987   Olivier Callot
C-   Updated  29-SEP-1987   Rod Engelmann
C-   Updated  12-JUL-1988   Domenico Pizzuto
C-   Updated  22-MAR-1989   Qizhong Li-Demarteau  use SRCP
C-   Updated  15-AUG-1989   Qizhong Li-Demarteau  separate hist. by sectors 
C-   Updated  25-AUG-1989   Qizhong Li-Demarteau  separate TIME hist by side 
C-   Updated  25-OCT-1989   Qizhong Li-Demarteau  set hbook_directory
C-   Updated   5-FEB-1991   Qizhong Li-Demarteau  added histograms for
C-                                                wire hits efficiency 
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated  12-DEC-1991   Qizhong Li-Demarteau  "# of hits" histograms 
C-                                       are filled when (HISTON(5)>1) 
C-                                       wire efficiency map are added
C-                                    
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDCLNK.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$INC:CDRESF.INC'
C
      INTEGER A,KPDLYR,IPTR,KPDCDA,NHITS,IHIT,JHIT,IND,IPTR3
      INTEGER TRFLAG,NTRSEC, ERR
      INTEGER LAYER,WIRE,I,J,KPDSEC,SECTOR,IPTR1
      INTEGER IDHIST,IPTR2,IER, INIT,NHS, SIDE
      INTEGER NID (0:MXFADC,6,0:3),HIST, HISTON(8), MAXLAY, MAXSEC
      INTEGER KPDTSG(0:3), NDTSG(0:3), LDTSG(0:3)
      INTEGER NUMDL,JID(NBDELY,0:3), NDL
      INTEGER IDHLAY, WIRNUM, IDHLAY_SW, WIRNUM_SW, IDHLAY_DL, WIRNUM_DL
      REAL VAR
      LOGICAL FIRST, EZERROR
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (IQ(LCDCH+1) .EQ. 0) RETURN           ! No hit in the chamber...
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','CDHFIL',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('MAXLAY',MAXLAY,ERR)
        CALL EZGET('MAXSEC',MAXSEC,ERR)
        CALL EZGET('HISTON(1)',HISTON(1),ERR)
        CALL EZRSET
      ENDIF
C
C         Create/Set HBOOK directory for DTRAKS
C
      CALL DHDIR('DTRAKS_RCP','HBOOK_DIRECTORY',IER,' ')
      IF (IER .NE. 0) THEN
        CALL ERRMSG('DTRAKS','DFLHST',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      DO 1 LAYER = 0, MAXLAY
        IDHLAY = 5400 + LAYER
        IDHLAY_SW = 5404 + LAYER
        IDHLAY_DL = 5408 + LAYER
        DO 2 SECTOR = 0, MAXSEC
          KPDSEC = LDSEC(SECTOR, LAYER)
          IF (KPDSEC .EQ. 0) GO TO 2
          KPDCDA = LDCDA(SECTOR, LAYER)
          DO 3 HIST = 1, 4
            IF (HISTON(HIST).EQ.0) GOTO 3
            DO 4 WIRE = 0, MXFADC
              IHIT = 0
              JHIT = 0
C
C  Histograms are separated by sectors when less than six sectors are in test.
C  Otherwise histograms are not separated by sectors.
C
              IF (MAXSEC .LT. 5) THEN
                IDHIST = 1000*HIST + 100*LAYER + SECTOR*20 + WIRE
              ELSE
                IDHIST = 1000*HIST + 100*LAYER + WIRE
              ENDIF
              VAR  = 0.
              NHITS = IQ(KPDCDA+4+WIRE)
              IF (NHITS .LE. 0) GO TO 4
              IF (HISTON(HIST) .EQ. 1) NHITS = 1    ! Only first
              IND   = KPDCDA + IQ(KPDCDA+2) + 4 + WIRE
              IPTR  = KPDCDA + IQ(IND)
              DO 10 IHIT = 1, NHITS
                VAR   = Q(IPTR + HIST + 1)
                IPTR = IPTR + IQ(KPDCDA+3)
                IF (HISTON(HIST).LT.3) GO TO 7
                IF (HISTON(HIST).EQ.3) THEN            ! Check if on a segment
                  CALL CDHTRK(LAYER,SECTOR,WIRE ,IHIT,1,TRFLAG,SIDE)
                ELSEIF (HISTON (HIST).EQ.4) THEN        ! Check if on a track
                  CALL CDHTRK(LAYER,SECTOR,WIRE ,IHIT,2,TRFLAG,SIDE)
                ENDIF
                IF (TRFLAG .EQ. 0) GO TO 10
                IF (HIST .EQ. 1) VAR = VAR * ((-1)**SIDE) ! separate
                                                ! drift time by side
    7           CALL HFF1(IDHIST,NID(WIRE,HIST,LAYER),VAR,1.)
   10         CONTINUE
    4       CONTINUE
    3     CONTINUE
C
C ****  Multiplicity per wire
C
          IF (HISTON(5).NE.0) THEN
            DO 13 WIRE = 0,MXFADC
              VAR = FLOAT(IQ(KPDCDA+4+WIRE))
              IF (VAR .GT. 0) THEN
                WIRNUM = WIRE + SECTOR * (MXFADC+1)
                CALL HF1(IDHLAY,FLOAT(WIRNUM),1.)
                IF (WIRE .LE. MXSENS) THEN
                  WIRNUM_SW = WIRE + SECTOR * (MXSENS+1)
                  CALL HF1(IDHLAY_SW,FLOAT(WIRNUM_SW),1.)
                ELSE
                  WIRNUM_DL = WIRE + SECTOR * (2*NBDELY)
                  CALL HF1(IDHLAY_DL,FLOAT(WIRNUM_DL),1.)
                ENDIF
              ENDIF
              IF (HISTON(5) .GT. 1) THEN
C
C  Histograms are separated by sectors when less than six sectors are in test.
C  Otherwise histograms are not separated by sectors.
C
                IF (MAXSEC .LT. 5) THEN
                  IDHIST = 5000 + 100*LAYER + SECTOR*20 + WIRE
                ELSE
                  IDHIST = 5000 + 100*LAYER + WIRE
                ENDIF
                CALL HFF1(IDHIST,NID(WIRE,5,LAYER),VAR,1.)
              ENDIF
   13       CONTINUE
          ENDIF
C
C ****    Track multiplicity per cell
C
          IF(HISTON(6).NE.0) THEN
            CALL ZGDTSG(LAYER,NDTSG(LAYER),LDTSG(LAYER),KPDTSG(LAYER))
            IDHIST= 6000 + 100*LAYER
            VAR=FLOAT(NDTSG(LAYER))
            CALL HFF1 (IDHIST,NID (0,6,LAYER),VAR,1.)
          ENDIF
C
C ****    Delayline resolution
C
          DO 11 NUMDL=1,NBDELY
            IF( HISTON( 6+NUMDL ).NE.0 ) THEN
              NDL = 0
              IF( NUMDL .EQ. 2 ) NDL = MXSENS
              IDHIST = (6+NUMDL)*1000 + 100*LAYER + SECTOR
              JHIT=0
              NHS   = IQ( KPDSEC + 4 + NDL)
              DO 12 IHIT = 1,NHS
C
C      Check if CDGETZ filled DL resolution for acceptable triplet
C
                IF( IHIT .GT. NHTDLY ) GOTO 11
                IF(DLRES(IHIT,NUMDL,SECTOR,LAYER).EQ.-999.) GO TO 12
                JHIT=JHIT+1
                IF(HISTON(6+NUMDL).LT.3) GO TO 14
                IF (HISTON (6+NUMDL).EQ.3) THEN
                  CALL CDHTRK (LAYER,SECTOR, NDL,IHIT,1,TRFLAG,SIDE)
                ELSEIF (HISTON (6+NUMDL).EQ.4) THEN
                  CALL CDHTRK (LAYER,SECTOR, NDL,IHIT,2,TRFLAG,SIDE)
                ENDIF
                IF(TRFLAG.EQ.0) GO TO 12
   14           CALL HFF1 (IDHIST,JID( NUMDL, LAYER ),
     &                   DLRES(IHIT,NUMDL,SECTOR,LAYER),1.)
                IF(HISTON(6+NUMDL).EQ.1.AND.JHIT.EQ.1) GO TO 11
   12         CONTINUE
            ENDIF
   11     CONTINUE
    2   CONTINUE
    1 CONTINUE
 999  RETURN
      END

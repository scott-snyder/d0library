      SUBROUTINE FDROAD(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX,NF,IDF)
C------------------------------------------------------------------
C
C  Purpose and Methods : Find FDC tracks in a road.
C
C  Inputs: ZVTX     = Z position of vertex for roads
C          PHIMIN   = minimum phi
C          PHIMAX   = maximum phi
C          THEMIN   = minimum theta
C          THEMAX   = maximum theta
C
C  Outputs: NF         = number of FDC tracks found
C           IDF(1:NF)  = ID numbers of FDC tracks
C
C-   Created  xx-JAN-1989   Daria Zieminska
C-   Updated  04-JAN-1990   Qizhong Li-Demarteau   Remove unused argument
C-   Updated  05-APR-1990   Qizhong Li-Demarteau   clean up
C-   Updated  12-APR-1990   Jeff Bantly/Qizhong Li-Demarteau  fix link area
C-                                                 problem
C-   Updated   8-NOV-1990   Jeffrey Bantly  add second pass at FDC tracks
C-   Updated   3-DEC-1990   Jeffrey Bantly  change MZBOOK from 0 to IXMAIN
C-   Updated   22-JAN-1991  JB for Susan K. Blessing  Add ladders by overlapping
C-                          sectors.  Replace call to FTFDCT by FTTRAK.
C-   Updated   6-FEB-1991   Jeffrey Bantly  add check to see if full-tracking
C-                                          is already done
C-   Updated  26-APR-1991   Jeffrey Bantly  use new PARAMS, pass ZVTX for
C-                                          correct FDC road calculation
C-   Updated   6-JUN-1991   Susan K. Blessing  Remove call to FT2FDCT.
C-    This is now handled by FTTRAK and FTHIRD_LAYER.
C-   Updated  25-JUN-1991   Susan K. Blessing  Change INEFF to TINEFF from RCP.
C-   Updated  11-JUL-1991   Susan K. Blessing  Change size of LADDRS,NLADD
C-    arrays.  Don't need HALF information.
C-   Updated  16-AUG-1991   Robert E. Avery  Move FPLTRK call to FTRINI
C-   Updated  27-AUG-1991   Robert E. Avery  Don't renumber tracks,
C-                                      just mark status bits.
C-   Updated  22-NOV-1991   Robert E. Avery  Fix redo option and
C-                                              change summary output.
C-   Updated  10-DEC-1991   Robert E. Avery  Drop old banks if full
C-                         tracking was not done (first time in event).
C-   Updated  17-JAN-1992   Susan K. Blessing  Remove OVERLAD_ONLY option.
C-    Add FLINSG2 option for building two layer ladders based on angle cuts.
C-    Add call to FIONIZ to calculate ionization for FDC tracks.
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block.
C-   Updated  28-FEB-1992   Susan K. Blessing  Add EDGE_TRACKS option to
C-    build tracks using segments in edges of FDC and vertex point.
C-    Remove OVERLAD option (always use), ALL_LADDERS (remove completely),
C-    FLINSG2 (always use).
C-   Updated   6-APR-1992   Robert E. Avery  Suppress error messages for
C-    Production passes.
C-   Updated  22-MAY-1992   Susan K. Blessing  Add flag DO_TRACKING to
C-    control whether tracking is done or if only segments are found.
C-   Updated  20-JUL-1992   Qizhong Li-Demarteau  added ZVTX in the call
C-                                                NFROAD
C-   Updated  17-DEC-1992   Susan K. Blessing  Remove DO_TRACKING flag.
C-   Updated  16-FEB-1993   Susan K. Blessing  Move booking of FTRH bank
C-    so that number of hits found can be put into +7 word for every event.
C-   Updated  22-APR-1993   Susan K. Blessing  Comment out call to FLINSG.
C-   Interchange call to FOVERLAD and FLINSG_2LAY.  Add AGAIN flag to
C-    FLINSG,FLINSG_2LAY,FOVERLAD to call multiple times if MXTRAK was
C-    hit.
C-   Updated  26-APR-1993   Robert E. Avery  Call FDC_DYNADJ to set dynamic
C-                               corrections to gain.
C-   Updated   5-MAY-1993   Robert E. Avery  Call FDC_DBL3 to read in DBL3
C-                               constants.
C-   Updated  12-MAY-1993   Susan K. Blessing  Change call to FTTRAK and
C-    FEDGE_TRACKS to include road information.
C-   Updated  25-MAY-1993   Susan K. Blessing  Add switch for calling FLINSG.
C-   Updated  29-JUN-1993   Susan K. Blessing  Remove USER bank booking.
C-    Book a stand alone bank instead.
C-   Updated  18-OCT-1993   Robert E. Avery  Correct treatment for
C-                              'ideal' GEAN hits.
C-   Updated  25-OCT-1993   Robert E. Avery  Many changes.
C-     Restructured to allow reconstruction from compressed hits banks.
C-     This routine is now used to do FULL fdc tracking also.
C-     Use FDCRECO flag. Change arguments to TSEGME, PSEGME.
C-   Updated  18-NOV-1993   Robert E. Avery  Exit if no FDC data available
C-     (Raw data or compressed hits).
C-   Updated   4-MAR-1994   Srini Rajagopalan  Add tracking with z-vertex
C-                          constraint (Results stored in extended FDCT bank)
C-   Updated  30-JUN-1994   Susan K. Blessing  Check REDOFDC flag even if
C-    full tracking has already been done.
C-   Updated  17-AUG-1994   Susan K. Blessing  Call FLFSEC to set ON array
C-    of sectors in road.
C-
C------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FLOCAL.INC/LIST'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER NF,IDF(*)
      INTEGER HALF,LAYER,QUAD,SECTOR
      INTEGER INEFF,ICALL,IER
      INTEGER LKFTRH,LKFDCH,LKZTRH,PLFDCT
      INTEGER NTRSG
      INTEGER NLADD,ILADD,LADDER(0:2),NSEGML(0:1,0:2)
      INTEGER LADDRS(0:2,MXTRAK),NGOOD,NTOT
      INTEGER MAX_HITS
      INTEGER IDX,LENGTH
      PARAMETER( IDX = 1 )
      INTEGER GZFTRH,GZZTRH,GZFDCH
      INTEGER LTEMP
      INTEGER FDCRECO
C
      REAL ZVTX
      REAL PHIMIN,PHIMAX,THEMIN,THEMAX
C
      CHARACTER*4 FPATH,PATH
C
      LOGICAL REDOFDC
      LOGICAL EZERROR
      LOGICAL FULL
      LOGICAL EDGE_TRACKS
      LOGICAL AGAIN
      LOGICAL THELIM
      LOGICAL CALL_FLINSG
      LOGICAL ON(0:1,0:1,0:7,0:35)
C
      SAVE ICALL,PATH,INEFF
      SAVE REDOFDC,EDGE_TRACKS
      SAVE MAX_HITS
      SAVE FDCRECO
C
      DATA REDOFDC/.FALSE./
      DATA ICALL/0/
      DATA EDGE_TRACKS/.FALSE./
      DATA CALL_FLINSG/.FALSE./
      DATA FDCRECO/3/
C
C---------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('FTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('FTRAKS','FDROAD','FTRAKS_RCP not found.','W')
        ELSE
          CALL EZGETS('FPATH',IDX,FPATH,LENGTH,IER)
          CALL EZGET_l('REDOFDC',REDOFDC,IER)
          CALL EZGET_i('MAX_HITS',MAX_HITS,IER)
          CALL EZGET_i('TINEFF',INEFF,IER)
          CALL EZGET_l('EDGE_TRACKS',EDGE_TRACKS,IER)
          CALL EZGET_l('CALL_FLINSG',CALL_FLINSG,IER)
          CALL EZGET('FDCRECO',FDCRECO,IER)
          CALL EZRSET
        ENDIF
        ICALL = 1
      END IF
      PATH = FPATH
      CALL PATHST(PATH)
C
      CALL FDC_TIMER_RESET
C
C  Check if full tracking results already exist
C
      LKFTRH = GZFTRH()
      IF ( LKFTRH.GT.0 ) THEN
        FULL = BTEST(IQ(LKFTRH),IDONE)
      ELSE
        FULL = .FALSE.
      ENDIF
      IF (FULL.AND. .NOT.REDOFDC) THEN
        GOTO 200
      ENDIF
C
C FDCRECO=1 for hitfinding
      IF (FDCRECO.LT.1) GO TO 999
C
C Call FLFSEC to set ON array for sectors in road
      CALL FLFSEC(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX)
C
C  Make sure that initial hit-finding has been done 
C               (should have already been called in ZTR_HITS or FTRAKS)
      CALL FDC_HITS( .TRUE. ) 
C
C  Exit if no FDC data available.
C
      LKFDCH = GZFDCH()
      IF ( LKFDCH.LE.0 ) THEN
        NF = 0
        GOTO 999
      ENDIF
C
C  Find hits in sectors containing the road
C
      IF (PATH.EQ.'RECO') THEN
        CALL FRHITS(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX)
      ELSE
        CALL FRHITS_GEAN(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX)
      ENDIF
C
C FDCRECO=2 for segment finding
      IF (FDCRECO.LT.2) GO TO 999
C
C  Make sure FDC Track bank exists.
C
      LKFTRH = GZFTRH()
      IF ( LKFTRH.LE.0 ) THEN
        CALL ERRMSG('FDC-track-header-lost','FDROAD',
     &    'pointer to FTRH bank is bad','W')
        GOTO 999
      ENDIF
C
      CALL GTFDCH(NTOT)
      IF ( (MAX_HITS.GT.0) .AND.
     &     (NTOT.GT.MAX_HITS) ) THEN
        CALL ERRMSG('FDC-too-many-hits-found', 'FDROAD',
     &    'More than max FDC hits to do tracking in this road','I')
        NF = 0
        GOTO 999
      ENDIF
C
C Initialize a temporary link area
      CALL MZLINT(IXCOM,'/FLOCAL/',FLOCAL,LFLOC,FLOCAL)
C Create stand alone bank for use in later routines
      CALL MZBOOK(IXMAIN,LTEMP,0,2,'FLOC',9,9,10,2,0)
      LFLOC = LTEMP
C
C Loop over both halves of FDC to find tracks
C
      CALL VZERO(NSEGML(0,0),6)
      DO 100 HALF = 0,MXHALF
C
        THELIM = .TRUE.
        IF (THEMIN.GT.HALFPI.AND.HALF.EQ.1) THELIM=.FALSE.
        IF (THEMAX.LT.HALFPI.AND.HALF.EQ.0) THELIM=.FALSE.
        IF (THELIM) THEN
C
C Find FDC segments
C
          CALL TSEGME(HALF,NSEGML)     ! Find THETA segments
          CALL PSEGME(HALF,NSEGML)     ! Find PHI segments
C
C FDCRECO=3 for tracking
          IF (FDCRECO.LT.3) GOTO 100
C
C Make FDC track candidates by matching segments
C
          NTRSG = NSEGML(HALF,0)+NSEGML(HALF,1)+NSEGML(HALF,2)
          IF (NTRSG.EQ.0) GOTO 100
C
C Form three layer ladders by matching angles of segments
C
          IF (CALL_FLINSG) THEN
            NLADD = 0
            AGAIN = .FALSE.
            DO WHILE (NLADD.EQ.0.OR.NLADD.EQ.MXTRAK)
              NLADD = 0
              CALL FLINSG(HALF,NLADD,LADDRS,AGAIN)
C
              IF (NLADD.GT.0) THEN
                CALL FTTRAK(HALF,NLADD,LADDRS,
     &            ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX)
                CALL VZERO(LADDRS,3*NLADD)
                AGAIN = .TRUE.
              ELSE
                NLADD = -1
              END IF
            END DO
          END IF
C
C Form two layer ladders from unused segments using overlapping sectors
C
          NLADD = 0
          AGAIN = .FALSE.
          DO WHILE (NLADD.EQ.0.OR.NLADD.EQ.MXTRAK)
            NLADD = 0
            CALL FOVERLAD(HALF,NLADD,LADDRS,AGAIN)
C
            IF (NLADD.GT.0) THEN
              CALL FTTRAK(HALF,NLADD,LADDRS,
     &          ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX)
              CALL VZERO(LADDRS,3*NLADD)
              AGAIN = .TRUE.
            ELSE
              NLADD = -1
            END IF
          END DO
C
C Form two layer ladders from unused segments by matching angles
C
          NLADD = 0
          AGAIN = .FALSE.
          DO WHILE (NLADD.EQ.0.OR.NLADD.EQ.MXTRAK)
            NLADD = 0
            CALL FLINSG_2LAY(HALF,NLADD,LADDRS,AGAIN)
C
            IF (NLADD.GT.0) THEN
              CALL FTTRAK(HALF,NLADD,LADDRS,
     &          ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX)
              CALL VZERO(LADDRS,3*NLADD)
              AGAIN = .TRUE.
            ELSE
              NLADD = -1
            END IF
          END DO
C
C Make tracks from inner theta sectors 4 and 5 and from outer theta sector
C 0 if requested.
C
          IF (EDGE_TRACKS) THEN
            CALL FEDGE_TRACKS(HALF,ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX)
          END IF
C
        END IF
  100 CONTINUE                          ! End of loop over FDC Half
C
C Drop stand alone bank when done
C
      CALL MZDROP(IXCOM,LFLOC,'L')
      FLOCAL(1) = 0
C
C Calculate ionization for FDC tracks
      CALL FIONIZ
C
C Mark status bits for FDC tracks
C
      CALL FMARK_TRKHITS
C
C Set status bits in FHIT bank
C
      CALL FHITST
C
C Do tracking with vertex constraint
C
      LKFTRH = GZFTRH()
      IF (LKFTRH.GT.0) THEN
        PLFDCT = LQ(LKFTRH-1)
        DO WHILE (PLFDCT.GT.0)
          CALL FDC_VERTEX_FIT(PLFDCT)
          PLFDCT = LQ(PLFDCT)
        ENDDO
      ENDIF
C
  200 CONTINUE
C
C  Find number of FDC tracks in the road and their ID's
C
      CALL NFROAD(ZVTX,PHIMIN,PHIMAX,THEMIN,THEMAX,NF,IDF)
C
C  Done
C
      CALL FDC_TIMER_UPDATE('Tracking in a road done')
C
C---------------------------------------------------------------------
C
  999 CONTINUE
C
      CALL PATHRS
C
      RETURN
      END 

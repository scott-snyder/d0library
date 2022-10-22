      SUBROUTINE PF_FIT_SEGMENTS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Action Routine to draw a multiple window 
C-                         display of an FDC R-Z view, fit a track to 
C-                         chosen segments and then draw single track's
C-                         sector plots, with the fitted track.
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  13-MAY-1991   Robert E. Avery 
C-   Updated  17-JUN-1991   Susan K. Blessing  Change size of CONT, ICONT
C-    arrays.
C-   Updated   7-NOV-1991   Robert E. Avery  Add new options,  
C-      'FDC FIT SEGTRK', do fit using SEGTRK fit (see FDC_SEGTRK in FDC_UTIL)
C-      'FDC FIT NO DL',  do fit with DL not in fit.
C-   Updated  25-JAN-1992   Robert E. Avery  Improve error checking for 
C-      bad ladders. 
C-   Updated  28-FEB-1992   Susan K. Blessing  Allow fitting of one layer
C-    tracks.
C-   Updated  28-FEB-1992   Robert E. Avery  Draw sector if the track
C-     passes through at either first or last wire of cell.
C-   Updated  16-MAR-1992   Robert E. Avery    Change FTFDCT call  
C-   Updated   2-APR-1993   Robert E. Avery  Compute ionization. 
C-   Updated  29-JUL-1993   Susan K. Blessing  Change FTFDCT call to include
C-    vertex number or 0 to not fit to a vertex.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:FLOCAL.INC/LIST'
      INCLUDE 'D0$LINKS:IZFDTH.LINK/LIST'
C
      INTEGER ICALL,JCALL,IFL
      INTEGER TRKNUM,LADDER(0:2),LOGCHA
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE,UB,MODULE
      INTEGER LAYER, QD
      INTEGER LFTRH ,GZFTRH
      INTEGER IXFDCT
      INTEGER IXFDTH
      INTEGER LFDTH
      INTEGER FIT_NO_DL 
      INTEGER IER
      INTEGER NDELAY
C
      REAL    CONT(62)
      REAL    XWIRE,YWIRE,ZWIRE
      REAL    XTRK,YTRK 
      REAL    Z0(0:1)
      REAL    CHINORM
      REAL    IONIZATION,IONERR
C
      CHARACTER*80 TEXT
C
      INTEGER IADD
      REAL    FIADD
      EQUIVALENCE ( FIADD, IADD )
C
      REAL QTRAK(26),QHTRK(3,34)        
      INTEGER IQTRAK(26),IQHTRK(3,34)        
      EQUIVALENCE (IQTRAK,QTRAK),(IQHTRK,QHTRK)
      LOGICAL PFPICK_SEGMT
      LOGICAL OK 
      LOGICAL SAVE_FIT_NO_DL
      LOGICAL EZERROR
      LOGICAL USE_FIT_SEGTRK 
      LOGICAL ONE_LAYER
      LOGICAL USE_VERTEX
C
      SAVE ICALL,JCALL,IFL,Z0,USE_VERTEX
      DATA ICALL,JCALL,IFL/0,0,2/
      DATA USE_VERTEX/.TRUE./
C----------------------------------------------------------------------
      IF(ICALL.EQ.0) THEN
        CALL MZFORM('FDCT','1B 1I 1B 19F',IXFDCT)
        CALL MZFORM('FDTH','/*S',IXFDTH)
        LFTRH = GZFTRH()
        IF ( LFTRH .GT. 0 ) THEN
          Z0(0)= Q(LFTRH+3)
          Z0(1)= Q(LFTRH+4)
          ICALL=1
        ENDIF
C
        CALL EZPICK('FTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('PIXIE','PF_FIT_SEGMENTS',
     &                  'Cannot find FTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('FIT_NO_DL',SAVE_FIT_NO_DL,IER)
        CALL EZRSET
      ENDIF
C
C
      JCALL = MOD(JCALL,4) + 1
      IF(JCALL.EQ.1) THEN 
C
C  List all of the FDC SEGMENTS shown and form ladder.
C
        OK = PFPICK_SEGMT(LADDER,HALF) 
        IF(.NOT.OK) THEN
          CALL INTMSG(' No segments present and-or chosen.')
          GOTO 999
        ENDIF
        IF(   ((LADDER(0).EQ.0) .AND. (LADDER(1).EQ.0))
     &    .OR.((LADDER(0).EQ.0) .AND. (LADDER(2).EQ.0))
     &    .OR.((LADDER(1).EQ.0) .AND. (LADDER(2).EQ.0)) ) THEN
C
          IF (LADDER(2).EQ.0.AND.(LADDER(0).GT.0.OR.LADDER(1).GT.0)) 
     &      THEN
            ONE_LAYER = .TRUE.
C Check for delay lines
            IF (LADDER(0).GT.0) THEN
              CALL FCHECK_DL(HALF,0,LADDER(0),NDELAY)
            ELSE
              CALL FCHECK_DL(HALF,1,LADDER(1),NDELAY)
            END IF
            IF ( NDELAY.EQ.0 ) THEN
              CALL INTMSG(' You must choose at least one theta'//
     &                          ' segment with a delay line.')
              OK = .FALSE.
              GOTO 999
            ENDIF
          ELSE
            CALL INTMSG(' You must choose at least one theta segment.')
            OK = .FALSE.
            GOTO 999
          END IF
        ENDIF
C
C  Determine whether to include DL in fit:
C
        CALL EZPICK('PX_FDCDIS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('PIXIE','PF_FIT_SEGMENTS',
     &                  'Cannot find PX_FDCDIS_RCP','W')
          GOTO 999
        ENDIF
        CALL PUGET_i('FDC FIT NO DL',FIT_NO_DL)
        CALL PUGET_l('FDC FIT SEGTRK',USE_FIT_SEGTRK)
        CALL EZRSET
C
        IF ( FIT_NO_DL .GT. 0) THEN
          CALL FTFDCT_FIT_NO_DL(.TRUE.)
          CALL FIT_SEGTRK_NO_DL(.TRUE.)
        ELSEIF ( FIT_NO_DL .LT. 0) THEN
          CALL FTFDCT_FIT_NO_DL(.FALSE.)
          CALL FIT_SEGTRK_NO_DL(.FALSE.)
        ENDIF                           ! Else leave at default
C
C Fit track to ladder and store in user area:
C
        IF (ONE_LAYER) THEN
          CALL FTFDCT(HALF,LADDER,QTRAK,IQTRAK,QHTRK,IQHTRK,
     &      CHINORM,1)
        ELSE IF ( USE_FIT_SEGTRK ) THEN
          CALL FIT_SEGTRK(HALF,LADDER,QTRAK,IQTRAK,CHINORM) 
        ELSE
          CALL FTFDCT(HALF,LADDER,QTRAK,IQTRAK,QHTRK,IQHTRK,
     &      CHINORM,0)
        ENDIF
        CALL FDEDX(HALF,LADDER,QTRAK(22),IONIZATION,IONERR)
        IQTRAK(1)=HALF
        QTRAK(20) = IONIZATION
        QTRAK(21) = IONERR
C
C Book temporary stand alone bank:
C
        CALL MZLINT(IXCOM,'/FLOCAL/',FLOCAL,LFLOC,FLOCAL)
        CALL MZBOOK(IXMAIN,LFLOC,0,2,'FDCT',1,1,26,IXFDCT,0)
        CALL UCOPY(QTRAK,Q(LFLOC+1),26)
        CALL MZBOOK(IXMAIN,LFDTH,LFLOC,-IZFDTH,'FDTH',0,0,105,IXFDTH,0)
        CALL UCOPY_i(LADDER,IQ(LFDTH+103),3)        ! Put in ladder
C
C  Reset DL fitting
C 
        CALL FTFDCT_FIT_NO_DL(SAVE_FIT_NO_DL)
        CALL FIT_SEGTRK_NO_DL(SAVE_FIT_NO_DL)
C
C  Output useful information to first screen.
C
        CALL PF_PR_SEGMENTS(HALF,LADDER,QTRAK,IQTRAK,CHINORM)
C
        GOTO 999
      ELSE
        IF(.NOT.OK) THEN
          GOTO 999
        ENDIF
      ENDIF
C
      LAYER = JCALL-2
      IF(LADDER(LAYER).GT.0) THEN
        MODULE=HALF*3+LAYER
        CALL GTFSEG(MODULE,LADDER(LAYER),CONT)
        FIADD=CONT(2)
        LOGCHA=IADD
C
C  Decode address and call FADC display routine with info.
C
        CALL FCODER(LOGCHA,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
C
        IF(UNIT.EQ.0) THEN
          CALL PF_THETA_SECT_VIEW(HALF,QUAD,SECTOR)
        ELSEIF(UNIT.EQ.1) THEN
          CALL PF_PHI_SECT_VIEW(HALF,SECTOR)
        ENDIF
C
      ELSE                              ! Find what sector track is in
        UNIT = 0
        QD = 0
        IF ( LAYER .EQ. 2 ) UNIT=1
        IF ( LAYER .EQ. 1 ) QD = 4
        CALL GTFALH( HALF,UNIT,QD,0,0,
     &               XWIRE,YWIRE,ZWIRE)
        XTRK = QTRAK(4) + QTRAK(7)* (ZWIRE - Z0(HALF) ) 
        YTRK = QTRAK(5) + QTRAK(8)* (ZWIRE - Z0(HALF) )
C
        CALL FGET_SECTOR(XTRK,YTRK,HALF,LAYER,QUAD,SECTOR)
        IF ( SECTOR .GT. -1 ) THEN
          IF(UNIT.EQ.0) THEN
            CALL PF_THETA_SECT_VIEW(HALF,QUAD,SECTOR)
          ELSEIF(UNIT.EQ.1) THEN
            CALL PF_PHI_SECT_VIEW(HALF,SECTOR)
          ENDIF
        ELSEIF(UNIT.EQ.1) THEN
          CALL GTFALH( HALF,UNIT,0,0,15,
     &               XWIRE,YWIRE,ZWIRE)
          XTRK = QTRAK(4) + QTRAK(7)* (ZWIRE - Z0(HALF) ) 
          YTRK = QTRAK(5) + QTRAK(8)* (ZWIRE - Z0(HALF) )
          CALL FGET_SECTOR(XTRK,YTRK,HALF,LAYER,QUAD,SECTOR)
          IF ( SECTOR .GT. -1 ) THEN
            CALL PF_PHI_SECT_VIEW(HALF,SECTOR)
          ELSE
            CALL PF_PR_MISS(LAYER,XTRK,YTRK)
          ENDIF
        ELSE
          CALL GTFALH( HALF,UNIT,QD,0,7,
     &               XWIRE,YWIRE,ZWIRE)
          XTRK = QTRAK(4) + QTRAK(7)* (ZWIRE - Z0(HALF) ) 
          YTRK = QTRAK(5) + QTRAK(8)* (ZWIRE - Z0(HALF) )
          CALL FGET_SECTOR(XTRK,YTRK,HALF,LAYER,QUAD,SECTOR)
          IF ( SECTOR .GT. -1 ) THEN
            CALL PF_THETA_SECT_VIEW(HALF,QUAD,SECTOR)
          ELSE
            CALL PF_PR_MISS(LAYER,XTRK,YTRK)
          ENDIF
        ENDIF
C
      ENDIF
      IF(JCALL.EQ.4) THEN
        CALL MZDROP(IXCOM,LFLOC,'L')
        FLOCAL(1) = 0
        LFLOC = 0
      ENDIF
C
C  Done.
C
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END

      SUBROUTINE FDC_HITS( FULL_HITFINDING ) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Full hit processing for the FDC.
C-      Either process hits starting from RAW data 
C-      or allow existing compressed hits to be used.
C-      Other event initialization is also done here.
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  18-OCT-1993   Robert E. Avery
C-   Updated  19-DEC-1995   Susan K. Blessing  Remove run checking from 
C-    FDC_DBL3 call so that FDC_DBL3 will be called even if the first
C-    event contains no FDC information.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FDEVNT.INC'
      INCLUDE 'D0$LINKS:IZCDD3.LINK'
C
      LOGICAL FULL_HITFINDING
C
      INTEGER LFTRH, GZFTRH
      INTEGER LFDCH, GZFDCH
      INTEGER LFHIT, GZFHIT
      INTEGER LFCHT, GZFCHT
      INTEGER NTOT
      INTEGER LCDD3
      INTEGER RUN,ID,RUNSAV,IDSAV
      INTEGER IER
      INTEGER RUNTYPE
      INTEGER IDX,LENGTH
      PARAMETER( IDX = 1 )
      INTEGER I
C
      CHARACTER*4 PATH,FPATH
C
      LOGICAL REDOFDC
      LOGICAL EZERROR
      LOGICAL FULL
      LOGICAL NEW_RUN
      LOGICAL FIRST
      LOGICAL RAW_EXIST 
      LOGICAL FHIT_EXIST 
      LOGICAL FCHT_EXIST 
      LOGICAL FULLTR_EXISTS 
      LOGICAL CREATE_FHIT  
      LOGICAL DONE 
      LOGICAL FTPREQ
      LOGICAL REDOFDC_TRACKING
C
      SAVE FTPREQ,RUNTYPE,IDSAV,RUNSAV,FPATH,FIRST
      SAVE REDOFDC,REDOFDC_TRACKING 
      DATA FIRST/.TRUE./
      DATA REDOFDC/.FALSE./
      DATA REDOFDC_TRACKING /.TRUE./
      DATA RUNSAV,IDSAV /-1,-1/
      DATA DONE /.FALSE./
C
C----------------------------------------------------------------------
C
C  make sure the FDC full hit reconstruction is done once per event only
C
      CALL EVNTID(RUN,ID)
      NEW_RUN = RUN .NE. RUNSAV
      IF (RUN .NE. RUNSAV .OR. ID .NE. IDSAV) THEN
        RUNSAV = RUN
        IDSAV = ID
      ELSE
        GOTO 999
      ENDIF
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('FTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('FTRAKS','FTRAKS','FTRAKS_RCP not found.','W')
        ELSE
          CALL EZGETS('FPATH',IDX,FPATH,LENGTH,IER)
          CALL EZGET('REDOFDC',REDOFDC,IER)
          CALL EZGET('REDOFDC_TRACKING',REDOFDC_TRACKING,IER)
          CALL EZGET('RUNTYPE',RUNTYPE,IER)
          CALL EZGET('FTPREQ',FTPREQ,IER)
          CALL EZRSET
        ENDIF
      END IF
      CALL FDC_TIMER_UPDATE('Begin event')
C
C  Check if GEAN hits are requested:
C
      PATH = FPATH
      CALL PATHST(PATH)
      IF (PATH .EQ. 'GEAN') THEN
        IF ( REDOFDC ) THEN
          LFTRH = GZFTRH()
          IF (LFTRH.GT.0) CALL MZDROP(IXCOM,LFTRH,' ')
          IF (LFTRH.GT.0) CALL MZGARB(IXMAIN,0)
        ENDIF
        GOTO 900
      ENDIF
C
C Check what databanks exist:
C
      LCDD3 = LQ(LHEAD - IZCDD3)        ! Raw data
      RAW_EXIST = LCDD3.GT.0
C
      LFCHT = GZFCHT()                  ! L2 style compressed hits
      FCHT_EXIST = LFCHT.GT.0
C
      LFHIT = GZFHIT()                  ! Space-point compressed hits
      IF ( LFHIT.GT.0 ) THEN
        FHIT_EXIST = BTEST(IQ(LFHIT),IDONE)
      ELSE
        FHIT_EXIST = .FALSE.
      ENDIF
C
      LFTRH = GZFTRH()                  ! Full tracking
      IF ( LFTRH.GT.0 ) THEN
        FULLTR_EXISTS = BTEST(IQ(LFTRH),IDONE)
      ELSE
        FULLTR_EXISTS = .FALSE.
      ENDIF
C
C Decide what reprocessing needs to be done:
C
      IF ( FHIT_EXIST.AND..NOT.REDOFDC ) THEN
        IF ( REDOFDC_TRACKING ) THEN
          IF (LFTRH.GT.0) CALL MZDROP(IXCOM,LFTRH,' ')
        ELSE
          IF ( LFTRH.GT.0 ) THEN
            IF ( .NOT.FULLTR_EXISTS  ) THEN
              CALL ERRMSG('FDC-not-redone','FDC_HITS',
     &          'REDOFDC_TRACKING switch not set, FDROAD can not '//
     &          'find new tracks since old ones exist','W')
              IQ(LFTRH) = IBSET(IQ(LFTRH),IDONE)
            ENDIF
          ENDIF
          GOTO 990  ! New hitfinding/tracking will not be done
        ENDIF
      ENDIF
C
      IF ( RAW_EXIST.OR.FCHT_EXIST ) THEN
        IF ( REDOFDC .OR. .NOT.FHIT_EXIST ) THEN
          CREATE_FHIT = .TRUE.
        ENDIF
      ELSEIF ( .NOT.FHIT_EXIST ) THEN
        CALL ERRMSG('FDC-NO-DATA','FDC_HITS',
     &    'No FDC data exists, can not reconstruct FDC!','W')
        LFTRH = GZFTRH()                  
        IF ( LFTRH.GT.0 ) THEN
          IQ(LFTRH) = IBSET(IQ(LFTRH),IDONE)
        ENDIF
        GOTO 990
      ENDIF
C
      IF ( CREATE_FHIT  ) THEN
        IF ( .NOT.DONE ) THEN
          DONE = .TRUE.
          IF ( RAW_EXIST ) THEN
            CALL INTMSG(' FDC-Reconstruction from raw data (CDD3)')
          ELSE
            CALL INTMSG(
     &        ' FDC-Reconstruction from L2-style compressed hits')
          ENDIF
        ENDIF
C
C  Drop any existing processed banks:
C
        LFDCH = GZFDCH()
        IF (LFDCH.GT.0) THEN 
          IF ( RAW_EXIST ) THEN
            CALL MZDROP(IXCOM,LFDCH,' ')
          ELSEIF (FCHT_EXIST ) THEN
            DO I =  1, 3
              IF ( LQ(LFDCH-I).GT.0  ) THEN
                CALL MZDROP(IXCOM,LQ(LFDCH-I),' ')
              ENDIF
            ENDDO
            IQ(LFDCH+1)  = 0   ! # hits in FDC now
            IQ(LFDCH+10) = 0   ! # DL hits in FDC now
          ENDIF
        ENDIF
        LFTRH = GZFTRH()
        IF (LFTRH.GT.0) CALL MZDROP(IXCOM,LFTRH,' ')
        IF ((LFTRH.GT.0).OR.(LFDCH.GT.0)) CALL MZGARB(IXMAIN,0)
C
C  Perform initialization for this event.
C
C     Timing pulse, for cosmic and TB data.
C
        TMPUBN = 0.0
        FINDTP = .FALSE.
        IF ( (RUNTYPE.GT.0) .AND. (RUNTYPE.LE.6) ) THEN
          CALL FDTBTP
          IF ( (.NOT.FINDTP) .AND. (FTPREQ) ) THEN
            GOTO 900
          ENDIF
        ENDIF
C
C     Fetch calibration constants
C
        CALL FDC_DBL3(RUN)
        CALL FDC_DYNADJ
        CALL FDC_TIMER_UPDATE('Dbl3 read done')
C
C  Perform FULL hitfinding. (PHIMIN=PHIMAX ==> full hitfinding)
C
        CALL BKFDCH( LFDCH )
        IF ( FULL_HITFINDING  ) THEN
          CALL FRHITS(0.,0.,0.,0.,0.)
        ENDIF
C
      ELSE
C
C  Use existing FHIT banks for reconstruction. Drop everything else.
C
        IF ( .NOT.DONE ) THEN
          DONE = .TRUE.
          CALL INTMSG(
     &      ' FDC-Reconstruction from compressed hits bank (FHIT)')
        ENDIF
C
        LFDCH = GZFDCH()
        IF (LFDCH.GT.0) THEN 
          DO I =  1, 2
            IF ( LQ(LFDCH-I).GT.0  ) THEN
              CALL MZDROP(IXCOM,LQ(LFDCH-I),' ')
            ENDIF
          ENDDO
          IQ(LFDCH+1)  = 0   ! # hits in FDC now
          IQ(LFDCH+10) = 0   ! # DL hits in FDC now
        ENDIF
        LFTRH = GZFTRH()
        IF (LFTRH.GT.0) CALL MZDROP(IXCOM,LFTRH,' ')
        IF ((LFTRH.GT.0).OR.(LFDCH.GT.0)) CALL MZGARB(IXMAIN,0)
      ENDIF
C
C  Book FDC Track header bank. zero hits found (until roads are called).
C
  900 CONTINUE
      CALL BKFTRH(LFTRH)
      IF ( LFTRH.LE.0 ) THEN
        CALL ERRMSG('FDC-track-header-lost','FDC_HITS',
     &    'pointer to FTRH bank is bad','W')
        GOTO 990
      ENDIF
      LFDCH = GZFDCH()
      IF (LFDCH.LE.0) THEN 
        CALL ERRMSG('FDC-hit-header-lost','FDC_HITS',
     &    'pointer to FDCH bank is bad','W')
        GOTO 990
      ENDIF
C
      IQ(LFTRH+7) = 0
C
  990 CONTINUE
      CALL FDC_TIMER_UPDATE('FDC hits done')
      CALL PATHRS
  999 CONTINUE
      RETURN
      END

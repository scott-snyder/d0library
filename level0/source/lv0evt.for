      FUNCTION LV0EVT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Main event analyzing control routine for LEVEL0
C-
C-   Returned value  : TRUE if LV0EVT called.
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   2-JUN-1992   Jeffrey Bantly
C-   Updated  30-NOV-1992   Jeffrey Bantly  skip if monte carlo data
C-   Updated  25-JAN-1993   Jeffrey Bantly  add diagnostics routine call
C-   Updated  10-DEC-1994   Jeffrey Bantly  add special MAX_LIVE veto section
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL LV0EVT
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER IER
      INTEGER LKPLV0
C
      LOGICAL EZERROR,FIRST,DOHIST
      LOGICAL DODIAG,DOFISA,DOCALIB,MCDATA
      LOGICAL OK, LV0REC
      EXTERNAL LV0REC
C
      INTEGER MAX_BAD_MR,NRUN,N1RUN,I
      PARAMETER( MAX_BAD_MR = 20 )
      INTEGER MR_BITS,IERR,RUNNO,RUNNUM,BAD_MR_RUNNO(MAX_BAD_MR)
      REAL TIME29
      LOGICAL MRBS, MBLANK
      EXTERNAL RUNNO
C
      SAVE FIRST,DOHIST
      SAVE DODIAG,DOFISA,DOCALIB,MCDATA
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      LV0EVT = .TRUE.
C
      IF(FIRST) THEN
        CALL EZPICK('LEVEL0_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('LEVEL0-no-rcp','LV0EVT',
     &                                 'LEVEL0_RCP not found.','W')
          GOTO 999
        ELSE
          CALL EZGETA_iarra('BAD_MR_RUNNO',0,0,0,NRUN,IER)
          IF (IER.EQ.0)
     &      CALL EZGETA('BAD_MR_RUNNO',1,NRUN,1,BAD_MR_RUNNO,IER)
          IF (NRUN.GT.MAX_BAD_MR) NRUN=MAX_BAD_MR
          CALL EZGET('DODIAG',DODIAG,IER)
          CALL EZGET('DOFISA',DOFISA,IER)
          CALL EZGET('DOHIST',DOHIST,IER)
          CALL EZGET('DOCALIB',DOCALIB,IER)
          CALL EZRSET
          IF ( DOCALIB ) THEN
C            CALL LV0_CALIB_BOOK
          ENDIF
        ENDIF
        MCDATA = IQ(LHEAD+1) .GT. 1000
        FIRST=.FALSE.
      ENDIF
C
C*****************************************************************************
C*****************************************************************************
C*****************************************************************************
C
C   Special Active Veto condition check for early Oct 1994 messed up data.
C   Run 1B Global runs 84175-84538.
C
      N1RUN=NRUN/2
      RUNNUM=RUNNO()
      DO I = 1, N1RUN
        IF ( RUNNUM.GE.BAD_MR_RUNNO(2*I-1) .AND.
     &         RUNNUM.LE.BAD_MR_RUNNO(2*I)) THEN
          MRBS=.FALSE.
          MBLANK=.FALSE.
          MR_BITS=0
          CALL MAIN_RING(TIME29,MR_BITS,IERR)
          IF (IERR.NE.0) THEN
            CALL ERRMSG('MAX_LIVE could not be checked','LV0EVT',
     &        'MAX_LIVE veto not made, TRGR missing','W')
          ELSE
            MRBS=BTEST(MR_BITS,12)
            MBLANK=BTEST(MR_BITS,13)
            IF (MRBS) THEN
              CALL ERRMSG('Event selected, fails MRBS','LV0EVT',
     &          'Event selected for failing MRBS','W')
              IF (MBLANK) THEN
                CALL ERRMSG('Event rejected, fails MAX_LIVE','LV0EVT',
     &            'Event rejected for failing MAX_LIVE','W')
                LV0EVT=.FALSE.
                GOTO 999
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C
C*****************************************************************************
C*****************************************************************************
C*****************************************************************************
C
      IF ( MCDATA ) THEN
        CALL BKPLV0(LKPLV0)
        GOTO 999
      ENDIF
C
C ****  Perform Level 0 reconstruction.
C
      OK=LV0REC()
C
C ****  Perform Level 0 Diagnostic studies, if requested (default=F).
C
      IF ( DODIAG ) THEN
C        CALL LV0DIA
      ENDIF
      IF (.NOT.OK) GOTO 999
C
C ****  Book and fill Level 0 histograms, if requested (default=T).
C
      IF ( DOHIST ) THEN
        CALL LV0HIS
      ENDIF
C
C ****  Perform Level 0 Isajet studies, if requested (default=F).
C
      IF ( DOFISA ) THEN
C        CALL LV0ISA
      ENDIF
C
C ****  Perform Level 0 calibration study, if requested (default=F).
C
      IF ( DOCALIB ) THEN
C        CALL LV0_CALIB
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END

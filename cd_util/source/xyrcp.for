      SUBROUTINE XYRCP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read in XY information.  First, look at link
C-     IZBMXY from VTXH bank.  If not there, try to read either from
C-     file or from run-summary database.  If any of above works, overwrite
C-     the VERTEX_RCP bank so that ZCONST_FIT will work properly.  If
C-     none works, restore original values from RCP file.
C-
C-   Controls: VERTEX_RCP
C-
C-   Created  18-APR-1994   Justin R. Bendich
C-
C-   Modified 26-JAN-1995   Justin R. Bendich
C-      Run before which database is not used is now RCP-settable
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$LINKS:IZBMXY.LINK'
      REAL BPUncorr(2), IPSigma(2), BPErr(2), Dummy
      LOGICAL First, MCDATA, BMXY_Read, IsBMXY, DBBypass
      INTEGER I, IER, Run, RUNNO, LastRun, ARun, LVTXH, GZVTXH,
     &        HoursStale, FormIndex, FirstDBRun
      CHARACTER*39 Message
C
      REAL RCP_BEAM_POS(3), RCP_BEAM_ERR(3)
      COMMON/USYKGI/ RCP_BEAM_POS, RCP_BEAM_ERR
C
      DATA First/.TRUE./, LastRun/-1/
   10 FORMAT('Error copying into ZEB',A3,'; IQUEST =',I4)
C----------------------------------------------------------------------
      IF(First) THEN
        First = .FALSE.
        CALL EZLOC('VERTEX_RCP', I)
        IF(I .LE. 0) THEN
          CALL INRCP('VERTEX_RCP', IER)
          IF(IER .NE. 0) CALL ERRMSG('INRCP','XYRCP','VERTEX_RCP','F')
        ENDIF
        CALL EZPICK('VERTEX_RCP')
        CALL EZGET('BEAM_POS', RCP_BEAM_POS, IER)
        IF(IER .NE. 0) CALL ERRMSG('VERTEX_RCP', 'XYRCP',
     &    'BEAM_POS not found', 'F')
        CALL EZGET('BEAM_ERR', RCP_BEAM_ERR, IER)
        IF(IER .NE. 0) CALL ERRMSG('VERTEX_RCP', 'XYRCP',
     &    'BEAM_ERR not found', 'F')
        CALL EZGETA('IP_SIGMA', 1, 2, 1, IPSigma, IER)
        IF(IER .NE. 0) CALL ERRMSG('VERTEX_RCP', 'XYRCP',
     &    'IP_SIGMA not found', 'F')
        CALL EZGET('HOURS_STALE', HoursStale, IER)
        IF(IER .NE. 0) CALL ERRMSG('VERTEX_RCP', 'XYRCP',
     &    'HOURS_STALE not found', 'F')
        CALL EZGET('FIRST_DB_RUN', FirstDBRun, IER)
        IF(IER .NE. 0) CALL ERRMSG('VERTEX_RCP', 'XYRCP',
     &    'FIRST_DB_RUN not found', 'F')
        CALL EZGET('BYPASS_DBL3_ERROR', DBBypass, IER)
        IF(IER .NE. 0) CALL ERRMSG('VERTEX_RCP', 'XYRCP',
     &    'BYPASS_DBL3_ERROR not found', 'F')
        CALL EZRSET
        CALL MZFORM('BMXY', '1I8F1I10F1I2F', FormIndex)
        MCDATA = IQ(LHEAD + 1) .GT. 1000
        LBMXY = 0
      ENDIF
      IF (MCDATA) GO TO 999
      Run = RUNNO()
      IF(Run .EQ. LastRun) GOTO 999
      LastRun = Run
      LVTXH = GZVTXH(0)
      IF(LVTXH .GT. 0) THEN
        IsBMXY = LQ(LVTXH - IZBMXY) .GT. 0
      ELSE
        IsBMXY = .FALSE.
      ENDIF
      IF(IsBMXY) THEN
C
C ****  Copy ZEBCOM BMXY to ZEBSTP
C
        IF(LBMXY .GT. 0) THEN
          CALL MZDROP(IXSTP, LBMXY, ' ')
          LBMXY = 0
        ENDIF
        CALL MZCOPY(IXMAIN, LQ(LVTXH - IZBMXY), IDVSTP, LBMXY, 1, ' ')
        IF(IQUEST(1) .NE. 0) THEN
          WRITE(Message,10) 'STP', IQUEST(1)
          CALL ERRMSG('MZCOPY', 'XYRCP', Message, 'W')
          LBMXY = 0
        ENDIF
      ELSEIF(Run .LT. FirstDBRun) THEN
        IF(LBMXY .GT. 0) THEN
          CALL MZDROP(IXSTP, LBMXY, ' ')
          LBMXY = 0
        ENDIF
        CALL VXY_BEAM1(0.0, BPUncorr(1), Dummy, BPUncorr(2), Dummy,
     &                 IER)
        IF(IER .LT. HoursStale) THEN
C--
C Construct BMXY bank
C--
          CALL MZBOOK(IDVSTP, LBMXY, LBMXY, 1, 'BMXY', 0, 0, 23,
     &                FormIndex, -1)
          CALL VXY_AUX(ARun, IC(LBMXY + 10), C(LBMXY + 6),
     &      C(LBMXY + 7), C(LBMXY + 4), C(LBMXY + 5))
C--
C The "store" will be the negative of the run number for which information
C was present
C--
          IC(LBMXY + 1) = -ARun
          C(LBMXY + 2) = BPUncorr(1)
          C(LBMXY + 3) = BPUncorr(2)
          C(LBMXY + 8) = 0
          C(LBMXY + 9) = 0
          C(LBMXY + 11) = 0
          C(LBMXY + 12) = 1
          DO 100 I = 2, 12
            C(LBMXY + I + 11) = C(LBMXY + I)
  100     CONTINUE
        ENDIF
      ELSEIF(.NOT. BMXY_Read(Run)) THEN
        IF(.NOT. DBBypass) CALL ERRMSG('Run summary database',
     &    'XYRCP', 'Problem; crashing...', 'F')
        IF(LBMXY .GT. 0) THEN
          CALL MZDROP(IXSTP, LBMXY, ' ')
          LBMXY = 0
        ENDIF
      ENDIF
      CALL EZPICK('VERTEX_RCP')
      IF(LBMXY .GT. 0) THEN
C
C ****  OVERWRITE RCP BANK!  First, "uncorrect" beam position so that it's
C ****  no longer at Z = 0 (the errors are calculated at the uncorrected Z)
C
        BPUncorr(1) = C(LBMXY + 13) + C(LBMXY + 15) * C(LBMXY + 22)
        BPUncorr(2) = C(LBMXY + 14) + C(LBMXY + 16) * C(LBMXY + 22)
        CALL EZSETA('BEAM_POS', 1, 2, 1, BPUncorr, IER)
        IF(IER .EQ. 0) THEN
          BPErr(1) = SQRT(C(LBMXY + 17)**2 + IPSigma(1)**2)
          BPErr(2) = SQRT(C(LBMXY + 18)**2 + IPSigma(2)**2)
          CALL EZSETA('BEAM_ERR', 1, 2, 1, BPErr, IER)
          IF(IER .NE. 0) CALL ERRMSG('VERTEX_RCP', 'XYRCP',
     &      'Error overwriting BEAM_ERR', 'W')
        ELSE
          CALL ERRMSG('VERTEX_RCP','XYRCP','Error overwriting BEAM_POS',
     &                'W')
        ENDIF
      ELSE                                         ! Restore to original values
        CALL EZSETA('BEAM_POS', 1, 2, 1, RCP_BEAM_POS, IER)
        CALL EZSETA('BEAM_ERR', 1, 2, 1, RCP_BEAM_ERR, IER)
      ENDIF
      CALL EZRSET
  999 END

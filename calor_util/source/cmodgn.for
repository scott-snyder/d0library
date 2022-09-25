      SUBROUTINE CMODGN(IRUN,FBKCAP,GAIN,ICRATE,IADC,IBLS,ITWR,IDEP,
     &  CGAIN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Used to do pulser corrections for runs where the
C-                         pulser corrections where not already done in calib
C-
C-   Inputs  : IRUN - run number
C-             FBKCAP - feedback capacitance for this channel
C-             GAIN - gain value from calib
C-             ICRATE - ADC crate
C-             IADC - ADC card
C-             IBLS - BLS card
C-             ITWR - BLS tower
C-             IDEP - BLS depth
C-   Outputs : CGAIN - corrected gain value
C-   Controls: none
C-
C-   Created   9-FEB-1994   Jan Guida
C-   Updated  19-FEB-1994   Jan Guida  Modify pulser 4 correction factor
C-                                        Now a slope rather than a constant 
C-   Updated  24-FEB-1994   Jan Guida  Set PLSCOR logical to flag that pulser 
C-                                       modifications have been done
C-                                       Will set bit in CAEP header.
C-   Updated   8-AUG-1995   Jan Guida  Change run number where calib bug
C-                                      occurred (described below).  
C-                                      Should be run 70779 not 77079.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_PULSE_LIST.PARAMS'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CUNFLG.INC'
      REAL GAIN,CGAIN
      REAL CORRECT,RESCOR_ICD,RES_COR
      REAL TM_INT,TM_SLOP,TMINT_5PF,TMSLOP_5PF,TMINT_10PF,TMSLOP_10PF
      REAL TRACE_COR,TCORRECT,TRACE_SLOPE_SHORT
      REAL TRACE_INT_LONG,TRACE_SLOPE_LONG,TRACE_INT_SHORT
      REAL PBOX_COR(0:11),PLS_COR
      REAL P4COR_INT,P4COR_SLOPE
      INTEGER IRUN,FBKCAP,IERR,CELL_CAP
      INTEGER ICRATE,IADC,IBLS,ITWR,IDEP,ICHAN
      INTEGER IBOX,IPRBRD,IPRTWR,IPRDEP
      INTEGER DATE,MON,DAY,YEAR,KDAY,I,IM,DAYS,CALENDAR(12)
      LOGICAL LFIRST
      CHARACTER*80 MSG
      DATA LFIRST/.TRUE./
C----------------------------------------------------------------------
C
      PLSCOR = .TRUE.   ! Set flag if gains modifications have been performed
      CGAIN = 0.
C
      IF (LFIRST) THEN
        CALL EZPICK('CAHITS_RCP')
        CALL EZGET('TM_INT_5PF',TMINT_5PF,IERR) ! Intercept for timing cor - 5pf
        CALL EZGET('TM_INT_10PF',TMINT_10PF,IERR) ! Intercept for tim cor - 10pf
        CALL EZGET('TM_SLOPE_5PF',TMSLOP_5PF,IERR) ! Slope for timing cor - 5pf
        CALL EZGET('TM_SLOPE_10PF',TMSLOP_10PF,IERR) ! Slope for tim cor - 10pf
        CALL EZGET('TRACE_INT_LONG',TRACE_INT_LONG,IERR) ! Int for trace cor
        CALL EZGET('TRACE_SLOPE_LONG',TRACE_SLOPE_LONG,IERR) ! Int for trace 
        CALL EZGET('TRACE_INT_SHORT',TRACE_INT_SHORT,IERR) ! Slope for trace 
        CALL EZGET('TRACE_SLOPE_SHORT',TRACE_SLOPE_SHORT,IERR) ! Slope trace 
        CALL EZGET_rarr('PBOX_COR',PBOX_COR,IERR) ! relative pulser cor factors
        CALL EZGET('P4COR_INT',P4COR_INT,IERR) ! pulser 4, cor factors - int
        CALL EZGET('P4COR_SLOPE',P4COR_SLOPE,IERR) ! pls 4, cor factors - slope
        CALL EZRSET
        RESCOR_ICD = (499./100.)/4.
        CALENDAR(1)=31
        CALENDAR(2)=28
        CALENDAR(3)=31
        CALENDAR(4)=30
        CALENDAR(5)=31
        CALENDAR(6)=30
        CALENDAR(7)=31
        CALENDAR(8)=31
        CALENDAR(9)=30
        CALENDAR(10)=31
        CALENDAR(11)=30
        CALENDAR(12)=31
        LFIRST = .FALSE.
      ENDIF
C
      ICHAN = IADC*384 + IBLS*48 + ITWR*12 + IDEP + 1
      CALL GTCCPT(ICRATE,ICHAN,CELL_CAP,IERR)
      IF (IERR.NE.0) THEN
        WRITE(MSG,27)ICRATE,ICHAN,IERR
   27   FORMAT(' unknown detector cap for crate ',I3,' channel ',I7,
     &    ' error = ',I4)
        CALL ERRMSG('NO_CAP_VALUE','CMODGN',MSG,'W')
        CELL_CAP = 0
      ENDIF
C
      RES_COR = 1.                    ! done in calib
      IF(FBKCAP.EQ.5) THEN
        TM_INT = TMINT_5PF
        TM_SLOP = TMSLOP_5PF
      ELSEIF(FBKCAP.EQ.10) THEN
        TM_INT = TMINT_10PF
        TM_SLOP = TMSLOP_10PF
      ELSEIF(FBKCAP.EQ.22) THEN
        IF (IRUN.GE.70779) THEN         ! fix calib bug, did not put in 
          RES_COR = RESCOR_ICD          ! resistor factor
        ENDIF
        TM_INT = 1.
        TM_SLOP = 0.
      ELSE
        WRITE(MSG,25)ICRATE,ICHAN,FBKCAP
   25   FORMAT(' unknown fbk cap for crate ',I3,' channel ',I7,
     &    ' value = ',I4)
        CALL ERRMSG('NO_CAP_VALUE','CMODGN',MSG,'W')
        RES_COR = 1.
        TM_INT = 1.
        TM_SLOP = 0.
      ENDIF
C
      IF (IRUN.GE.66932) THEN
        PBOX_COR(4) = 0.987
      ELSE
        DATE = IC(LCGNH+7)
        MON = DATE/10000
        DAY = (DATE-MON*10000)/100
        YEAR = MOD(DATE,100)
        KDAY = -1                ! Starting date is January 1, 1992, day 0
        DO I = 92, YEAR - 1
          KDAY = KDAY + 365
          IF (MOD(I,4).EQ.0) KDAY = KDAY + 1       ! Check for leap year
        ENDDO
        DO IM = 1, MON - 1
          KDAY = KDAY + CALENDAR(IM)
          IF (MON.EQ.2) THEN       ! Check for leap year
            IF (MOD(I,4).EQ.0) KDAY = KDAY + 1
          ENDIF
        ENDDO
        DAYS = KDAY + DAY
C
        IF (DAYS.GT.500) THEN           ! Constant after this day
          PBOX_COR(4) = 1.035
        ELSE                            ! Pulser drifted with time
          PBOX_COR(4) = P4COR_SLOPE*DAYS + P4COR_INT
        ENDIF
      ENDIF
C
C   Find preamp box (or pulser box) and preamp tower
      CALL CADPR(PP_QUART,ICRATE,IADC,IBLS,ITWR,IDEP,
     &              IBOX,IPRBRD,IPRTWR,IPRDEP,IERR)
C
      TRACE_COR = 1.
      PLS_COR = 1.
      IF ((IERR.EQ.0) .AND. IBOX.GE.0) THEN           ! Connected channels
C
C   Short/long preamp board trace corrections
        IF (IPRTWR.LT.2) THEN       ! Short trace correction
          TRACE_COR = TRACE_SLOPE_SHORT*CELL_CAP + TRACE_INT_SHORT
        ELSE
          TRACE_COR = TRACE_SLOPE_LONG*CELL_CAP + TRACE_INT_LONG
        ENDIF
C
C   Relative pulser correction factor  (normalize all pulsers to CCNW)
        PLS_COR = PBOX_COR(IBOX)
      ELSEIF (IERR.NE.2) THEN       ! Error  (IERR=2 for ICD)
        WRITE(MSG,26)IERR,ICRATE,ICHAN
   26   FORMAT(' Error ',I4,' returned from CADPR for crate ',I2,
     &    ' channel ',I7)
        CALL ERRMSG('CADPR error','CMODGN',MSG,'W')
      ENDIF
C
      CORRECT = TRACE_COR/PLS_COR/RES_COR
C
C   Timing corrections  -  "EM has faster rise time than FH"
      TCORRECT = TM_SLOP*CELL_CAP + TM_INT
C
C   Correct the gain value
C
      CGAIN = GAIN*TCORRECT*CORRECT
C
  999 RETURN
      END

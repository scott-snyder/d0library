      FUNCTION WZ_EVENT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Main routine for WZ ANALYSIS
C-
C-   Returned value  : True if OK
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   2-OCT-1990   Rajendran Raja
C-   Updated  22-JAN-1992   Norman A. Graf
C-   Updated  22-SEP-1992   Cecilia E. Gerber
C-   Updated  15-JAN-1993   Ulrich Heintz  add E_GOOD_RUN
C-   Updated  25-JAN-1993   Cecilia E. Gerber/Ulrich Heintz  call filter code
C-   Updated   8-FEB-1993   Ulrich Heintz check word 10 in HEAD bank for filter
C-   Updated   3-JUN-1993   Cecilia Gerber: add MU_GOOD_RUN
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL WZ_EVENT
      LOGICAL FIRST
      LOGICAL DO_ANLWENU,DO_ANLZEE,DO_ANLWMUNU,DO_ANLZMUMU,E_GOOD_RUN
      LOGICAL MU_GOOD_RUN,IGNORE_FILTER
      INTEGER IER,JBIT
      EXTERNAL E_GOOD_RUN
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN                     ! LOCAL INIT
        FIRST = .FALSE.
        CALL EZPICK('WZ_RCP')
        CALL EZGET('ANALYZE_W_ENU',DO_ANLWENU,IER)
        CALL EZGET('ANALYZE_Z_EE',DO_ANLZEE,IER)
        CALL EZGET('ANALYZE_W_MUNU',DO_ANLWMUNU,IER)
        CALL EZGET('ANALYZE_Z_MUMU',DO_ANLZMUMU,IER)
        CALL EZGET('IGNORE_FILTER',IGNORE_FILTER,IER)
        CALL EZRSET
      ENDIF
C
      CALL FLGSET('WRITE_STREAM_STA',.FALSE.)
      CALL FLGSET('WRITE_STREAM_WEV',.FALSE.)
      CALL FLGSET('WRITE_STREAM_ZEE',.FALSE.)
      IF(E_GOOD_RUN())THEN
        IF(JBIT(IQ(LHEAD+10),7).NE.0.OR.IGNORE_FILTER)THEN
          IF(DO_ANLWENU)CALL ANLWENU        ! ANALYZE W -> e nu
        ENDIF
        IF(JBIT(IQ(LHEAD+10),8).NE.0.OR.IGNORE_FILTER)THEN
          IF(DO_ANLZEE)CALL ANLZEE          ! ANALYZE Z -> e e
        ENDIF
      ENDIF
      CALL FLGSET('WRITE_STREAM_WMU',.FALSE.)       ! WMU stream
      CALL FLGSET('WRITE_STREAM_ZMU',.FALSE.)       ! ZMU stream
      IF(MU_GOOD_RUN()) THEN
        IF(JBIT(IQ(LHEAD+10),9).NE.0.OR.IGNORE_FILTER)THEN
          IF(DO_ANLWMUNU) CALL ANLWMUNU      ! ANALYZE W -> mu nu
        ENDIF
        IF(JBIT(IQ(LHEAD+10),10).NE.0.OR.IGNORE_FILTER)THEN
          IF(DO_ANLZMUMU) CALL ANLZMUMU      ! ANALYZE Z -> mu mu
        ENDIF
      ENDIF
C
      WZ_EVENT = .TRUE.
  999 RETURN
      END

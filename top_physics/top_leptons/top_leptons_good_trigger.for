      LOGICAL FUNCTION TOP_LEPTONS_GOOD_TRIGGER()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Tests L1 trigger bits and L2 filter bits
C-                         set for current event and tests against
C-                         a user-defined selection list. If the event
C-                         passes one (or more) of the selected triggers
C-                         then the routine returns as .TRUE.. 
C-                         Also tests against micro_blanking - if requested
C-                         and requires no veto for a .TRUE. return.
C-
C-   Inputs  :  
C-               None
C-   Outputs : 
C-               GOOD_TRIGGER = .TRUE./.FALSE. for OK/Reject Event
C-
C-   Controls: 
C-               None
C-
C-   Created   1-NOV-1992   Stephen J. Wimpenny
C-   Modified 26-Jan-1993   Micro_Blank test added
C-   Modified 22-Feb-1993   Error in MicroBlank logic fixed
C-   Modified 16-Mar-1993   Routine name changed for library compatibility
C-   Modified 22-Mar-1993   Routine name change Which_Trig_Bits,
C-                          Test_Micro_Blank, Test_Mrbs
C-   Modified  4-Dec-1993   Selection on Filter Names added as an option
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      LOGICAL DO_TRIG_TEST,DO_TRIGFAIL_SELECT,FIRST
C##
      LOGICAL DO_BIT_TEST,DO_FILTNAME_TEST,FILT_PASS,L2NAME_PASSED
      CHARACTER*32 FILT_LIST(128)
      INTEGER NO_FILTS,ILEN
C##
      LOGICAL DO_MICRO_BLANK_CUT,TOP_LEPTONS_UTIL_MICRO_BLANK
      LOGICAL DO_MRBS_LOSS_CUT,TOP_LEPTONS_UTIL_MRBS_LOSS
      INTEGER IER,I,J
      INTEGER NO_L1_SET,NO_L2_SET,L1_BITS_SET(32),L2_BITS_SET(64)
      INTEGER NO_L1_SELECTED,NO_L2_SELECTED,L1_BITS_SELECTED(32)
      INTEGER L2_BITS_SELECTED(64)
C
      DATA FIRST/.TRUE./
C
      IF(FIRST) THEN
C
C *** Read Trigger test information from RCP file
C
        CALL EZPICK('TOP_LEPTONS_RCP')
        CALL EZGET('DO_TRIGGER_SELECT',DO_TRIG_TEST,IER)
        IF(DO_TRIG_TEST) THEN
          IF(IER.EQ.0) 
     1      CALL EZGET('DO_TRIGFAIL_SELECT',DO_TRIGFAIL_SELECT,IER)
C##
          IF(IER.EQ.0) CALL EZGET('DO_BIT_SELECT',DO_BIT_TEST,IER)
          IF(DO_BIT_TEST)THEN
C##
            IF(IER.EQ.0) CALL EZGETA('L1_BITS',0,0,0,NO_L1_SELECTED,IER)
            IF(IER.EQ.0) CALL EZGET('L1_BITS',L1_BITS_SELECTED,IER)
            IF(IER.EQ.0)
     1         CALL EZGETA('L2_FILTERS',0,0,0,NO_L2_SELECTED,IER)
            IF(IER.EQ.0) CALL EZGET('L2_FILTERS',L2_BITS_SELECTED,IER)
C###
          ENDIF
          IF(IER.EQ.0) CALL EZGET('DO_FILTERNAME_SELECT',
     1                            DO_FILTNAME_TEST,IER)
          IF(DO_FILTNAME_TEST)THEN
            IF(IER.EQ.0) CALL EZGET_NUMBER_STRINGS('FILTER_LIST',
     1                                            NO_FILTS,IER)
            DO I = 1,NO_FILTS
              IF(IER.EQ.0) CALL EZGETS('FILTER_LIST',I,
     1                                 FILT_LIST(I),ILEN,IER)
            END DO
          ENDIF
C###
        ENDIF
        IF(IER.EQ.0) CALL EZGET('DO_MICRO_BLANK_CUT',
     1    DO_MICRO_BLANK_CUT,IER)
        IF(IER.EQ.0) CALL EZGET('DO_MRBS_LOSS_CUT',DO_MRBS_LOSS_CUT,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error in TOP_LEPTONS_RCP',
     &    'TOP_LEPTONS_GOOD_TRIGGER',' ','F')
        FIRST=.FALSE.
      ENDIF
C
      TOP_LEPTONS_GOOD_TRIGGER=.TRUE.
      IF(.NOT.DO_TRIG_TEST) GO TO 100
C
C *** Trigger selection wanted so look at what is set for this event
C
      CALL TOP_LEPTONS_UTIL_DECODE_TRIG(NO_L1_SET,L1_BITS_SET,NO_L2_SET,
     1  L2_BITS_SET,IER)
C
      IF(NO_L2_SELECTED.LT.1) GO TO 10      
      IF(NO_L2_SET.LT.1) GO TO 20
C
C *** Test on L2 Filter Bits 
C
      DO I=1,NO_L2_SELECTED
        DO J=1,NO_L2_SET
          IF(L2_BITS_SELECTED(I).EQ.L2_BITS_SET(J)) THEN
            GO TO 10
          ENDIF
        ENDDO
      ENDDO
      GO TO 20
   10 CONTINUE
C
      IF(NO_L1_SELECTED.LT.1) GO TO 100   !Shouldn't this be 100?
C      IF(NO_L1_SELECTED.LT.1) GO TO 999   !Shouldn't this be 100?
      IF(NO_L1_SET.LT.1) GO TO 20
C
C *** Test on L1 Trigger Bits
C
      DO I=1,NO_L1_SELECTED
        DO J=1,NO_L1_SET
          IF(L1_BITS_SELECTED(I).EQ.L1_BITS_SET(J)) THEN
            GO TO 100
          ENDIF
        ENDDO
      ENDDO
      GO TO 20
  100 CONTINUE
C####
C
C *** Test on Filter bits if requested
C
      IF(.NOT.DO_FILTNAME_TEST) GO TO 200
      IF(NO_FILTS.LT.1) GO TO 200
      DO I = 1,NO_FILTS
        FILT_PASS = L2NAME_PASSED(FILT_LIST(I))
        IF(FILT_PASS) GO TO 200
      ENDDO
      GO TO 20
  200 CONTINUE
C####
C
C *** Test for MRBS loss
C
      IF(DO_MRBS_LOSS_CUT) THEN
        IF(TOP_LEPTONS_UTIL_MRBS_LOSS()) THEN
          GO TO 20
        ELSE
          GO TO 25
        ENDIF
      ENDIF
   25 CONTINUE
C
C *** Test for micro_blanking flag
C
      IF(DO_MICRO_BLANK_CUT) THEN
        IF(TOP_LEPTONS_UTIL_MICRO_BLANK()) THEN
          GO TO 20
        ELSE
          GO TO 999
        ENDIF
      ELSE
        GO TO 999
      ENDIF
C
C *** We failed to find a match and/or filed micro_blank cut so Zap event !
C
   20 TOP_LEPTONS_GOOD_TRIGGER=.FALSE.
C----------------------------------------------------------------------
  999 CONTINUE
      IF(DO_TRIGFAIL_SELECT) THEN
        IF(.NOT.TOP_LEPTONS_GOOD_TRIGGER) THEN
          TOP_LEPTONS_GOOD_TRIGGER=.TRUE.
        ELSE
          TOP_LEPTONS_GOOD_TRIGGER=.FALSE.
        ENDIF
      ENDIF
      RETURN
      END

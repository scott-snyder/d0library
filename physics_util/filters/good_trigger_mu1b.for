      LOGICAL FUNCTION GOOD_TRIGGER_MU1B()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Tests L1 trigger bits and L2 filter bits
C-                         set for current event and tests against
C-                         a user-defined selection list. If the event
C-                         passes one (or more) of the selected triggers
C-                         then the routine returns as .TRUE.. 
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
C-   Modified  17-DEC-92    Thorsten Huehn  minor adapt for b_physics
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      LOGICAL DO_TRIG_TEST,FIRST
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
        CALL EZPICK('MU1_B_RCP')
        CALL EZGET('CHECK_TRIGGER_BITS_MU1B',DO_TRIG_TEST,IER)
        IF(IER.NE.0) STOP ' CHECK_TRIGGER_BITS_MU1B not found'
        IF(DO_TRIG_TEST) THEN
          IF(IER.EQ.0) CALL EZGETA('L1_BITS_MU1B',0,0,0,
     1            NO_L1_SELECTED,IER)
          IF(IER.EQ.0) CALL EZGET('L1_BITS_MU1B',L1_BITS_SELECTED,IER)
          IF(IER.EQ.0)
     1       CALL EZGETA('L2_FILTERS_MU1B',0,0,0,NO_L2_SELECTED,IER)
          IF(IER.EQ.0) CALL EZGET('L2_FILTERS_MU1B',
     1                                           L2_BITS_SELECTED,IER)
        ENDIF
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error in MU1_B_RCP',
     &    'GOOD_TRIGGER',' ','F')
        FIRST=.FALSE.
      ENDIF
C
      GOOD_TRIGGER_MU1B=.TRUE.
      IF(.NOT.DO_TRIG_TEST) GO TO 999
C
C *** Trigger selection wanted so look at what is set for this event
C
      CALL WHICH_TRIG_BITS(NO_L1_SET,L1_BITS_SET,NO_L2_SET,
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
      IF(NO_L1_SELECTED.LT.1) GO TO 999
      IF(NO_L1_SET.LT.1) GO TO 20
C
C *** Test on L1 Trigger Bits
C
      DO I=1,NO_L1_SELECTED
        DO J=1,NO_L1_SET
          IF(L1_BITS_SELECTED(I).EQ.L1_BITS_SET(J)) THEN
            GO TO 999
          ENDIF
        ENDDO
      ENDDO
C
C *** We failed to find a match so Zap event !
C
   20 GOOD_TRIGGER_MU1B=.FALSE.
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END

      SUBROUTINE L15_CAL_SIM ( NUM_L15_TERM_USED, L15_TERM_STATES,
     &  L15_TERM_INDICES)
C----------------------------------------------------------------------
C-
C-      The logical names of the Andor Terms this routine retrieves are
C-      'L15C_CT_0_TRM_n' with n ..15
C-
C-   Inputs  : none
C-   Outputs : NUM_L15_TERM_USED   The number of LEVEL 1.5 Terms this routine
C-                                 is returning. This number may be 0.
C-             L15_TERM_STATES     The states of returned Andor Terms.
C-             L15_TERM_INDICES    The corresponding Andor Term number for
C-                                 each returned Andor Term.
C-   Controls: none
C-
C-   Created   24-MAR-1994   sFahey  Used L15_EXAMPLE_SIM as a
C-                                   template for Cal L15
C-
C--------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L15COOR_PARSER.PARAMS'
C
      INTEGER NUM_L15_TERM_USED
      LOGICAL L15_TERM_STATES(32)
      INTEGER L15_TERM_INDICES(32)
C
C       This is a LOGICAL function provided by L1SIM which selects the
C       correct RCP bank to retrieve Level 1.5 Term numbers.
C
      LOGICAL  L1UTIL_PICK_L15RESOURCE_RCP
      EXTERNAL L1UTIL_PICK_L15RESOURCE_RCP
C
      INTEGER IER
C
      CHARACTER*20 L15_CAL_TERM_NAME
      INTEGER NUM_L15_CAL_USED
      LOGICAL L15_CAL_STATES(16)
      INTEGER L15_CAL_INDICES(16)
      INTEGER ITERM
      LOGICAL L15CAL_EM_SIM
      LOGICAL TERM_ON,L15_CAL_TERM_ON
      LOGICAL FIRST
      SAVE FIRST, NUM_L15_CAL_USED, IER
      DATA FIRST / .TRUE. /
C
C       Find the Level 1.5 Term numbers for the Level 1.5 Cal trigger
C       for the first call only
C
      IF (FIRST .EQV. .TRUE.) THEN
        FIRST = .FALSE.
        IER = 0
C
C       The function L1UTIL_PICK_L15RESOURCE_RCP calls EZPICK with the
C         correct bank name
C
        IF ( L1UTIL_PICK_L15RESOURCE_RCP() .EQV. .FALSE.) THEN
C
C       Handle the error case where the RCP bank does not exist
C
          IER = 1   ! Indicate that the Andor Term was not defined
C
        ELSE
C
C       Get the correct term number
C
          NUM_L15_CAL_USED = 0
          DO ITERM = L15TM_NUM_MIN+1 , L15TM_NUM_MAX+1
C
            IF ( ITERM .LE. 10 ) THEN
              WRITE ( L15_CAL_TERM_NAME, 100 ) ( ITERM - 1 )
  100         FORMAT ( 'L15C_CT_0_TRM_', I1 )
            ELSE
              WRITE ( L15_CAL_TERM_NAME, 200 ) ( ITERM - 1)
  200         FORMAT ( 'L15C_CT_0_TRM_', I2 )
            END IF
C
            CALL EZGET( L15_CAL_TERM_NAME,
     &                  L15_CAL_INDICES(ITERM), IER)
            IF ( IER .EQ. 0 ) THEN
              NUM_L15_CAL_USED = NUM_L15_CAL_USED + 1
            ELSE
              L15_CAL_INDICES(ITERM) = -1
            END IF
C
          ENDDO
C
          IF ( NUM_L15_CAL_USED .EQ. 0 ) THEN
            IER = -1 ! Indicate that no L1.5 Term was found
          ELSE
            IER = 0 ! Indicate successful initialization
          END IF
C
C       De-select the current RCP bank.
C
          CALL EZRSET()
        ENDIF
      ENDIF
C
C       Handle the case where the term number was not specified in the
C         RCP file
C
      IF (IER .NE. 0) THEN
        NUM_L15_CAL_USED = 0   ! The L15_term Terms were not specified
        GOTO 999               !  in the RCP file
      END IF
C
C
C     Calculate the term states for terms that are ON (i.e. need to be
C     evaluated for this event.  The Level 1.5 Calorimeter trigger only
C     runs on terms that need it.
C
      DO ITERM = 1, NUM_L15_CAL_USED
        TERM_ON = L15_CAL_TERM_ON(L15_CAL_INDICES(ITERM))
        IF (TERM_ON) THEN
          L15_CAL_STATES(ITERM) = L15CAL_EM_SIM(ITERM-1) 
C                                                    ^
C                   L15cal terms start at 0 not 1 ---^
        ELSE
          L15_CAL_STATES(ITERM) = .FALSE.
        ENDIF
C
C     Convert L15CAL states and indices to L15 states and indices
C
        L15_TERM_STATES(ITERM) = L15_CAL_STATES(ITERM)
        L15_TERM_INDICES(ITERM) = L15_CAL_INDICES(ITERM)
C
      ENDDO
      NUM_L15_TERM_USED = NUM_L15_CAL_USED
C
C   Fill frame code section of L15CAL data block
C
      CALL L15C_FILL_FRAME_CODE_BLOCK
C
C   Fill DEBUG section of common block
C Note that for now am calling with event type set to 1 (mark and pass) for all
C events.  eventually will need to control event type either by pass one of 1
C scaler or, if running on real data can control by hardware event type.
      CALL L15C_FILL_DEBUG_SECTION(0)
C
  999 RETURN
      END

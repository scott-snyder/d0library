      SUBROUTINE L15_MUON_SIM ( NUM_L15_TERM_USED, L15_TERM_STATES,
     &  L15_TERM_INDICES)
C----------------------------------------------------------------------
C-
C-      The logical names of the Andor Terms this routine retrieves are
C-      'L1M_MUSECTION_CMP_n' with n ..15
C-
C-   Inputs  : none
C-   Outputs : NUM_L15_TERM_USED   The number of LEVEL 1.5 Terms this routine
C-                                 is returning. This number may be 0.
C-             L15_TERM_STATES     The states of returned Andor Terms.
C-             L15_TERM_INDICES    The corresponding Andor Term number for 
C-                                 each returned Andor Term.
C-   Controls: none
C-
C-   Created   3-SEP-1991 MICHIGAN STATE UNIVERSITY, TRIGGER CONTROL SOFTWARE
C-   Modified for the level 1.5 Muon Trigger Simulator, Kamel Bazizi, 11-16-91
C-   Updated  13-DEC-1991   Philippe Laurens, Steven Klocek   
C-              update the name of the Level 1.5 terms
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER NUM_L15_TERM_USED
      LOGICAL L15_TERM_STATES(32)
      INTEGER L15_TERM_INDICES(32)
C
C       This is a LOGICAL function provided by L1SIM which selects the correct
C       RCP bank to retrieve Level 1.5 Term numbers.
C
      LOGICAL  L1UTIL_PICK_L15RESOURCE_RCP
      EXTERNAL L1UTIL_PICK_L15RESOURCE_RCP
C
      INTEGER IER
C
      CHARACTER*20 L15_MUON_TERM_NAME
      INTEGER L15_MUON_TERM_NUM(16)
      LOGICAL L15_MUON_BITS(16)
      INTEGER ITERM
      LOGICAL FIRST
      SAVE FIRST, L15_MUON_TERM_NUM, IER
      DATA FIRST / .TRUE. /
C
C       Find the Level 1.5 Term numbers for the Level 1.5 muon trigger
C       for the first call only
C
      IF (FIRST .EQV. .TRUE.) THEN
        FIRST = .FALSE.
        IER = 0   
C
C       The function L1UTIL_PICK_L15RESOURCE_RCP calls EZPICK with the correct 
C         bank name
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
          NUM_L15_TERM_USED = 0 
          DO ITERM = 1, 16
C
            IF ( ITERM .LE. 10 ) THEN 
              WRITE ( L15_MUON_TERM_NAME, 100 ) ( ITERM - 1)
  100         FORMAT ( 'L15M_MUPT_CMP_', I1 )
            ELSE 
              WRITE ( L15_MUON_TERM_NAME, 200 ) ( ITERM - 1 ) 
  200         FORMAT ( 'L15M_MUPT_CMP_', I2 )
            END IF
C
            CALL EZGET( L15_MUON_TERM_NAME, 
     &                  L15_MUON_TERM_NUM(ITERM), IER)
            IF ( IER .EQ. 0 ) THEN 
              NUM_L15_TERM_USED = NUM_L15_TERM_USED + 1
            ELSE
              L15_MUON_TERM_NUM(ITERM) = -1
            END IF
C
          ENDDO
C
          IF ( NUM_L15_TERM_USED .EQ. 0 ) THEN
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
        NUM_L15_TERM_USED = 0  ! The L15_term Terms were not specified in the RCP file
        GOTO 999
      END IF
C
C       -----------------------------------------------------------
C       At this point, calculate the Level 1.5 Term states by a CALL
C       to the ENTRY point L15_MUON which calculates an array of 
C       16  logicals describing the STATES of the L1.5 muon TERM 
C       terms and assign the return  LOGICAL values.
C       -----------------------------------------------------------
C
      CALL MU_L15_PHYS_BITS(L15_MUON_BITS)
C
      NUM_L15_TERM_USED = 0 
      DO ITERM = 1,16
        IF ( L15_MUON_TERM_NUM(ITERM) .GE. 0 ) THEN 
          NUM_L15_TERM_USED = NUM_L15_TERM_USED + 1
          L15_TERM_STATES( NUM_L15_TERM_USED )= L15_MUON_BITS(ITERM)
          L15_TERM_INDICES( NUM_L15_TERM_USED)= L15_MUON_TERM_NUM(ITERM)
        ENDIF
      ENDDO
C
C----------------------------------------------------------------------
  999 RETURN
      END

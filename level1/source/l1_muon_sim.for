      SUBROUTINE L1_MUON_SIM ( NUM_ANDOR_USED, ANDOR_STATES,
     &  ANDOR_INDICES)
C----------------------------------------------------------------------
C-
C-      The logical names of the Andor Terms this routine retrieves are
C-      'L1M_MUCOUNT_CMP_n' with n ..15
C-
C-   Inputs  : none
C-   Outputs : NUM_ANDOR_USED   The number of Andor Terms this routine is
C-                              returning. This number may be 0.
C-             ANDOR_STATES     The states of returned Andor Terms.
C-             ANDOR_INDICES    The corresponding Andor Term number for
C-                              each returned Andor Term.
C-   Controls: none
C-
C-   Created   3-SEP-1991 MICHIGAN STATE UNIVERSITY, TRIGGER CONTROL SOFTWARE
C-   Updated  09-NOV-1991   Kamel Bazizi
C-                          Modified for the Muon Trigger Simulator
C-   Updated  12-DEC-1991   Philippe Laurens, Steven Klocek
C-                          modify the Muon andor term names, and skip
C-                          simulation when no andor terms has been defined.
C-   Updated  26-MAY-1992   Kamel Bazizi
C-                          replace MUANLZ by a simpler routine, MU_HITS_UNPACK
C-   Updated  23-NOV-1992   Guilherme Lima
C-                          Remove call to MU_HITS_UNPACK
C-   Updated  18-JAN-1993   Kamel Bazizi
C-                          Minor array size change of TRGR_MUON_DATA
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER NUM_ANDOR_USED
      LOGICAL ANDOR_STATES(256)
      INTEGER ANDOR_INDICES(256),ierr
C
C       This is a LOGICAL function provided by L1SIM which selects the correct
C       RCP bank to retrieve Andor Term numbers.
C
      LOGICAL L1UTIL_PICK_RESOURCE_RCP
      EXTERNAL L1UTIL_PICK_RESOURCE_RCP
C
      INTEGER IER
C
      CHARACTER*20 L1_MUON_ANDOR_NAME
      INTEGER L1_MUON_ANDOR_NUM(16)
      LOGICAL L1_MUON_BITS(16),L15_MUON_BITS(16)
      INTEGER DELTA_WORDS,TRGR_MUON_DATA(500)
      INTEGER ITERM
      LOGICAL FIRST
C
      CHARACTER*32 MESSID,CALLER
      CHARACTER*80 MESSAG
C
      INTEGER LMUD1,GZMUD1
C
      SAVE FIRST, L1_MUON_ANDOR_NUM, IER
      DATA FIRST / .TRUE. /
C
C
C---------------------------------------------------------------
C       Find the Andor Term number for the Muon Level 1 Sub-system
C       on the first call only.
C
      IF (FIRST .EQV. .TRUE.) THEN
        FIRST = .FALSE.
        IER = 0

C-- Book Histograms
C
        CALL MUSIM_BOOK_L1
        CALL MUSIM_BOOK_L15

C       The function  L1UTIL_PICK_RESOURCE_RCP calls EZPICK with the correct
C         bank name
C
        IF ( L1UTIL_PICK_RESOURCE_RCP() .EQV. .FALSE.) THEN
C
C       Handle the error case where the RCP bank does not exist
C
          IER = 1   ! Indicate that the Andor Term was not defined
C
        ELSE
C
C       Get the correct term number
C
          NUM_ANDOR_USED = 0
          DO ITERM = 1, 16
C
            IF ( ITERM .LE. 10 ) THEN
              WRITE ( L1_MUON_ANDOR_NAME, 100 ) ( ITERM - 1)
  100         FORMAT ( 'L1M_MUCOUNT_CMP_', I1 )
            ELSE
              WRITE ( L1_MUON_ANDOR_NAME, 200 ) ( ITERM - 1 )
  200         FORMAT ( 'L1M_MUCOUNT_CMP_', I2 )
            END IF
C
            CALL EZGET( L1_MUON_ANDOR_NAME,
     &                  L1_MUON_ANDOR_NUM(ITERM), IER)
            IF ( IER .EQ. 0 ) THEN
              NUM_ANDOR_USED = NUM_ANDOR_USED + 1
            ELSE
              L1_MUON_ANDOR_NUM(ITERM) = -1
            END IF
C
          ENDDO
C
          IF ( NUM_ANDOR_USED .EQ. 0 ) THEN
            IER = -1 ! Indicate that no Andor term was found
          ELSE
            IER = 0 ! Indicate successful initialization
          END IF
C
C       De-select the current RCP bank.
C
          IF(IER.EQ.0)CALL EZRSET()
        ENDIF
      ENDIF
C
C       Handle the case where the term number was not specified in the
C         RCP file
C
      IF (IER .NE. 0) THEN
        NUM_ANDOR_USED = 0  ! The Andor Terms were not specified in the RCP file
        GOTO 999
      END IF
C
C---------------------------------------------------------------
C       At this point, calculate the Andor Term states by a CALL
C       to the MU_SUPERVISOR routine which returns with level 1,
C       level 1.5 muon trigger information but only the
C       LEVEL 1 Bits are passed at this stage
C---------------------------------------------------------------
C
C

      CALL MU_SUPERVISOR(L1_MUON_BITS,L15_MUON_BITS,DELTA_WORDS,
     &  TRGR_MUON_DATA,IERR)
C
C-- pass the level 1 bits and the corresponding ANDOR indices
C-- retrieved from the resources RCP file
C
C-- Fill Histograms
C
      CALL MUSIM_FILL_L1
      CALL MUSIM_FILL_L15

  800 CONTINUE
      NUM_ANDOR_USED = 0
      DO ITERM = 1,16
        IF ( L1_MUON_ANDOR_NUM(ITERM) .GE. 0 ) THEN
          NUM_ANDOR_USED = NUM_ANDOR_USED + 1
          ANDOR_STATES( NUM_ANDOR_USED )  = L1_MUON_BITS(ITERM)
          ANDOR_INDICES( NUM_ANDOR_USED ) = L1_MUON_ANDOR_NUM(ITERM)
        ENDIF
      ENDDO

C----------------------------------------------------------------------
C
   61 FORMAT('Error code=',I10,' ')
  999 RETURN
      END

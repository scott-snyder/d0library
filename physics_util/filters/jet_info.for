      SUBROUTINE JET_INFO ( ALG, TEMPLATE, WANTED, FOUND, INFO, OK )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find the set of jets corresponding to the jet-finder
C-   TEMPLATE, sorted in decreasing Et order; return up to the number WANTED
C-   jets in the array INFO; FOUND is the actual number of jets FOUND by the
C-   algorithm (not just the number returned).
C-
C-   Inputs  : ALG            - name of the jet finding algorithm wanted
C-             TEMPLATE       - array of control values to indicate the jet
C-                              finder which is wanted
C-             WANTED         - how many jets to return in the array INFO
C-   Outputs : FOUND          - the number of jets found by the algorithm
C-             INFO           - array (set of column vectors) containing the jet
C-                              information wanted.
C-                              INFO(1,i) - Et of jet i (scalar sum)
C-                              INFO(2,i) - Energy of jet i (scalar sum)
C-                              INFO(3,i) - eta of jet i
C-                              INFO(4,i) - phi of jet i
C-                              INFO(5,i) - RMS eta width of jet i
C-                              INFO(6,i) - RMS phi width of jet i
C-                              INFO(7,i) - EM Et fraction of jet i
C-                              INFO(8,i) - Pt of jet i (vector sum)
C-                              INFO(9,i) - P of jet i (vector sum)
C-                              INFO(10,i) - angle in phi between jet and
C-                                           missing Et vector.  Uses the ICD
C-                                           corrected missing Et (PNUT(2))
C-   Controls:
C-
C-   Created   6-DEC-1991   Marc Paterno
C-   Updated  15-OCT-1992   Marc Paterno  Added angle between jet and MEt
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C ****  Global variables
C
      INCLUDE  'D0$INC:ZEBCOM.INC'
      INTEGER   NWORDS
      PARAMETER (NWORDS = 10)
      CHARACTER*(*)  ALG
      INTEGER  WANTED, FOUND
      REAL     TEMPLATE(*), INFO(NWORDS, WANTED)
      LOGICAL  OK
C----------------------------------------------------------------------
C
C ****  Local variables
C
      INTEGER  IER, LJETS, GZJETS, LJTSH, GZJTSH, NZBANK, SORTBANKS
      INTEGER  I, LPNUT, GZPNUT
      EXTERNAL GZJETS, GZJTSH, NZBANK, SORTBANKS, GZPNUT
      REAL     PT, P, MPHI, PHI_DIFFERENCE
      EXTERNAL PHI_DIFFERENCE
C----------------------------------------------------------------------
      CALL SET_CAPH ( ALG(1:1), TEMPLATE, IER )
      IF ( IER .NE. 0 ) THEN
        OK = .FALSE.
        CALL ERRMSG ('SUSYUTIL', 'JET_INFO',
     &    'SET_CAPH failed to find the correct CAPH to match template',
     &    'W' )
        CALL RESET_CAPH
        RETURN
      ENDIF                            ! if ier .ne. 0

      CALL VZERO ( INFO, WANTED*NWORDS )
      FOUND = 0
      OK = .TRUE.

      LJETS = GZJETS()
      IF ( LJETS .LE. 0 )  THEN
        CALL RESET_CAPH
        RETURN
      ENDIF

      FOUND = NZBANK ( IXCOM, LJETS )
      LJETS = SORTBANKS ( LJETS, 6, 'D', 'F' )
      LPNUT = GZPNUT(2)
      IF ( LPNUT .LE. 0 ) THEN         ! no PNUT
        MPHI = 0.0
      ELSE
        MPHI = Q(LPNUT + 10)
      ENDIF                        ! if lpnut .le. 0y

      I = 0

      DO WHILE ( LJETS .GT. 0 )
        I = I + 1
        IF ( I .GT. WANTED ) GOTO 100   ! already have all we asked for
        INFO(1,I) = Q(LJETS + 6)
        INFO(2,I) = Q(LJETS + 5)
        INFO(3,I) = Q(LJETS + 9)
        INFO(4,I) = Q(LJETS + 8)
        PT = Q(LJETS+2)**2 + Q(LJETS+3)**2
        P  = PT + Q(LJETS+4)**2
        PT = SQRT(PT)
        P  = SQRT(P)
        INFO(8,I) = PT
        INFO(9,I) = P
        INFO(5,I)  = Q(LJETS + 12)
        INFO(6,I)  = Q(LJETS + 13)
        INFO(7,I) = Q(LJETS + 14)
        INFO(10, I) = PHI_DIFFERENCE (MPHI, Q(LJETS+8))
        LJETS = LQ(LJETS)
      ENDDO
  100 CONTINUE                          ! jump here after final jet

      CALL RESET_CAPH

      RETURN
      END

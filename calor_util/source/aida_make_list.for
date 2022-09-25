      SUBROUTINE AIDA_MAKE_LIST (ET_THRESH, LIST, OK, NFOUND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Runs over the CAEH bank and finds all cells that are
C-   candidates for removal; the requirement is that the cells have transverse
C-   energy greater than or equal to ET_THRESH.
C-
C-   Inputs  : ET_THRESH  [R] minimum ET for a cell to become a candidate
C-   Outputs : LIST       [I(AIDA_LIST_LENGTH)] list of PTCAEP pointers to
C-                               candidate hits.  If there are not enough
C-                               candidate hits to fill up the whole array, the
C-                               end is padded with zeros; if there are too many
C-                               to fit, the highest ET hits are kept.
C-             OK         [L] .TRUE. if successful, .FALSE. on error
C-             NFOUND     [I] number of candidates found (may be greater than
C-                            AIDA_LIST_LENGTH)
C-   Controls: none
C-
C-   Created  12-MAR-1993   Marc Paterno
C-   Updated   7-APR-1993   Marc Paterno  Added the output of NFOUND
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$INC:ZEBCOM.INC'
      INCLUDE  'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE  'D0$INC:PTCAEP.INC'
C----------------------------------------------------------------------
      REAL     ET_THRESH
      INTEGER  LIST(AIDA_LIST_LENGTH), NFOUND
      LOGICAL  OK
C----------------------------------------------------------------------
      INTEGER  LCAEH, GZCAEH, NR, NCH, I, PTR, IMIN, J
      EXTERNAL GZCAEH
      REAL     CURRENT_MIN, ETARRAY(AIDA_LIST_LENGTH), ETCELL
C----------------------------------------------------------------------
C
C ****  Reset for the new event
C
      NFOUND = 0
      CALL VZERO (ETARRAY, AIDA_LIST_LENGTH)
      CALL VZERO_i (LIST, AIDA_LIST_LENGTH)
C
C ****  Return with error status if there is no CAEH bank
C
      LCAEH = GZCAEH()
      IF ( LCAEH .LE. 0 ) THEN
        CALL ERRMSG ('No CAEH bank', 'AIDA_MAKE_LIST',
     &    'AIDA_MAKE_LIST aborting', 'W')
        OK = .FALSE.
      ENDIF
C
C ****  Loop over all hits in the CAEH bank, constructing a list of those with
C ****  ET greater than ET_THRESH
C
      NR = IQ(LCAEH+2)
      NCH = IQ(LCAEH+3)

      DO I = 0, NCH-1
        PTR = LCAEH + I*NR
        ETCELL = Q(PTR+8)

        IF ( ETCELL .GT. ET_THRESH ) THEN
          NFOUND = NFOUND + 1

          IF ( NFOUND .LE. AIDA_LIST_LENGTH ) THEN  ! just tag the new one on
C                                                   ! the end
            LIST(NFOUND) = I+1        ! number of the hit = PTCAEP pointer value
            ETARRAY(NFOUND) = ETCELL

          ELSE       ! array is full - replace lowest Et entry with a new one
            IMIN = 1
            CURRENT_MIN = ETARRAY(1)

            DO J = 2, AIDA_LIST_LENGTH
              IF ( ETARRAY(J) .LT. CURRENT_MIN ) THEN
                CURRENT_MIN = ETARRAY(J)
                IMIN = J
              ENDIF                       ! if etarray(j) .lt. current_min
            ENDDO                         ! j = 2, AIDA_LIST_LENGTH

            IF ( ETCELL .GT. CURRENT_MIN ) THEN
              LIST(IMIN) = I + 1
              ETARRAY(IMIN) = ETCELL
            ENDIF
          ENDIF                         ! if nfound .le. AIDA_LIST_LENGTH
        ENDIF                           ! if etcell .gt. et_thresh
      ENDDO                             ! i = 1, nch

      OK = .TRUE.

      RETURN
      END

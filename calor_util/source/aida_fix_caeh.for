      SUBROUTINE AIDA_FIX_CAEH (LIST, OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given a list of PTCAEP value pointers to "bad" hits
C-   in the CAEH, deflate their energy by multiplying by the small number
C-   HOTSUP.  The CAEP bank is not touched by this process.
C-
C-
C-   Inputs  : LIST   [I(*)] array of pointers to hits in the CAEP/H
C-   Outputs : OK     [L]    .TRUE. if successful, .FALSE. on error
C-   Controls: none
C-
C-   Created  18-MAR-1993   Marc Paterno
C-   Updated   7-APR-1993   Marc Paterno  Altered to NOT touch CAEP
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$INC:ZEBCOM.INC'
      INCLUDE  'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INTEGER   LIST(AIDA_LIST_LENGTH)
      LOGICAL   OK
C----------------------------------------------------------------------
      INTEGER  LCAEH, GZCAEH, PTR_CAEH, I
      INTEGER  NREP_CAEH, NHITS
C----------------------------------------------------------------------
C
C ****  Find the caeh
C
      LCAEH = GZCAEH()
      IF (LCAEH .LE. 0) THEN
        CALL ERRMSG ('AIDA_FIX_CAEH finds no CAEH bank',
     &               'AIDA_FIX_CAEH',
     &               'AIDA_FIX_CAEH aborting',
     &               'W' )
        OK = .FALSE.
        RETURN
      ENDIF

      NHITS     = IQ(LCAEH + 3)
      NREP_CAEH = IQ(LCAEH + 2)

C
C ****  Smash the bad hits: ET, EX, EY, EZ, ENERGY, and sigmas in CAEH
C
      DO I = 1, AIDA_LIST_LENGTH
        IF ( LIST(I) .EQ. 0 ) THEN
          CONTINUE
        ELSE IF ( LIST(I) .GT. 0 .AND. LIST(I) .LE. NHITS ) THEN

          PTR_CAEH = LCAEH + (LIST(I) - 1) * NREP_CAEH

          Q(PTR_CAEH+4) = Q(PTR_CAEH+4) * HOTSUP      ! suppress Ex
          Q(PTR_CAEH+5) = Q(PTR_CAEH+5) * HOTSUP      !  ... Ey
          Q(PTR_CAEH+6) = Q(PTR_CAEH+6) * HOTSUP      !  ... Ez
          Q(PTR_CAEH+7) = Q(PTR_CAEH+7) * HOTSUP      !  ... E
          Q(PTR_CAEH+8) = Q(PTR_CAEH+8) * HOTSUP      !  ... Et
          Q(PTR_CAEH+9) = Q(PTR_CAEH+9) * HOTSUP**2   !  ... SIG**2(Ex)
          Q(PTR_CAEH+10) = Q(PTR_CAEH+10) * HOTSUP**2 !  ... SIG**2(Ey)
        ELSE
          CALL ERRMSG ( 'Illegal pointer in LIST',
     &                  'AIDA_FIX_CAEH',
     &                  'Correction is suspect for this event',
     &                  'W' )
          OK = .FALSE.
        ENDIF                             ! if list(i) .EQ. 0
      ENDDO                             ! i = 1, aida_list_length

      OK = .TRUE.
      RETURN
      END

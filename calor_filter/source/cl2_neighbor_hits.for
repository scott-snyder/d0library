      SUBROUTINE CL2_NEIGHBOR_HITS (IETA, IPHI, ILYR, ETUP, ETDN, N, OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given the address of a cell, find the transverse
C-   energy in the neighbors above and below.  If the neighbor in question is in
C-   EM floor 3, we return the AVERAGE of the cells in that layer.
C-
C-   N.B.  - this is expected to run on the L2 CAEP bank, which has word +5 as
C-   NOMINAL (z=0) TRANSVERSE ENERGY, not energy.  If the RECO CAEP is used, one
C-   will end up with ENERGY rather than TRANSVERSE energy.
C-
C-   Inputs  : IETA, IPHI, ILYR [I] the indices of the cell
C-   Outputs : ETUP             [R] the trans. energy in the neighbor above
C-             ETDN             [R] the trans. energy in the neighbor below
C-             N                [I] the number of neighbors (not necessarily
C-                                  hit) found; should be 0, 1, or 2.
C-             OK               [L] .TRUE. if successful; .FALSE. on error
C-   Controls: none
C-
C-   Created  18-SEP-1992   Marc Paterno
C-                                        offline CAEP instead of L2 CAEP
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$CALOR_FILTER$SOURCE:PTCAEP2.DEF'
      INTEGER  IETA, IPHI, ILYR, N
      REAL     ETUP, ETDN
      LOGICAL  OK
C----------------------------------------------------------------------
      INTEGER  UP, DN, LCAEP, GZCAEP, NREP, PTR, I, ABSIETA
      INTEGER  IL
      EXTERNAL GZCAEP
      REAL     ET(2)
C----------------------------------------------------------------------
      ETUP = 0.
      ETDN = 0.
      CALL VZERO(ET,2)
      N = 0

      CALL CL2_GET_NEIGHBORS (IETA, IPHI, ILYR, UP, DN)
C
C ****  If no neighboring cells were identified, return with error status.
C
      IF ( UP .EQ. 0 .AND. DN .EQ. 0 ) THEN
        OK = .FALSE.
        GOTO 999
      ENDIF
C
C ****  Find the CAEP bank; return with error if none found.
C
      LCAEP = GZCAEP()
      IF ( LCAEP .LE. 0 ) THEN
        OK = .FALSE.
        GOTO 999
      ENDIF

      NREP = IQ(LCAEP+2)

      ABSIETA = ABS(IETA)
C
C ****  Loop over upper and lower neighbors, and count them (taking special care
C ****  of EM layer 3, with its finer granularity).
C
      DO I = 1, 2

        IF ( I .EQ. 1 ) THEN
          IL = UP
        ELSE
          IL = DN
        ENDIF                           ! if i .eq. 1

        IF ( IL .NE. 0 ) THEN           ! skip this one if nonexistant
          N = N + 1                     ! found another neighbor
C
C ****  Special handling required for EM layer 3
C
          IF ( IL .EQ. 3 ) THEN
            IF ( ABSIETA .LE. 11 ) THEN       ! normal CC
              DO IL = 3, 6
                PTR = PTCAEP2(IL, IPHI, IETA)
                IF ( PTR .GT. 0 ) THEN
                  PTR = LCAEP + (PTR-1)*NREP + 5
                  ET(I) = ET(I) + Q(PTR)
                ENDIF
              ENDDO
            ELSE IF ( IETA .EQ. 12 ) THEN     ! south end CCEM abberation
              DO IL = 3, 4
                PTR = PTCAEP2(IL, IPHI, IETA)
                IF ( PTR .GT. 0 ) THEN
                  PTR = LCAEP + (PTR-1)*NREP + 5
                  ET(I) = ET(I) + Q(PTR)
                ENDIF
              ENDDO
            ELSE IF ( IETA .EQ. -12 ) THEN    ! north end CCEM abberation
              DO IL = 5, 6
                PTR = PTCAEP2(IL, IPHI, IETA)
                IF ( PTR .GT. 0 ) THEN
                  PTR = LCAEP + (PTR-1)*NREP + 5
                  ET(I) = ET(I) + Q(PTR)
                ENDIF
              ENDDO
            ELSE IF ( IETA .EQ. 14 ) THEN     ! south end CCEM abberation
              DO IL = 5, 6
                PTR = PTCAEP2(IL, IPHI, IETA)
                IF ( PTR .GT. 0 ) THEN
                  PTR = LCAEP + (PTR-1)*NREP + 5
                  ET(I) = ET(I) + Q(PTR)
                ENDIF
              ENDDO
            ELSE IF ( IETA .EQ. -14 ) THEN    ! north end CCEM abberation
              DO IL = 3, 4
                PTR = PTCAEP2(IL, IPHI, IETA)
                IF ( PTR .GT. 0 ) THEN
                  PTR = LCAEP + (PTR-1)*NREP + 5
                  ET(I) = ET(I) + Q(PTR)
                ENDIF
              ENDDO
            ELSE IF ( ABSIETA .LE. 26 ) THEN  ! normal ECEM
              DO IL = 3, 6
                PTR = PTCAEP2(IL, IPHI, IETA)
                IF ( PTR .GT. 0 ) THEN
                  PTR = (PTR - 1)*NREP + LCAEP + 5
                  ET(I) = ET(I) + Q(PTR)
                ENDIF
              ENDDO
            ELSE                            ! end ECEM; no subdivision
              PTR = PTCAEP2(IL, IPHI, IETA)
              IF ( PTR .GT. 0 ) THEN
                PTR = (PTR - 1)*NREP + LCAEP + 5
                ET(I) = Q(PTR)
              ENDIF                           ! if ptr .gt. 0
            ENDIF                             ! if absieta .le. 11...

          ELSE                              ! not EM layer 3
            PTR = PTCAEP2(IL, IPHI, IETA)
            IF ( PTR .GT. 0 ) THEN
              PTR = (PTR - 1)*NREP + LCAEP + 5
              ET(I) = ET(I) + Q(PTR)
            ENDIF
          ENDIF                             ! if il .eq. 3
        ENDIF                               ! if il .ne. 0
      ENDDO                                 ! i = 1, 2
C
C ****  take values from the arrays and stick them into the outputs
C
      ETUP = ET(1)
      ETDN = ET(2)
      OK = .TRUE.

  999 CONTINUE
      RETURN
      END

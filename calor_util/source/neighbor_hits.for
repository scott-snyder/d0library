      SUBROUTINE NEIGHBOR_HITS (IETA, IPHI, ILYR, ENERGYUP, ENERGYDN,
     &   N, OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given the address of a cell, find the energy in the
C-   neighbors above and below.  If the neighbor in question is in EM floor 3,
C-   we return the TOTAL of the cells in that layer.
C-
C-   This subroutine expects to find a CAEP bank.
C-
C-   Inputs  : IETA, IPHI, ILYR [I] the indices of the cell
C-   Outputs : ENERGYUP             [R] the energy in the neighbor above
C-             ENERGYDN             [R] the energy in the neighbor below
C-             N                [I] the number of neighbors (not necessarily
C-                                  hit) found; should be 0, 1, or 2.
C-             OK               [L] .TRUE. if successful; .FALSE. on error
C-   Controls: none
C-
C-   Created  18-SEP-1992   Marc Paterno
C-   Updated  19-SEP-1992   Marc Paterno  Adapted from L2 version; made to use
C-                                        offline CAEP instead of L2 CAEP, and
C-                                      energy rather than ET
C-   Updated  26-MAR-1993   Marc Paterno  Corrected bug in pointer lookup
C-   Updated  25-MAY-1993   Marc Paterno  Coorected bug in comment and in ECEM
C-                                        ieta lookup. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE  'D0$INC:PTCAEP.INC'
      INCLUDE  'D0$INC:ZEBCOM.INC'
      INTEGER  IETA, IPHI, ILYR, N
      REAL     ENERGYUP, ENERGYDN
      LOGICAL  OK
C----------------------------------------------------------------------
      INTEGER  UP, DN, LCAEP, GZCAEP, NREP, PTR, I, ABSIETA, N_PTR
      INTEGER  IL
      EXTERNAL GZCAEP
      REAL     ENERGY(2)
C----------------------------------------------------------------------
      ENERGYUP  = 0.0
      ENERGYDN  = 0.0
      ENERGY(1) = 0.0
      ENERGY(2) = 0.0
      N = 0

      CALL GET_NEIGHBORS (IETA, IPHI, ILYR, UP, DN)


      IF ( UP .EQ. 0 .AND. DN .EQ. 0 ) THEN
        OK = .FALSE.
        RETURN
      ENDIF

      LCAEP = GZCAEP()
      IF ( LCAEP .LE. 0 ) THEN
        OK = .FALSE.
        RETURN
      ENDIF

      NREP = IQ(LCAEP+2)

      ABSIETA = ABS(IETA)
C
C ****  Loop over upper and lower neighbors
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
                N_PTR = PTCAEP(IETA, IPHI, IL)
                IF ( N_PTR .GT. 0 ) THEN
                  N_PTR = LCAEP + (N_PTR-1)*NREP + 5
                  ENERGY(I) = ENERGY(I) + Q(N_PTR)
                ENDIF
              ENDDO
              ENERGY(I) = ENERGY(I)
            ELSE IF ( IETA .EQ. 12 ) THEN     ! south end CCEM abberation
              DO IL = 3, 4
                N_PTR = PTCAEP(IETA, IPHI, IL)
                IF ( N_PTR .GT. 0 ) THEN
                  N_PTR = LCAEP + (N_PTR-1)*NREP + 5
                  ENERGY(I) = ENERGY(I) + Q(N_PTR)
                ENDIF
              ENDDO
              ENERGY(I) = ENERGY(I)
            ELSE IF ( IETA .EQ. -12 ) THEN    ! north end CCEM abberation
              DO IL = 5, 6
                N_PTR = PTCAEP(IETA, IPHI, IL)
                IF ( N_PTR .GT. 0 ) THEN
                  N_PTR = LCAEP + (N_PTR-1)*NREP + 5
                  ENERGY(I) = ENERGY(I) + Q(N_PTR)
                ENDIF
              ENDDO
              ENERGY(I) = ENERGY(I)
            ELSE IF ( IETA .EQ. 14 ) THEN     ! south end CCEM abberation
              DO IL = 5, 6
                N_PTR = PTCAEP(IETA, IPHI, IL)
                IF ( N_PTR .GT. 0 ) THEN
                  N_PTR = LCAEP + (N_PTR-1)*NREP + 5
                  ENERGY(I) = ENERGY(I) + Q(N_PTR)
                ENDIF
              ENDDO
              ENERGY(I) = ENERGY(I)
            ELSE IF ( IETA .EQ. -14 ) THEN    ! north end CCEM abberation
              DO IL = 3, 4
                N_PTR = PTCAEP(IETA, IPHI, IL)
                IF ( N_PTR .GT. 0 ) THEN
                  N_PTR = LCAEP + (N_PTR-1)*NREP + 5
                  ENERGY(I) = ENERGY(I) + Q(N_PTR)
                ENDIF
              ENDDO
              ENERGY(I) = ENERGY(I)
            ELSE IF ( ABSIETA .LE. 26 ) THEN  ! normal EC
              DO IL = 3, 6
                N_PTR = PTCAEP(IETA, IPHI, IL)
                IF ( N_PTR .GT. 0 ) THEN
                  N_PTR = LCAEP + (N_PTR-1)*NREP + 5
                  ENERGY(I) = ENERGY(I) + Q(N_PTR)
                ENDIF
              ENDDO
              ENERGY(I) = ENERGY(I)
            ELSE                              ! no EM3 subdivision
              N_PTR = PTCAEP(IETA, IPHI, IL)
              IF ( N_PTR .GT. 0 ) THEN
                N_PTR = (N_PTR - 1)*NREP + LCAEP + 5
                ENERGY(I) = Q(N_PTR)
              ENDIF
            ENDIF

          ELSE                              ! not EM layer 3
            N_PTR = PTCAEP(IETA, IPHI, IL)
            IF ( N_PTR .GT. 0 ) THEN
              N_PTR = (N_PTR - 1)*NREP + LCAEP + 5
              ENERGY(I) = Q(N_PTR)
            ENDIF                           ! if ptr .gt. 0
          ENDIF                             ! if il .eq. 3

        ENDIF                               ! if il .ne. 0
      ENDDO                                 ! i = 1, 2
C
C ****  take values from the arrays and stick them into the outputs
C
      ENERGYUP = ENERGY(1)
      ENERGYDN = ENERGY(2)
      OK = .TRUE.
      RETURN
      END

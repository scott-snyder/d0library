      FUNCTION L2CRCAL_CLUSTER()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : We will take the ideal track hits as reported by
C-                         L2CRCAL_TRACK_MUON (see L2CRCAL_CONT.INC) and
C-                         for each ideal 'hit', we will add up the energy
C-                         in that cell and the neighboring cells of that
C-                         layer as determined by IMUHIT_CONE.
C-                              IMUHIT_CONE = 0  -> just the 'hit' cell
C-                              IMUHIT_CONE = 1  -> add nearest neighbors
C-                              IMUHIT_CONE = 2  -> add 2nd ring of neighbors
C-
C-   Inputs  :
C-   Controls:
C-
C-   Created  23-AUG-1990   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$CALOR_FILTER$SOURCE:PTCAEP2.DEF'
      INCLUDE 'D0$INC:L2CRCAL_CONT.INC'      ! tool common
      INCLUDE 'D0$INC:L2CRCALS.INC'     ! tool string common
      INCLUDE 'D0$INC:L2J_UTIL.INC'     ! L2 utility
      LOGICAL L2CRCAL_CLUSTER
      INTEGER I,LCAEP,IPOINT,GZCAEP,L
      INTEGER IE,IP,IETA2,IPHI2,NRING
C----------------------------------------------------------------------
      L2CRCAL_CLUSTER = .FALSE.         ! Assume bad
      IF (NMCELL .LE. 0) THEN           ! Return if the track did not
        MUMSG = ' Predicted track did not intercept any cells'
        SMSG  = ' No cells hit '
        GOTO 800
      END IF
C---We also want to keep track of total energy found.
      MUHIT_ETOT = 0.0
C---Size of cone
      NRING = IMUHIT_CONE( L2CRCAL_PARAM )
C---We now need to know which cells we need to unpack. We know the
C---cone size as it is a parameter and the center of the cone is give by
C---the IMUHIT_XXX arrays.
      DO I = 1,NMCELL
        CALL L2J_CL2_DECLARE(IMUHIT_ETA(I),IMUHIT_PHI(I),IMUHIT_CONE(
     &    L2CRCAL_PARAM) )
      END DO
C---We are ready to unpack. Get the CAEP bank link.
      LCAEP = GZCAEP()
      MUMSG = ' No CAEP bank '
      SMSG  = ' NO CAEP-no cluster'
      IF (LCAEP .LE. 0) GOTO 800
C---Now we are going to try to cluster around each 'hit'. The arrays
C---IMUHIT_XXXX describe 1 cell per layer. We are going to cluster around
C---each of these 'hit's according to the value of IMUHIT_ICONE.
      CALL VZERO(MUHIT_E,NMCELL_MAX)    ! Zero energies
      DO I = 1,NMCELL                   ! For each 'hit'
C---Cycle over all members of the cone defined by IMUHIT_ICONE
        IETA2 = 2*IMUHIT_ETA(I) - SIGN(1,IMUHIT_ETA(I))
        IPHI2 = IMUHIT_PHI(I)
        DO IE = IETA2 - 2*NRING , IETA2 + 2*NRING , 2
          DO IP = IPHI2 - NRING , IPHI2 + NRING
            IF (IMUHIT_LYR(I) .EQ. 3) THEN      ! EM3, do other pads
              DO L = 4,6
                IF (L2J_ETA(IE) .NE. 0) THEN
                  IPOINT = PTCAEP2(L,L2J_PHI(IP),L2J_ETA(IE))
                  IF (IPOINT .NE. 0) THEN
                    IPOINT = (IPOINT-1)*IQ(LCAEP+2)
                    MUHIT_E(I) = MUHIT_E(I) + Q(LCAEP + IPOINT + 5)/
     &                L2J_SIN( L2J_ETA(IE) )
                    MUHIT_ETOT = MUHIT_ETOT + Q(LCAEP + IPOINT + 5)/
     &                L2J_SIN( L2J_ETA(IE) )
                  END IF
                END IF
              END DO
            END IF
            IF (L2J_ETA(IE) .NE. 0) THEN
              IPOINT = PTCAEP2(IMUHIT_LYR(I),L2J_PHI(IP),L2J_ETA(IE))
              IF (IPOINT .NE. 0) THEN
                IPOINT = (IPOINT-1)*IQ(LCAEP+2)
                MUHIT_E(I) = MUHIT_E(I) + Q(LCAEP + IPOINT + 5)/
     &                L2J_SIN( L2J_ETA(IE) )
                MUHIT_ETOT = MUHIT_ETOT + Q(LCAEP + IPOINT + 5)/
     &                L2J_SIN( L2J_ETA(IE) )
              END IF
            END IF
          END DO
        END DO
      END DO

      L2CRCAL_CLUSTER = .TRUE.
      RETURN
  800 CONTINUE                  ! ERROR
      RETURN
      END

      SUBROUTINE L2JETS_ENG
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 1) Add to JET core energy by extending cone
C-                            1 ring at a time. In this way adjoining
C-                            jets 'share' energy.
C-                         2) Add to EM ET of jet
C-                         3) Continue calculating eta-phi 'size' of jet
C-                         4) Round cone
C-
C-   Inputs  :
C-   Outputs :
C-   Controls: Controlled by current parameter set: NOWPAR
C-
C-   Created  16-MAY-1990   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$PARAMS:L2JETS.PARAMS'
      INCLUDE 'D0$PARAMS:JAUX.PARAMS'   ! parameters for JAUX
      INCLUDE 'D0$INC:ZEBCOM.INC'       ! ZEBCOM zebra common
      INCLUDE 'D0$INC:L2LINK.INC'       ! L2 link common
      INCLUDE 'D0$INC:L2JETS_CONT.INC'  ! JETS control common
      INCLUDE 'D0$INC:L2JETS_CHARS.INC' ! Character variables
      INCLUDE 'D0$INC:L2JETS_HOT.INC'   ! Hot tower candidates
      INCLUDE 'D0$INC:L2JETS_ALGO.INC'  ! Algorithm utitlities
      INCLUDE 'D0$INC:L2JETS_PAR.INC'   ! Parameter sets
      INTEGER ICAND,IP,IRING,IPOINT
      INTEGER NTOWER,LIST(4,100),ITOWER
      REAL EMET,TOTET,DETAL1,DPHIL1
      LOGICAL SKIP                      ! SKIP this routine if set
      INTEGER NRING1,NRING2
C----------------------------------------------------------------------
      DETAL1 = (MAXETA-MINETA)/FLOAT(2*NETAL1)
      DPHIL1 = (MAXPHI-MINPHI)/FLOAT(NPHIL1)
C---We assume that jet cores have been found first. This means that
C
C-      1) Each Jet candidate under consideration is flagged with
C-      JAUX EVT entry = L2J_CORE
C-      2) Each such candidate has a IETA,IPHI,PHI,ETA and Jet Core ET
C-      entry in JAUX as well.
C
C- In this routine we will add to the energy of each such candidate ring
C- by ring. We will then flag each EVT entry in JAUX with L2J_ENG This
C- signifies that the Jet has its full ET.

C---Define pointer into JAUX
      IPOINT = LJAUX + (NOW_IND_PARAM - 1)*NJTHOT*NREP_JAUX
C---For each ring in addition to the jet core already defined. Cycle through
C---the candidates and add the available energy in that ring to that of
C---the candidates.
      NRING1 = ICON_CEN( NOWPARAM ) + 1
      NRING2 = ICON_ENG( NOWPARAM )
      SKIP   = .FALSE.                  ! Dont skip this routine
      ! unless...
      IF (NRING1 .GT. NRING2 ) THEN     ! do nothing here but set to -3
        NRING2 = NRING1                 ! do this to force one loop
        SKIP   = .TRUE.                 ! but skip any real work
      END IF


      DO IRING = NRING1 , NRING2
        DO 500, ICAND = 1, NJTHOT
C---Take only those being currently considered:
        IP = IPOINT + (ICAND-1)*NREP_JAUX
        IF ( IQ( IP + PJEVT) .NE. L2J_CORE)   GOTO 500
        IF (IRING .EQ. NRING2) IQ( IP + PJEVT ) = L2J_ENG  !mark it
        IF (SKIP) GOTO 500

        CALL CL1RING(IQ(IP+PJIETA),IQ(IP+PJIPHI),IRING,NTOWER,LIST)
        DO 600, ITOWER = 1,NTOWER
        IF (ICELL_USED(LIST(1,ITOWER),LIST(2,ITOWER),NOW_IND_PARAM).EQ.
     &      NOWEVT .AND.
     &      (LIST(1,ITOWER)**2 + LIST(2,ITOWER)**2 ) .LE. 
     &      ICON_ENG(NOWPARAM)**2 ) GOTO 600
        CALL CL1PHET(LIST(2,ITOWER),LIST(1,ITOWER),EMET,TOTET)
        Q( IP + PJET) = Q( IP + PJET) + TOTET
        Q( IP + PJEMFR)= Q( IP + PJEMFR) + EMET
        Q( IP + PJETASIZ) = Q( IP + PJETASIZ) + 
     &      TOTET*ABS( LIST(3,ITOWER))*DETAL1
        Q( IP + PJPHISIZ) = Q( IP + PJPHISIZ) + 
     &      TOTET*ABS( LIST(4,ITOWER))*DPHIL1
C---Flag this tower as being unavailable to other jets for this parameter
C---set.
        ICELL_USED(LIST(1,ITOWER),LIST(2,ITOWER),NOW_IND_PARAM) = NOWEVT
  600   CONTINUE
  500   CONTINUE
      END DO
  999 RETURN
      END

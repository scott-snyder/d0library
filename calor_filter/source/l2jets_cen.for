      SUBROUTINE L2JETS_CEN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 1) Find center of Jet 'core'
C-                         2) Find Et of Core
C-
C-   Inputs  :
C-   Outputs :
C-   Controls: see L2JETS_ALGO & L2JETS_PARAM common blocks (INC files)
C-              Use parameter set NOWPARAM as passed by level 2
C-              3/1/92: Make cone round instead of square
C-   Created  14-MAY-1990   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$PARAMS:L2JETS.PARAMS'
      INCLUDE 'D0$PARAMS:JAUX.PARAMS'   ! parameters for JAUX
      INCLUDE 'D0$INC:ZEBCOM.INC'       ! ZEBCOM zebra common
      INCLUDE 'D0$INC:L2LINK.INC'       ! L2 link common
      INCLUDE 'D0$INC:L2JETS_CONT.INC'  ! JETS control common
      INCLUDE 'D0$INC:L2JETS_CHARS.INC'         ! Character variables
      INCLUDE 'D0$INC:L2JETS_HOT.INC'   ! Hot tower candidates
      INCLUDE 'D0$INC:L2JETS_ALGO.INC'  ! Algorithm utitlities
      INCLUDE 'D0$INC:L2JETS_PAR.INC'   ! Parameter sets
      INTEGER ICAND,IP,IRING,IPOINT,IAND,IOR, IP1, II

C&IF VAXVMS,VAXELN
C&ELSE
C&      EXTERNAL IAND,IOR
C&ENDIF
      INTEGER NTOWER,LIST(4,100),ITOWER
      REAL EMET,TOTET,DPHIL1,DETAL1
C----------------------------------------------------------------------
      DETAL1 = (MAXETA-MINETA)/FLOAT(2*NETAL1)
      DPHIL1 = (MAXPHI-MINPHI)/FLOAT(NPHIL1)
C---In this routine we define what is called the jet 'CORE'. It is
C---defined by taking a candidate hot tower from the hot tower list
C---and adding up rings of trigger towers around it. The number of rings
C---used is defined by ICON_CEN( NOWPARAM ). For example, if ICON_CEN = 1,
C---then we define the core of the candidate as the hot tower + all its
C---nearest neighbors.
C
C---We calculate  quanities:
C       1) Eta of core center (as defined by Et weighted average)
C       2) Phi '  '     '
C       3) total ET of core
C       4) EM ET of core
C       5) sum of ET*|(eta(tower) - eta(hot tower)|
C       6) same as 5 but for phi
C---This algorithm is to be run on a Jet candidate as determined by Level 1
C---and passed to us via the hot tower list. So we will cycle through the
C---list of hot towers (candidates) and run this algorithm on each one
C---EXCEPT those that:
C
C       1) Do not have the proper bit set in their mask.
C       2) Have already gone through this algorithm
C       3) Those that have been 'captured' by another jet candidate.

C---Define pointer into JAUX
      IPOINT = LJAUX + (NOW_IND_PARAM - 1)*NJTHOT*NREP_JAUX

      DO 500, ICAND = 1, NJTHOT
      IP = IPOINT + (ICAND-1)*NREP_JAUX
C---Take only candidates with correct bit set
      IF ( IAND(NOWL1BIT, IHOT_MSK_JT(ICAND)) .EQ. 0 ) GOTO 500
C---Skip those done before
      IF ( IQ( IP + PJEVT) .NE. L2J_UNPROC)   GOTO 500
      IQ(IP+PJEVT) = L2J_ABSORB             ! Flag it as being looked at
C---Skip it if its energy has been claimed by someone else:
      IF (ICELL_USED( IQ(IP+PJIETA),IQ(IP+PJIPHI),NOW_IND_PARAM) .EQ.
     &  NOWEVT)
     &  GOTO 500
      IQ(IP+PJEVT) = L2J_ENG               ! We call this a JET 
C      IQ(IP+PJEVT) = L2J_CORE             ! We call this a JET CORE
C---Cycle over all the rings in this cone.  For each one, unpack all the
C---contributing towers. Find total ET and average PHI,ETA
C
C---New: Only unpack those within a circular cone. 
C---Require R**2  .LE. ICON_ENG( NOWPARAM )**2
      DO IRING = 0, ICON_CEN( NOWPARAM )
        CALL CL1RING(IQ(IP+PJIETA),IQ(IP+PJIPHI),IRING,NTOWER,LIST)
C
        DO ITOWER = 1,NTOWER
          CALL CL1PHET(LIST(2,ITOWER),LIST(1,ITOWER),EMET,TOTET)
          IF (ICELL_USED( LIST(1,ITOWER), LIST(2,ITOWER),
     &        NOW_IND_PARAM) .NE. NOWEVT    .AND.
     &        ( LIST(4,ITOWER)**2 + LIST(3,ITOWER)**2 ) .LE.
     &          ICON_ENG( NOWPARAM )**2 )  THEN
            Q( IP + PJET) = Q( IP + PJET) + TOTET
            Q( IP + PJEMFR)= Q( IP + PJEMFR) + EMET
            Q( IP + PJETA) = Q( IP + PJETA) + TOTET*LIST(3,ITOWER)
     &        *DETAL1
            Q( IP + PJPHI) = Q( IP + PJPHI) + TOTET*LIST(4,ITOWER)
     &        *DPHIL1
            Q( IP + PJETASIZ) = Q( IP + PJETASIZ) +
     &        TOTET*ABS( LIST(3,ITOWER))*DETAL1
            Q( IP + PJPHISIZ) = Q( IP + PJPHISIZ) +
     &        TOTET*ABS( LIST(4,ITOWER))*DPHIL1
C---Flag this tower as being unavailable to other jets for this parameter
C---set.
            ICELL_USED(LIST(1,ITOWER),LIST(2,ITOWER),NOW_IND_PARAM) =
     &        NOWEVT
          END IF
        END DO
      END DO
C---Divide averages by total ET of cone
      IF (Q(IP+PJET) .GE. 1.) THEN
        Q( IP + PJETA) = Q( IP + PJETA)/( Q(IP + PJET) + ZEERO)
        Q( IP + PJPHI) = Q( IP + PJPHI)/ (Q(IP + PJET) + ZEERO)
      ELSE
        Q(IP+PJETA) = 0.
        Q(IP+PJPHI) = 0.
      END IF
C---Find real bin positions
      Q(IP+PJETA) = Q(IP+PJETA) + DETAL1*IQ(IP+PJIETA) -
     &  SIGN(1,IQ(IP+PJIETA)) * DETAL1/2.
      Q(IP+PJPHI) = Q(IP+PJPHI) + DPHIL1*IQ(IP+PJIPHI) -
     &  SIGN(1,IQ(IP+PJIPHI)) * DPHIL1/2.
C---Handle boundary conditions
C     IF ( Q(IP+PJETA) .GT. MAXETA .OR. Q(IP+PJETA) .LT. MINETA)
C    &      STOP 765
      IF (Q(IP+PJPHI) .GT. MAXPHI) Q(IP+PJPHI) = Q(IP+PJPHI) -
     &      MAXPHI
     &      + MINPHI
      IF (Q(IP+PJPHI) .LT. MINPHI) Q(IP+PJPHI) = Q(IP+PJPHI) +
     &      MAXPHI
     &      - MINPHI
C
C: We have a jet. In clustering this jet, we may have 'absorbed' other
C: candidates. If so, we add their trigger mask to that off the seed
C: candidate.
C
      DO II = 1, NJTHOT
        IF ( II .NE. ICAND ) THEN
          IP1 = IPOINT + (II-1)*NREP_JAUX
          IF ( IQ( IP1+ PJEVT ) .EQ. L2J_UNPROC .AND. ICELL_USED( IQ(
     &      IP1+PJIETA), IQ( IP1+PJIPHI), NOW_IND_PARAM ) .EQ. NOWEVT )
     &      THEN
            JET_MASK(ICAND) = IOR( JET_MASK(ICAND), IHOT_MSK_JT( II ) )
            IQ( IP1 + PJEVT ) = L2J_ABSORB
          ENDIF
        ENDIF
      ENDDO

  500 CONTINUE
  999 RETURN
      END

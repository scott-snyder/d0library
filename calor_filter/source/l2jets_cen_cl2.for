      SUBROUTINE L2JETS_CEN_CL2
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 1) Find center of Jet 'core'
C-                         2) Find Et of Core
C-                         3) Find Em Et of Core
C-                         4) Add eta-phi rms size
C-                         5) Round the cone (used to be square )
C-                         Same as L2JETS_CEN, but use readout towers
C-              with J. Linnemann's fast unpack (CL2_XXXX) routines.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls: see L2JETS_ALGO & L2JETS_PARAM common blocks (INC files)
C-              Use parameter set NOWPARAM as passed by level 2
C-   Created  14-MAY-1990   Richard V. Astur
C-   Updated   1-OCT-1994   James T. Linnemann  speed up tower loop 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:L2JETS.PARAMS'
      INCLUDE 'D0$PARAMS:JAUX.PARAMS'   ! parameters for JAUX
      INCLUDE 'D0$INC:ZEBCOM.INC'       ! ZEBCOM zebra common
      INCLUDE 'D0$CALOR_FILTER$SOURCE:PTCAEP2.DEF' ! pointer array,
                                                   !perhaps zero'd here
      INCLUDE 'D0$INC:L2LINK.INC'       ! L2 link common
      INCLUDE 'D0$INC:L2JETS_CONT.INC'  ! JETS control common
      INCLUDE 'D0$INC:L2JETS_CHARS.INC'         ! Character variables
      INCLUDE 'D0$INC:L2JETS_HOT.INC'   ! Hot tower candidates
      INCLUDE 'D0$INC:L2JETS_ALGO.INC'  ! Algorithm utitlities
      INCLUDE 'D0$INC:L2JETS_PAR.INC'   ! Parameter sets
      INCLUDE 'D0$INC:L2J_UTIL.INC'     ! eta-phi lookup
      INCLUDE 'D0$INC:TTEDGE.INC'       ! Trigger to Readout eta array

      INTEGER ICAND,IPOINT,IAND,IOR,POINT
      INTEGER IEJ,IPJ,NR,LOC
C&IF VAXVMS,VAXELN
C&ELSE
C&      EXTERNAL IAND,IOR
C&ENDIF
      REAL ET,EMET,DPHIL2,DETAL2, EM_ET_TOW, ET_TOW
      INTEGER IETA2,IPHI2,IETA3,IETA1,IPHI3,IPHI1, II
      INTEGER LCAEP,GZCAEP,IE,IP,IPH,IL, L2J_GET_PT_TO_L1FADC, IP1
      LOGICAL BAD_CAL_FLAG
C----------------------------------------------------------------------
      DETAL2 = .1
      DPHIL2 = (MAXPHI-MINPHI)/FLOAT(NPHIL)
C
C---In this routine we define what is called the jet 'CORE'. It is
C---defined by taking a candidate hot tower from the hot tower list
C---and adding up rings of trigger towers around it. The number of rings
C---used is defined by ICON_CEN( NOWPARAM ). For example, if ICON_CEN = 1,
C---then we define the core of the candidate as the hot tower + all its
C---nearest neighbors.
C
C---We calculate three quanities:
C       1) Eta of core center (as defined by Et weighted average)
C       2) Phi '  '     '
C       3) total ET of core
C       4) EM ET of core
C       5) Sum of abs( eta - eta(hottower))*ET of core
C       6) same as 5 but with phi
C
C---This algorithm is to be run on a Jet candidate as determined by Level 1
C---and passed to us via the hot tower list. So we will cycle through the
C---list of hot towers (candidates) and run this algorithm on each one
C---EXCEPT those that:
C
C       1) Do not have the proper bit set in their mask.
C       2) Have already gone through this algorithm
C       3) Those that have been 'captured' by another jet candidate.
C
C---Define pointer into JAUX
      IPOINT = LJAUX + (NOW_IND_PARAM - 1)*NJTHOT*NREP_JAUX

      DO 500, ICAND = 1, NJTHOT
      IP = IPOINT + (ICAND-1)*NREP_JAUX
C---Skip those done before
      IF ( IQ( IP + PJEVT) .NE. L2J_UNPROC)   GOTO 500
      JET_MASK( ICAND ) = IHOT_MSK_JT( ICAND )  ! Set its trigger mask
C---Take only candidates with correct bit set
      IF ( IAND(NOWL1BIT, JET_MASK( ICAND ) ) .EQ. 0 ) GOTO 500
C---Skip it if its energy has been claimed by someone else:
      IF (ICELL_USED( IQ(IP+PJIETA),IQ(IP+PJIPHI),NOW_IND_PARAM) .EQ.
     &  NOWEVT) GOTO 500
      IQ(IP+PJEVT) = L2J_ABSORB                 ! Flag it as being looked at
C---Cycle over all the readout towers in the 'trigger tower' cone defined
C---by ICON_CEN(NOWPARAM).  We will use the L2J_UTIL lookup arrays to do this.
C---First we declare the area we are interested in terms of readout towers.

      IETA2 = IQ( IP + PJIETA )         ! L1 Eta index of tower
C
C: 9/26/93 - Use the trigger tower which the average of the cone
C
      IF ( IETA2 .GT. 0 ) THEN          ! Convert to 1:40
        IETA2 = IETA2 + 20
      ELSE
        IETA2 = IETA2 + 21
      ENDIF
      CALL l2j_etaphi_ave( ieta2, iq(ip+pjiphi), -icon_cen( NOWPARAM ),
     &  -icon_cen( NOWPARAM ), icon_cen( NOWPARAM), 
     &  icon_cen( NOWPARAM), q( l2j_get_pt_to_l1fadc() ), 
     &  iq( ip + pjieta ) , iq( ip +pjiphi ), emet, et )

      IF ( IQ( IP + PJIETA ) .GT. 20 ) THEN
        IQ( IP + PJIETA ) = IQ( IP + PJIETA ) - 20
      ELSE
        IQ( IP + PJIETA ) = IQ( IP + PJIETA ) - 21
      ENDIF
C
C: This may change the position of our candidate, so check it again to
C: see if it is already used.
C
      IF (ICELL_USED( IQ(IP+PJIETA),IQ(IP+PJIPHI),NOW_IND_PARAM) .EQ.
     &  NOWEVT) GOTO 500
C     IQ(IP+PJEVT) = L2J_CORE           ! We call this a JET CORE
      IQ(IP+PJEVT) = L2J_ENG           ! We call this a JET 
C
      IETA2 = TTEDGE( IQ(IP+PJIETA) )   ! Smallest eta in this t. tower
      IPHI2 = 2*IQ(IP+PJIPHI) - 1       ! Always valid even in 'coarse'
C                                       ! regions of detector.
C      IETA2 = 2*IQ(IP+PJIETA) - (1 + SIGN(1,IQ(IP+PJIETA)) )/2
C      IPHI2 = 2*IQ(IP+PJIPHI) - 1          ! map to smallest ieta,iphi

      CALL L2J_CL2_DECLARE(IETA2,IPHI2,2*ICON_CEN(NOWPARAM)+1)
C---Now we are ready to loop over the cone. Remap IETA2,IPHI2 so as to use
C---L2J_UTIL arrays.
      IETA3 = 2*IETA2 - SIGN(1,IETA2)
      IPHI3 = IPHI2
C---We are ready to unpack. Get the CAEP bank link.
      LCAEP = GZCAEP()
      IF ( LCAEP .LE. 0 ) THEN
        IF ( BAD_CAL_FLAG() ) THEN
          NOWERRMESS = 'Cal Data problem'
          NOWSMESS  = 'Bad Cal Data'
          NOWERR    = ERR_UTILITY
          ERR_FLAG  = 'W'
          GOTO 900
        ELSE
          NOWERRMESS = 'No CAEP bank'
          NOWSMESS   = 'No CAEP bank'
          NOWERR     =  ERR_CAL_UNPACK
          ERR_FLAG   = 'W'
          GOTO 900
        ENDIF
      ENDIF
C---Loop
C---New: Make square cone 'round'.
C---Newer: Center cone in middle of trigger tower.
      NR = IQ(LCAEP+2)
      LOC = LCAEP + 5 - NR
      DO IE = IETA3 - 4*ICON_CEN(NOWPARAM) , IETA3 + 4*ICON_CEN(
     &  NOWPARAM) + 2,2
        IEJ = L2J_ETA(IE)
        IF (IEJ .NE. 0) THEN
          DO IPH = IPHI3 - 2*ICON_CEN(NOWPARAM) , IPHI3 + 2*ICON_CEN(
     &      NOWPARAM) + 1
            IF  ( ( -.5 +(IE-IETA3)/2)**2 + (IPH-IPHI3-.5)**2  .GT.
     &        (2*ICON_CEN(NOWPARAM)+1)**2 ) GOTO 333
C
C          IF ( ( ((IE-IETA3)/2)**2 + (IPH-IPHI3)**2 ) .GT. (2*ICON_ENG(
C     &      NOWPARAM ))**2 ) GOTO 333
            IETA1 = (IEJ+SIGN(1,IEJ))/2
            IPJ = L2J_PHI(IPH)
            IPHI1 = (IPJ+1)/2
            IF (ICELL_USED(IETA1,IPHI1,NOW_IND_PARAM) .NE. NOWEVT) THEN
C
C: 9/23/93. Dont add tower if it is less than L2J_ET_TOWER_MIN
              ET_TOW    = 0.0
              EM_ET_TOW = 0.0
              DO IL = 1,NLYRL
                POINT = PTCAEP2(IL,IPJ,IEJ)
                IF (POINT .GT. 0) THEN
                  ET = Q( LOC + POINT*NR)
                  ET_TOW = ET_TOW + ET
                  IF ( IL .LE. MXLYEM ) EM_ET_TOW = EM_ET_TOW + ET
                END IF
              END DO
              IF ( ET_TOW .GE. L2J_ET_TOWER_MIN ) THEN
                Q(IP + PJET) = Q( IP + PJET) + ET_TOW
                Q(IP + PJEMFR) = Q(IP + PJEMFR) +
     &              EM_ET_TOW
                Q(IP + PJETA) = Q( IP + PJETA)+ET_TOW*((IE-IETA3)/2)
     &              *DETAL2
                Q(IP + PJPHI) = Q( IP + PJPHI) + ET_TOW*(IPH-IPHI3)
     &            *DPHIL2 
                Q(IP + PJETASIZ) = Q( IP + PJETASIZ)+
     &              ET_TOW*(ABS(IE-IETA3)/2)*DETAL2
                Q(IP + PJPHISIZ) = Q( IP + PJPHISIZ) +
     &              ET_TOW*ABS(IPH-IPHI3)*DPHIL2
              ENDIF
            END IF
  333       CONTINUE      ! Skip if outside cone
          END DO
        END IF
      END DO
C---Flag this tower as being unavailable for other jets.
      DO IE = IETA3 - 4*ICON_CEN(NOWPARAM) , IETA3 + 4*ICON_CEN(
     &  NOWPARAM) + 2,2
        IEJ = L2J_ETA(IE)
        DO IPH = IPHI3 - 2*ICON_CEN(NOWPARAM) , IPHI3 + 2*ICON_CEN(
     &    NOWPARAM) + 1
          IPJ = L2J_PHI(IPH)
          IETA1 = (IEJ+SIGN(1,IEJ))/2
          IPHI1 = (IPJ+1)/2
          ICELL_USED(IETA1,IPHI1,NOW_IND_PARAM) = NOWEVT
        END DO
      END DO
C---Divide averages by total ET of cone
      Q( IP + PJETA) = Q( IP + PJETA)/( Q(IP + PJET) + ZEERO )
      Q( IP + PJPHI) = Q( IP + PJPHI)/( Q(IP + PJET) + ZEERO )
C---Find real bin positions
      Q(IP+PJETA) = Q(IP+PJETA) + DETAL2*IETA2 -
     &  SIGN(1,IETA2) * DETAL2/2.
      Q(IP+PJPHI) = Q(IP+PJPHI) + DPHIL2*IPHI2 -
     &  SIGN(1,IPHI2) * DPHIL2/2.
C---Handle boundary conditions
CD     IF ( Q(IP+PJETA) .GT. MAXETA .OR. Q(IP+PJETA) .LT. MINETA)
CD    &      STOP 765
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
      GOTO 999
  900 CONTINUE
  999 RETURN
      END

      FUNCTION L2CRCAL_TRACK_MUON(ITRACK,OUT_OF_RANGE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determine what cells will be hit by the
C-                         specified muon track. See L2CRCAL_CONT.INC
C-
C-   Inputs  : ITRACK refers to which muon track as stored in L2CRCAL common
C-   Outputs : OUT_OF_RANGE = .TRUE. if track pointed out of calorimeter
C-   Controls:
C-
C-   Created  10-JUN-1990   Richard V. Astur
C-   Note: This code is specific to the central calorimeter. Some sacrifice
C-         of accuracy has been made to improve speed.
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL ZEERO
      PARAMETER (ZEERO = 1.E-37)
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:L2CRCAL_CONT.INC'      ! Cosmic Ray Cal. Tool Common
      INCLUDE 'D0$INC:L2CRCALS.INC'
      LOGICAL L2CRCAL_TRACK_MUON
      REAL INCPHI
      PARAMETER( INCPHI = 2*PI/64.)
      REAL DXDS,DYDS,DZDS,X0,Y0,Z0,DVDS,DZDV,DYDV,DXDV,V0
      REAL DV,X,Y,Z,PHI,DPHI
      INTEGER IDEP,ILAYER,IE,ITRACK,IPHI,IETA,IMID
      REAL INITPHI
      LOGICAL OUT_OF_RANGE,UP,DOWN
      INTEGER ISIGN
C----------------------------------------------------------------------
      L2CRCAL_TRACK_MUON = .FALSE.      ! Assume nothing
      NMCELL = 0                        ! no hits yet.
      IF (NTRAK .LT. ITRACK) THEN       ! Not enough tracks
        MUMSG = ' Not enought tracks '
        SMSG  = ' No track '
        GOTO 800
      END IF


C---We have a 3 dimensional track. What I need to do is shift to a cood.
C---system where it is two dimensional. I will keep the Z axis the same.
C---and define V coordinate to be along vector sum of the x and y  of the
C---particle. (Note the particle then is moving in the -V direction!)
C
C--NOTE: I assume that the directional cosines point the particle towards the
C--calorimeter.
C---Now find x,y and z components of track direction
      DXDS = XCOS( ITRACK)
      DYDS = YCOS( ITRACK)
      DZDS = ZCOS( ITRACK)               ! directional cosines of track.
      X0   = XPART(ITRACK)               ! Assumed starting position
      Y0   = YPART(ITRACK)
      Z0   = ZPART(ITRACK)               ! position at detection of muon
      V0 = SQRT( X0**2 + Y0**2 )
      IF ( X0*DXDS + Y0*DYDS .GT. 0.) THEN      ! DR/Dt
        DXDS = -DXDS
        DYDS = -DYDS
        DZDS = -DZDS
      END IF
C---This muon must have a nonzero component along V direction.
      DVDS = SQRT( DXDS**2 + DYDS**2 )
      MUMSG = ' Transverse component too small '
      SMSG  = ' No PT comp'
      IF (DVDS .LT. .001) GOTO 800
C---Define components along V axis:
      DZDV = DZDS/DVDS
      DXDV = DXDS/DVDS
      DYDV = DYDS/DVDS
      INITPHI = ACOS(X0/V0)
      IF (Y0 .LT. 0.) INITPHI = 2*PI - INITPHI
      OUT_OF_RANGE = .FALSE.
      IMID=(2*NCIETA + 2)/2              ! Start in middle of Eta array
      IE  = IMID
      UP = .FALSE.
      DOWN = .FALSE.
      IDEP = 1                          ! First radius track might hit.
C---Make sure this IE corresponds to a real eta bin.
      IF (IDIETA(IE) .EQ. 0) THEN
        IE = IE + 1                     ! try another
        IF (IE .EQ. 2*NCIETA+3) IE = 1  ! restart if go over
        MUMSG = ' Cant find valid eta bin '
        SMSG   = ' NO VALID BIN'
        IF (IE .EQ. IMID) GOTO 800      ! quit if cant find any
      END IF
C---Now cycle through the various depths
C*** Track the particle throughout the detector:
      DO IDEP = 1, 2*NCLAYER + 1
        IF (OUT_OF_RANGE) GOTO 304
        DV = V0 - RBEG( IDEP )
        X = X0 + DXDV*DV                  ! Where does track hit first radius
        Y = Y0 + DYDV*DV
        Z = Z0 + DZDV*DV
        ILAYER = IDLAYER( IDEP )        ! Get real ILAYR indice
C---We must find the proper IE that corresponds to the first depth.
C---Find new IETA
  100   CONTINUE
        UP = (Z .GE. ZEND(IE , IDEP))
        DOWN=(Z .LT. ZBEG(IE , IDEP))
        IF (UP .AND. DOWN) THEN         ! Confusing
          MUMSG = ' UP AND DOWN?!!'
          SMSG  = ' UP AND DOWN'
          GOTO 800
        ELSE IF (UP) THEN
          IE = IE + 1
          GOTO 100
        ELSE IF (DOWN) THEN
          IE = IE - 1
          GOTO 100
        ELSE
          GOTO 500                      ! DONE
        END IF
C---Find new IPHI
  500   CONTINUE
C---Find IETA,IPHI
        IETA = IDIETA(IE)               ! get ieta number
        IF (OUT_OF_CC(IE,IDEP)) IETA=0
        PHI = ACOS(X/SQRT(X**2+Y**2))
        IF (Y .LT. 0) PHI = 2*PI - PHI
        IPHI= ((PHI/(2*PI+.001))*64)/1 + 1
C---For now we will treat EM3 as a tower of layer=3. It does not seem to
C---do much good to break it down at this level.
C---Adjust IPHI,IETA for EM3....
c        IF (ILAYER .EQ. 3) THEN         ! do special for EM3
c          IF (IETA .EQ. -12) THEN
c            ILAYER = ILAYER + 2
c          ELSE IF (IETA .EQ. 12) THEN
c          ELSE
c            IF (Z .GE. (ZBEG(IE,IDEP)+ZEND(IE,IDEP))/2. ) ILAYER =
c     &        ILAYER + 2
c          END IF
c          IF (PHI - (IPHI-1)*INCPHI .GE. .5*INCPHI ) ILAYER = ILAYER + 1
c        END IF
C---test new PHI to see if we are out of range:
        DPHI = INITPHI - PHI
        IF (ABS(DPHI) .GT. DPHI_CUT(L2CRCAL_PARAM)) THEN
          IF (ABS(ABS(DPHI) - PI) .GT. DPHI_CUT(L2CRCAL_PARAM))
     &      OUT_OF_RANGE = .TRUE.
          IF (OUT_OF_RANGE) THEN
            MUMSG = ' Track does not point to beam line '
            SMSG = ' Off Center'
            GOTO 800
          END IF
        END IF
        IF (ILAYER .NE. 0 .AND. IETA .NE. 0 .AND. .NOT. OUT_OF_RANGE)
     &    THEN
          NMCELL = NMCELL + 1
          IMUHIT_ETA( NMCELL ) = IETA
          IMUHIT_PHI( NMCELL ) = IPHI
          IMUHIT_LYR( NMCELL ) = ILAYER
        END IF
  305   IF (OUT_OF_CC(IE,IDEP) .AND. IDEP .GT. 1 + NCLAYER .AND. IDEP .
     &    LT. 2*NCLAYER + 2) THEN
          IF (Z .GT. 0.) IE = IE - 1
          IF (Z .LT. 0.) IE = IE + 1
          GOTO 305
        END IF
C        IF (IETA .EQ. 0 .AND. IDEP .NE. 1) IE = IMID
      END DO
  304 CONTINUE
      L2CRCAL_TRACK_MUON = .TRUE.       ! NO ERROR
      RETURN
  800 CONTINUE                          ! ERROR
  999 RETURN
      END

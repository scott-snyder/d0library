      SUBROUTINE L2_FDHITS(FROAD,HALF,UNIT,QUAD,SECT,WIRE,NPULSE,ZVTX)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fast hit counting in Level-2
C-
C-   Inputs  : HALF,UNIT,QUAD,SECT,WIRE,ZVTX
C-             FROAD    ( road array of max. and min. eta and theta )
C-   Outputs : NPULSE
C-
C-   Created : 15-MAR-1992   Yi-Cheng Liu  (for fast hit finding
C-                                          within a road, ideas from
C-                                          Dan Claes' L2_CDHITS )
C-   Updated : 15-JUL-1993   Yi-cheng Liu  (replacing MYSORT with FLPSOR)
C------------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:CDCMAP.INC/LIST'
      INCLUDE 'D0$LINKS:IZCDD3.LINK/LIST'
C
      INTEGER HALF,UNIT,QUAD,SECT,WIRE,LABEL
      INTEGER LCHA,CHNL          ! LCHA :  length of channel cluster
C                                ! CHNL :  logical channel address
      INTEGER POINT,END,HITLEN,HITADR
      INTEGER MASK8,MASK16,NPULSE
      INTEGER L2FTQU,L2FTPH      ! Pointer to wire timing information
      INTEGER L2FTSE             ! ( to the bank : FTSE )
      INTEGER JWIRE,WPARAM       ! Pointer to each wire.
      INTEGER GZL2FTSE,GZL2FTQU,GZL2FTPH    ! For getting the pointers
      INTEGER J, NUM_NEG_DIST
      REAL XC, YC, ZC, ZVTX
      REAL ZWIRE                 ! z-coordinates of the wire centers
      REAL FROAD(1:4)
      REAL RMIN, RMAX            ! ROAD_to-axis distances on X-Y plane
      REAL HEIGHT_THETA(0:7,0:5)       ! Heights of
C                                      ! theta wire planes (from axis)
      REAL ETAMIN,ETAMAX,PHIMIN,PHIMAX,THETAMIN,THETAMAX
      REAL PHIMIN_TEMP,PHIMAX_TEMP
      REAL DRIFT_MIN, DRIFT_MAX
      REAL ROAD_WIRE_DIST(4)     ! Theta sect ROAD-corner-to-wire dist.
      REAL DRIFT_VEL         ! drift velocity, get from STP (micron/ns)
      REAL PHI_1, PHI_2
      REAL THETA_1,THETA_2
      REAL THETA_SECT            ! theta of the center of a theta sector
      REAL PHI_SECT              ! phi of the center of phi sector
      REAL PHI_QUAD(0:1,0:7)     ! phi of the center of theta sector
C                                ! ( same as the quadrant center phi's )
C                                ! ( in terms of PHI_QUAD(HALF,QUAD) )
      REAL BEG_PULS, END_PULS
      REAL TZERO                 ! Get T0's from STP (where CALIB Tzero
C                                ! information were included ).
      data MASK8 / z'FF' /
      data MASK16 / z'FFFF' /
C
      DATA PHI_QUAD /0.785,0.785,2.356,2.356,3.927,3.927,
     &               5.498,5.498,0.0,0.0,1.571,1.571,
     &               3.142,3.142,4.712,4.712/
C
C----------------------------------------------------------------------
C
      NPULSE = 0
      NUM_NEG_DIST = 0
C
C  unpack channel length and channel number
C
      CALL FCODER(LABEL,HALF,UNIT,QUAD,SECT,WIRE,0,2)
      POINT = MAP(LABEL)           ! Get pointer to the last word of
C                                  ! the channel data.
      LCHA = IAND(IQ(POINT), MASK16) ! channel length ( byte counts
C                                      including itself )
      IF (LCHA.LE.4) GO TO 999     ! If no pulse at all, skip this wire
C                                  ! (there's something on this channel
C                                  !  only when there's more than one
C                                  !  data longword. )
      CHNL = IAND(ISHFT(IQ(POINT), -16), MASK16) ! Logical ch number.
      IF (CHNL.NE.LABEL) GO TO 999   ! double-checking to be safe.
      END = POINT - LCHA/4 + 1   ! Beginning word of the channel data.
C                                ! 'END' is a misnomer
C
C- ( Find drift_min and drift_max for hits to be within the road )
C  ( purely geometric )
C
C***** Corrected for finite ZVTX ! **********
C
      PHIMIN   = FROAD(1)
      PHIMAX   = FROAD(2)
      THETAMIN = FROAD(3)
      THETAMAX = FROAD(4)
C
C- ( Get the z-coordinate of the specified wire )
C- ( Need to use information in FALH bank )
C
      CALL GTL2FALH(HALF,UNIT,QUAD,SECT,WIRE,XC,YC,ZC)
      ZWIRE = ABS(ZC-ZVTX) ! distances in cm, have to correct for ZVTX
      IF (ZWIRE.EQ.0.0) GOTO 999
      RMIN = MIN(ABS(ZWIRE * TAN( THETAMIN )),
     &           ABS(ZWIRE * TAN( THETAMAX )))
      RMAX = MAX(ABS(ZWIRE * TAN( THETAMIN )),
     &           ABS(ZWIRE * TAN( THETAMAX )))
C
C-( Find max and min drift distances allowed by the ROAD )
C
      IF (UNIT.EQ.1) THEN                     ! phi-sector
        PHI_SECT = (PI/36.) + SECT * (PI/18.)
        PHIMIN_TEMP = ABS( PHIMIN - PHI_SECT )     ! Change to phi sector
        PHIMAX_TEMP = ABS( PHIMAX - PHI_SECT )     ! coordinate system
C
        PHI_1 = PHIMIN - PHI_SECT             ! If PHI_1*PHI_2 > 0 then
        PHI_2 = PHIMAX - PHI_SECT             ! wire not in the road ?
C
        IF (PHIMIN_TEMP.GT.PHIMAX_TEMP) THEN       ! Sorting phi
          PHIMAX = PHIMIN_TEMP
          PHIMIN = PHIMAX_TEMP
        ELSE
          PHIMAX = PHIMAX_TEMP
          PHIMIN = PHIMIN_TEMP
        ENDIF
C  Distances in microns.
        DRIFT_MIN = ( RMIN ) * PHIMIN*10000. ! Covers all
        DRIFT_MAX = ( RMAX ) * PHIMAX*10000. ! possibilities
C
      ELSE                                    ! theta sector
C
C-( Change to theta sector coordinate system  )
C
        THETA_SECT = ATAN2((SQRT(XC**2+YC**2)),(ZC-ZVTX))
        IF (THETA_SECT.LT.(0.0)) THEN  ! preventing - Theta values !
          THETA_SECT = THETA_SECT + PI
        ENDIF
        THETA_1 = THETAMIN - THETA_SECT       ! For checking if ROAD
        THETA_2 = THETAMAX - THETA_SECT       ! is straddling the wire
C
        PHIMIN_TEMP = ABS(PHIMIN - PHI_QUAD(HALF,QUAD))
        PHIMAX_TEMP = ABS(PHIMAX - PHI_QUAD(HALF,QUAD))
C
        IF (PHIMIN_TEMP.GT.PHIMAX_TEMP) THEN       ! Sorting phi
          PHIMAX = PHIMIN_TEMP
          PHIMIN = PHIMAX_TEMP
        ELSE
          PHIMAX = PHIMAX_TEMP
          PHIMIN = PHIMIN_TEMP
        ENDIF
C
        HEIGHT_THETA(QUAD,SECT) = SQRT(XC**2 + YC**2)    ! in cm.
C
C-( to find DRIFT_MIN and DRIFT_MAX : focusing on the 4 corners of
C-  the road , calculating their distances to the wire plane, and then
C-  just sort out the max and min. )
C
        ROAD_WIRE_DIST(1) =    (RMAX * COS(PHIMAX) -
     &                          HEIGHT_THETA(QUAD,SECT))
        ROAD_WIRE_DIST(2) =    (RMAX * COS(PHIMIN) -
     &                          HEIGHT_THETA(QUAD,SECT))
        ROAD_WIRE_DIST(3) =    (RMIN * COS(PHIMAX) -
     &                          HEIGHT_THETA(QUAD,SECT))
        ROAD_WIRE_DIST(4) =    (RMIN * COS(PHIMIN) -
     &                          HEIGHT_THETA(QUAD,SECT))
C
        DO J = 1, 4
          IF (ROAD_WIRE_DIST(J).LT.(0.0)) THEN
            NUM_NEG_DIST = NUM_NEG_DIST + 1
          ENDIF
          ROAD_WIRE_DIST(J) = ABS(ROAD_WIRE_DIST(J))
        ENDDO
C
C- ( Sorting the 4 ROAD-to-wire distances )
C
        CALL FLPSOR(ROAD_WIRE_DIST,4)
C
        DRIFT_MIN = ROAD_WIRE_DIST(1)*10000.  ! in microns.
        DRIFT_MAX = ROAD_WIRE_DIST(4)*10000.  ! in microns.
C
      ENDIF               ! End of drift distances finding
C
C- ( Get the TZERO for this wire , from time-to-position banks )
C
      L2FTSE = GZL2FTSE(HALF,UNIT,QUAD,SECT) ! see D0$ZEB$FDCCON:FTQU.ZEB
      WPARAM  = IC(L2FTSE+4)
      JWIRE = 6 + (WIRE) * 4         ! see D0$ZEB$FDCCON:FTSE.ZEB
C
      TZERO = C(L2FTSE+6+WPARAM*WIRE+1)    ! Read Electronic t-zero value
C
      DRIFT_VEL = C(L2FTSE+JWIRE + 3)      ! Read drift_velocities
C
      DO WHILE (POINT.GT.END.AND.NPULSE.LT.5)  ! Begin pulse loop for
C                                              ! this channel
        POINT = POINT - 1  ! Get to the word with time slice and
C                          ! pulse width information.
        HITLEN = IAND(IQ(POINT), MASK16)               ! Pulse width
        HITADR = IAND(ISHFT(IQ(POINT), -16), MASK16)   ! Time slice
C
C- ( Check if timing falls within ROAD. If NOT, skip to NEXT pulse! )
C
        END_PULS = (HITADR * 10. - TZERO) * DRIFT_VEL
        BEG_PULS = ((HITADR - HITLEN + 5) * 9.43 - TZERO)
     &             * DRIFT_VEL
C
C *** Get Drift Time and see if within drift_min and drift_max
C
        IF (BEG_PULS.LT.-100) GOTO 300          ! Bad HITLEN extracted
        IF (HITADR.GT.500) GOTO 999              ! Bad HITADR extracted
C
        IF (UNIT.EQ.1.AND.(PHI_1*PHI_2).GT.0) THEN ! Road does NOT straddle
                                                   ! wire
          IF (END_PULS .LT. DRIFT_MIN) GOTO 300 ! Pulse comes too early
        ELSEIF (UNIT.EQ.0.AND.((THETA_1*THETA_2).GT.0)) THEN
C                                               ! Road does NOT straddle
          IF ((SECT.LE.3).AND.(NUM_NEG_DIST.GE.1)) GOTO 250
C                                 ! Special treatment for half cells.
          IF (END_PULS .LT. DRIFT_MIN) GOTO 300 ! Pulse comes too early
  250     CONTINUE
        ENDIF
        IF (BEG_PULS .GT. DRIFT_MAX) GOTO 300   ! Pulse comes too late
        NPULSE = NPULSE + 1
        IF (NPULSE. GE. 5) GOTO 999
C
  300   CONTINUE
C
C- ( Skip to the next pulse )
C
        POINT = POINT - (HITLEN/4) + 1
C
      ENDDO                  ! End pulse loop for this channel.
C
  999 RETURN
      END

      SUBROUTINE VTGETX( LAYER, SECTOR, WIRE, RWTIME,
     &                   TIMERR, ZCOORD, NEWFLG, TIME, XDRIFT, XERROR )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert drift time to drift distance.
C-                         Two values are returned: one assuming phi>0,
C-                         and one assuming phi<0.  This version uses
C-                         a linear relation between time and distance,
C-                         and there is no correction for
C-                         z-coordinate (wire-bowing).
C-
C-   Inputs  : RWTIME: drift time (T0 included)
C-             TIMERR: error in drift time
C-             ZCOORD: z-coordinate along wire in cm
C-             LAYER, SECTOR, WIRE: location in VTX
C-
C-   Outputs : XDRIFT(2): x-position (local) of hit in cm
C-                       (1): solution for phi>0
C-                       (2): solution for phi<0
C-             XERROR: error in XDRIFT
C-             TIME: drift time (T0 removed)
C-
C-   Controls:
C-
C-   Created   1-FEB-1989   Peter Grudberg (from Chris Klopfenstein)
C-   Modified 07-NOV-1989   P.G. - T0 subtraction (new TIME output)
C-   Modified 10-SEP-1992   Alexandre Zinchenko - different time-distance
C-                          conversion function for "realistic" MC
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      REAL RWTIME, TIMERR, ZCOORD
C
      REAL XDRIFT(2), XERROR, TIME
      REAL VCONST, TZERO
C
      REAL STAGR(0:7)                   ! Wire stagger in cm
C
      INTEGER LVRFT, LVTMW, IWIRE, IPVTMW
      INTEGER LAYER, SECTOR, WIRE, GZVRFT, GZVTMW
      REAL XMINE, XDRMAX(0:23), RAD, PHI0
      INTEGER NEWFLG, ILAY
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C *** calculate max drift distances
C
      IF (FIRST) THEN
        LVRFT = GZVRFT()
        IF ( LVRFT .LE. 0 ) GO TO 999
        FIRST=.FALSE.
        DO 10 ILAY = 0,2
          PHI0 = C ( LVRFT + 6 + ILAY*7 )
          DO 10 IWIRE = 0,7
            RAD = C ( LVRFT + 7*(ILAY+1) ) + C ( LVRFT + 23 + IWIRE )
            XDRMAX(ILAY*8+IWIRE) = RAD * TAN(PHI0/180.*ACOS(-1.))
   10   CONTINUE
      ENDIF
C
C **** Get wire stagger from VRFT bank
C
      LVRFT = GZVRFT()
      IF ( LVRFT .LE. 0 ) GO TO 999
      DO 100 IWIRE = 0, 7
        STAGR(IWIRE) = C( LVRFT + 31 + IWIRE ) * (-1)**SECTOR
  100 CONTINUE
C
C **** Get drift velocity and offset from VTMW
C
      LVTMW = GZVTMW(LAYER)
      IF ( LVTMW .LE. 0 ) GO TO 999
      IPVTMW = LVTMW + (SECTOR*IC(LVTMW+4)+WIRE)*IC(LVTMW+3) + 5
      TZERO  = C( IPVTMW + 1 )          ! time offset (ns)
      VCONST = C( IPVTMW + 2 )          ! Drift velocity in cm/ns
C
C **** Calculate position:  distance=vconst*time+-stagger
C **** No z dependent correction for now
C
      TIME = RWTIME - TZERO
      IF (NEWFLG.NE.0) THEN ! "realistic" MC
        CALL VTXERT(TIME, XMINE, XERROR)
        XDRIFT(1) = XMINE + STAGR(WIRE)
        XDRIFT(2) = -(XMINE - STAGR(WIRE))
C **** Put hit on cathode, if drift distance is too long
        IF (ABS(XDRIFT(1)).GT.XDRMAX(LAYER*8+WIRE)) XDRIFT(1) =
     &      XDRMAX(LAYER*8+WIRE)*SIGN(1.,XDRIFT(1))
        IF (ABS(XDRIFT(2)).GT.XDRMAX(LAYER*8+WIRE)) XDRIFT(2) =
     &      XDRMAX(LAYER*8+WIRE)*SIGN(1.,XDRIFT(2))
      ELSE ! "ideal" MC
        XDRIFT(1) = TIME * VCONST + STAGR(WIRE)   ! phi>0 solution
        XDRIFT(2) = -(TIME * VCONST - STAGR(WIRE))! phi<0 solution
        XERROR = TIMERR * VCONST ! Error = time error * drift velocity
      ENDIF
C
  999 RETURN
      END

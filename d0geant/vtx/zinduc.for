      FUNCTION ZINDUC( LAYER, WIRE )
C-----------------------------------------------------------------------
C-   Purposes and Methods :
C-  Function ZINDUC gives the pulse area of the induced z strip pulse
C-  relative to the inducing sense wire pulse.  The algorithm currently
C-  used here reduces the pulse height by a constant PHFAC for pulses
C-  induced by the nearest sense wire and introduces a 1/r dependence 
C-  for more distant sense wires.
C-   Inputs  :
C-    LAYER is the z layer (0 through 5)
C-    WIRE is the wire number (0 through 7)
C-   Outputs :
C-                      Distance from z strip to closest wire
C-    ZINDUC = PHFAC * ---------------------------------------
C-                     Distance from z strip to wire no. WIRE
C-   Controls: none
C-
C-  T. Trippe, 3 Jan. 1986
C-  P. Grudberg 14-DEC-1988  get dimensions from geom. banks
C-  P. Grudberg 27-JUN-1989  change parameter PHFAC
C-  P. G.  17-NOV-1989  clean up
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
C
      INTEGER LVZST, LVRFT, NBLAYR, NWZLYR
      INTEGER GZVZST, GZVRFT
      INTEGER LAYER, WIRE, IZLAYR
      REAL ZINDUC, PHFAC, DIST
      REAL WIRSEP, WITOZS(0:5)
      DATA PHFAC /0.75/   ! ratio of induced z-strip to closest-wire
                          ! pulse height.
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
C-----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
C **** Get dimensions from geometry file
C
        LVZST = GZVZST()
        NWZLYR = IC( LVZST + 2 )
        DO IZLAYR = 0 , 5
          WITOZS(IZLAYR) = C( LVZST + 11 + NWZLYR*IZLAYR )
        ENDDO
        LVRFT = GZVRFT()
        NBLAYR = IC( LVRFT + 1 )
        WIRSEP = C( LVRFT + 7*NBLAYR + 3 ) - C( LVRFT + 7*NBLAYR + 2 )
      ENDIF
C
      IF( LAYER .EQ. 3 .OR. LAYER .EQ. 5 ) THEN
         DIST = WITOZS(LAYER) + (7-WIRE)*WIRSEP  ! distance to outer z layer
         ZINDUC = PHFAC * WITOZS(LAYER) / DIST
C
      ELSE IF ( LAYER .EQ. 2 .OR. LAYER .EQ. 4 ) THEN
         DIST= WITOZS(LAYER) + WIRE*WIRSEP       ! distance to inner z layer
         ZINDUC = PHFAC * WITOZS(LAYER) / DIST
C
      ELSE
         ZINDUC = 0.                            ! z layer not implemented
      ENDIF
C
  999 RETURN
      END

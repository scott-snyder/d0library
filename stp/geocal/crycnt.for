      SUBROUTINE CRYCNT (NAME,SHAPE,VNAME,MEDIUM,A1,A2,THICK,Z,Y,NPT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert raw data into (Z,Y) points for the
C-                         CENTRAL CRYOSTAT VESSELS. Units: centimeters.
C-
C-   Inputs  : NAME        Name of section. Up to 32 characters
C-   Outputs : SHAPE       GEANT volume shape (PCON, TUBE) (Character*4)
C-             VNAME       GEANT volume name (Character*4)
C-             MEDIUM      GEANT medium number
C-             A1,A2       Angles made by cross sectional lines
C                          at the endpoints of each section.
C-             THICK       Thickness of material in the section
C-             Z(*),Y(*)   Coordinates of points which define section
C-             NPT         Number of points
C-   Controls: None
C-
C-   Created  2-NOV-1988   Harrison B. Prosper, Elliott A. Treadwell
C-   Updated   8-DEC-1989   Harrison B. Prosper  
C-      Made compatible with new RCP 
C-   Updated  13-Feb-1992   Herbert Greenlee
C-      Got rid of machine blocks
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FIRST,TUBE,BOTTOM,TOP,RESET
      CHARACTER*(*) NAME
      CHARACTER*132 CTEMP
      CHARACTER*4   TEMP,SHAPE,VNAME
      REAL          SCALE,A1,A2,RMIN,Y1,Y2,ZFIRST
      REAL         CZ,CY,SINTH,THETA,R,RADC1,RADC2,RADC3
      REAL         Z(*),Y(*),THICK,THETA1,THETA2,DTHETA,ZLEN
      REAL         PI,PIBY2,DZ,DY,ZREL1,YREL1,ZREL2,YREL2,ZREL3,YREL3
      INTEGER      I,J,K,L,M,N,NPT,II,JJ,NMAX,NPT1,NPT2,NPT3,LT,MEDIUM
      PARAMETER( PI = 3.14159265 )
      PARAMETER( PIBY2 = PI/2.0 )
      PARAMETER( SCALE = 2.54 )
      PARAMETER( NMAX = 15 )
      REAL    GENER(NMAX)
      REAL    RAW(NMAX)
      INTEGER IRAW(NMAX)
      INTEGER TRULEN
      EQUIVALENCE (RAW(1),IRAW(1))
      DATA FIRST /.TRUE./
      SAVE FIRST
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
C ****  Get general raw data into local array
C
        CALL EZGSET ('GENERAL(3)',GENER(1),1)
        CZ = GENER(1) ! Coordinates of Central cryostat reference point
        CY = GENER(2)
      ENDIF
C
C ****  Determine position of end of sub-string WARM or COLD
C
      CALL WORD (NAME,I,J,L)
      LT = INDEX (NAME(1:L),'WARM')
      IF ( LT .LE. 0 ) THEN
        LT = INDEX(NAME(1:L),'COLD')
      ENDIF
      LT = LT + 3
C
C ****  Determine which section
C
      TUBE   = INDEX(NAME(1:L),'TUBE')   .GT. 0
      BOTTOM = INDEX(NAME(1:L),'BOTTOM') .GT. 0
      TOP    = INDEX(NAME(1:L),'TOP')    .GT. 0
C
      IF ( BOTTOM ) THEN
C
C ****  CENTRAL CRYOSTAT BOTTOM SURFACE; comprises bottom bend and lower
C       part of side
C
C ****  Get data on central tube
C
        CTEMP = NAME(1:LT)//'_TUBE(1)'
        CALL EZGSET_i (CTEMP(1:TRULEN(CTEMP)),IRAW(1),1)
C        CALL EZGSET (NAME(1:LT)//'_TUBE(1)',IRAW(1),1)
        ZLEN  = RAW(3)
C
C ****  Get data on bottom bend
C
        CTEMP = NAME(1:LT)//'_BOTTOM(1)'
        CALL EZGSET_i (CTEMP(1:TRULEN(CTEMP)),IRAW(1),1)
C        CALL EZGSET (NAME(1:LT)//'_BOTTOM(1)',IRAW(1),1)
        NPT1  = IRAW(1)     ! Number of points in section
        ZREL1 = RAW(2) - CZ ! Center of curvature relative to ORIGIN
        YREL1 = CY - RAW(3)
        RADC1 = RAW(4)
        THICK = RAW(5)  ! Thickness of skin
        CALL UHTOC (IRAW(7),4,SHAPE,4) ! Get GEANT shape
        CALL UHTOC (IRAW(8),4,VNAME,4) ! Get GEANT volume name
        MEDIUM = IRAW(9)               ! GEANT medium number
        A1 = RAW(10)  ! cross-section angle at lower end (in degrees)
        A2 = RAW(11)  ! cross-section angle at upper end
C
C ****  Get data on side arc
C
        CTEMP = NAME(1:LT)//'_SIDE(1)'
        CALL EZGSET_i(CTEMP(1:TRULEN(CTEMP)),IRAW(1),1)
C        CALL EZGSET (NAME(1:LT)//'_SIDE(1)',IRAW(1),1)
        NPT2  = IRAW(1) ! Number of points in section
        ZREL2 = RAW(2) - CZ  ! Center of curvature rel. to origin, Z coord.
        YREL2 = CY - RAW(3)  ! Center of curvature relative to origin, Y coord.
        RADC2 = RAW(4)  ! Radius of curvature to first surface
C
C ****  Get data on top bend
C
        CTEMP = NAME(1:LT)//'_TOP(1)'
        CALL EZGSET_i(CTEMP(1:TRULEN(CTEMP)),IRAW(1),1)
C        CALL EZGSET (NAME(1:LT)//'_TOP(1)',IRAW(1),1)
        NPT3  = IRAW(1)     ! Number of points in section
        ZREL3 = RAW(2) - CZ ! Center of curvature relative to ORIGIN
        YREL3 = RAW(3) + CY
        RADC3 = RAW(4)
C
C ****  Compute position of first point
C
        R = RADC1 + 0.5*THICK ! Correct radius of curvature for thickness
        II = 1
        Z(II) = ZLEN
        Y(II) = YREL1 - R
C
C ****  Compute first and last angle defining angular range of bend
C
        THETA1 =-PIBY2
        DZ = ZREL1 - ZREL2
        DY = YREL1 - YREL2
        THETA2 = ATAN2(DY,DZ)
C
C ****  Compute points along bend
C
        DTHETA = (THETA2-THETA1)/(NPT1-1)
        DO 100 JJ =  1, NPT1
          II = II + 1
          THETA = THETA1 + (JJ-1)*DTHETA
          Z(II) = R*COS(THETA) + ZREL1
          Y(II) = R*SIN(THETA) + YREL1
  100   CONTINUE
C
C ****  Compute correct radius of curvature for arc section
C
        R = RADC2 + 0.5*THICK
C
C ****  Compute points for arc; Zs must be monotonically increasing
C
        THETA1 = THETA2
        DZ = ZREL3 - ZREL2
        DY = YREL3 - YREL2
        THETA2 = ATAN2(DY,DZ)
        DTHETA = (THETA2-THETA1)/(NPT2-1)
C
        II = NPT1 + 1
        DO 110 JJ =  2, NPT2
          THETA = THETA1 + (JJ-1)*DTHETA
          II = II + 1
          Z(II) = R*COS(THETA) + ZREL2
          Y(II) = R*SIN(THETA) + YREL2
          IF ( Z(II) .LT. Z(II-1) ) GOTO 115
  110   CONTINUE
  115   CONTINUE
        NPT = II - 1
C
      ELSEIF ( TOP ) THEN
C
C ****  CENTRAL CRYOSTAT TOP SURFACE; comprises upper part of arc, top bend
C       and tube.
C
C ****  Get data on bottom bend
C
        CTEMP = NAME(1:LT)//'_BOTTOM(1)'
        CALL EZGSET_i(CTEMP(1:TRULEN(CTEMP)),IRAW(1),1)
C        CALL EZGSET (NAME(1:LT)//'_BOTTOM(1)',IRAW(1),1)
        ZREL1 = RAW(2) - CZ ! Center of curvature relative to ORIGIN
        YREL1 = CY - RAW(3)
C
C ****  Get data on side arc
C
        CTEMP = NAME(1:LT)//'_SIDE(1)'
        CALL EZGSET_i(CTEMP(1:TRULEN(CTEMP)),IRAW(1),1)
C        CALL EZGSET (NAME(1:LT)//'_SIDE(1)',IRAW(1),1)
        NPT2  = IRAW(1) ! Number of points in section
        ZREL2 = RAW(2) - CZ  ! Center of curvature rel. to origin, Z coord.
        YREL2 = CY - RAW(3)  ! Center of curvature relative to origin, Y coord.
        RADC2 = RAW(4)  ! Radius of curvature to first surface
C
C ****  Get data on top bend
C
        CTEMP = NAME(1:LT)//'_TOP(1)'
        CALL EZGSET_i(CTEMP(1:TRULEN(CTEMP)),IRAW(1),1)
C        CALL EZGSET (NAME(1:LT)//'_TOP(1)',IRAW(1),1)
        NPT3  = IRAW(1)     ! Number of points in section
        ZREL3 = RAW(2) - CZ ! Center of curvature relative to ORIGIN
        YREL3 = RAW(3) + CY
        RADC3 = RAW(4)
        THICK = RAW(5)  ! Thickness of skin
        CALL UHTOC (IRAW(7),4,SHAPE,4)
        CALL UHTOC (IRAW(8),4,VNAME,4) ! Get GEANT volume name
        MEDIUM = IRAW(9)               ! GEANT medium number
        A1 = RAW(10)  ! cross-section angle at lower end (in degrees)
        A2 = RAW(11)  ! cross-section angle at upper end
C
C ****  Compute correct radius of curvature for arc section
C
        R = RADC2 + 0.5*THICK
C
C ****  Compute points for arc; Zs must be monotonically decreasing
C
        DZ = ZREL1 - ZREL2
        DY = YREL1 - YREL2
        THETA1 = ATAN2(DY,DZ)
        DZ = ZREL3 - ZREL2
        DY = YREL3 - YREL2
        THETA2 = ATAN2(DY,DZ)
        DTHETA = (THETA2-THETA1)/(NPT2-1)
C
        II = 1
        RESET = .TRUE.
        THETA = THETA1
        Z(II) = R*COS(THETA) + ZREL2
        Y(II) = R*SIN(THETA) + YREL2
        DO 120 JJ =  2, NPT2
          THETA = THETA1 + (JJ-1)*DTHETA
          II = II + 1
          Z(II) = R*COS(THETA) + ZREL2
          Y(II) = R*SIN(THETA) + YREL2
          IF ( RESET ) THEN
            IF ( Z(II) .LT. Z(II-1) ) THEN
              Z(1) = Z(II-1)
              Y(1) = Y(II-1)
              Z(2) = Z(II)
              Y(2) = Y(II)
              II = 2
              RESET = .FALSE.
            ENDIF
          ENDIF
  120   CONTINUE
C
C ****  Compute first and last angle defining angular range of bend
C
        R = RADC3 + 0.5*THICK
        THETA1 = THETA2
        THETA2 = PIBY2
C
C ****  Compute points along bend
C
        DTHETA = (THETA2-THETA1)/(NPT3-1)
        DO 140 JJ =  2, NPT3
          II = II + 1
          THETA = THETA1 + (JJ-1)*DTHETA
          Z(II) = R*COS(THETA) + ZREL3
          Y(II) = R*SIN(THETA) + YREL3
  140 CONTINUE
C
C ****  Compute last point
C
      NPT = II + 1
      Z(NPT) = 0.0
      Y(NPT) = Y(NPT-1)
C
      ELSEIF ( TUBE ) THEN
C
C ****  CENTRAL CRYOSTAT TUBE
C
        CTEMP = NAME(1:L)//'(1)'
        CALL EZGSET_i(CTEMP(1:TRULEN(CTEMP)),IRAW(1),1)
C        CALL EZGSET (NAME(1:L)//'(1)',IRAW(1),1)
        NPT    = IRAW(1)  ! Number of points
        RMIN   = RAW(2)   ! Inner radius of tube
        ZLEN   = RAW(3)   ! Length in Z
        THICK  = RAW(4)
        ZFIRST = RAW(5)   ! Distance of first surface from origin
        CALL UHTOC (IRAW(7),4,SHAPE,4) ! Get GEANT shape
        CALL UHTOC (IRAW(8),4,VNAME,4) ! Get GEANT volume name
        MEDIUM = IRAW(9)               ! GEANT medium number
        Z(1) = ZFIRST
        Y(1) = RMIN + 0.5*THICK
        Z(2) = Z(1) + ZLEN
        Y(2) = Y(1)
      ENDIF
C
C ****  Convert to centimetres
C
      THICK = SCALE*THICK
      DO 150 I =  1,NPT
        Z(I) = SCALE*Z(I)
        Y(I) = SCALE*Y(I)
  150 CONTINUE
C
  999 RETURN
      END

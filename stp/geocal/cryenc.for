      SUBROUTINE CRYENC (NAME,SHAPE,VNAME,MEDIUM,A1,A2,THICK,Z,Y,NPT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert raw data into (Z,Y) points for the ENDCAP
C-                         CRYOSTAT VESSELS. Units: centimeters.
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
C-   Created  31-OCT-1988   Harrison B. Prosper, Elliott A. Treadwell
C-   Updated  21-MAR-1989   Harrison B. Prosper
C-                          Split Bellows into three volumes (TUBEs)
C-   Updated   8-DEC-1989   Harrison B. Prosper   
C-      Made compatible with new RCP
C-   Updated  13-Feb-1992   Herbert Greenlee
C-      Removed machine blocks.
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FIRST,FRONT,BULGE,BACK,RING,BTUBE,FLANGE,BELLOW
      CHARACTER*(*) NAME,INNER,OUTER
      CHARACTER*4   TEMP,SHAPE,VNAME
      REAL          SCALE,A1,A2,RMIN,Y1,Y2,ZFIRST,HLEN,VLEN1,VLEN2
      REAL         EMINUS,EPLUS,SINTH,THETA,R,RADC1,RADC2
      REAL         Z(*),Y(*),THICK,THETA1,THETA2,DTHETA,ZLEN
      REAL         PI,PIBY2,DZ,DY,ZREL1,YREL1,ZREL2,YREL2
      INTEGER      I,J,K,L,M,N,NPT,II,JJ,NMAX,NPT1,NPT2,LT,MEDIUM,JSIGN
      PARAMETER( PI = 3.14159265 )
      PARAMETER( PIBY2 = PI/2.0 )
      PARAMETER( SCALE = 2.54 )
      PARAMETER( NMAX = 15 )
      PARAMETER( INNER = '_RING_INNER' )
      PARAMETER( OUTER = '_RING_OUTER' )
      REAL    GENER(NMAX)
      REAL    RAW(NMAX)
      INTEGER IRAW(NMAX)
      CHARACTER*132 CTEMP
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
        CALL EZGSET ('GENERAL',GENER(1),1)
C
C ****  GENER(1) Distance from E- to E0 (origin)
C       GENER(2) Distance from E0 to E+
        EMINUS = GENER(1)
        EPLUS  = GENER(2)
      ENDIF
      A1 = 0.0
      A2 = 0.0
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
      FRONT = INDEX(NAME(1:L),'FRONT') .GT. 0
      BULGE = INDEX(NAME(1:L),'BULGE') .GT. 0
      BACK  = INDEX(NAME(1:L),'BACK')  .GT. 0
      RING  = INDEX(NAME(1:L),'RING')  .GT. 0
      BTUBE = INDEX(NAME(1:L),'BEAM_TUBE')  .GT. 0
      FLANGE = INDEX(NAME(1:L),'FLANGE')  .GT. 0
      BELLOW = INDEX(NAME(1:L),'BELLOW')  .GT. 0
C
      IF ( FRONT .OR. BULGE ) THEN
C
C ****  END CRYOSTAT BULGE OR FRONT SURFACE
C
        CALL EZGSET (NAME(1:L),IRAW(1),1)
        NPT   = IRAW(1) ! Number of points in section
C
        IF ( BULGE ) THEN
          ZREL1 = RAW(2) + EPLUS  ! Center of curv. rel. to origin
          JSIGN =-1
        ELSE
          ZREL1 = RAW(2) - EMINUS ! Center of curv. rel. to origin
          JSIGN = 1
        ENDIF
C
        YREL1 = RAW(3)  ! Center of curvature relative to point E0, Y coord.
        RADC1 = RAW(4)  ! Radius of curvature to first surface
        THICK = RAW(5)  ! Thickness of skin
        Y1    = RAW(6)  ! Y Coordinate of lowest point in section
C
        CALL UHTOC (IRAW(7),4,SHAPE,4) ! Get GEANT shape
        CALL UHTOC (IRAW(8),4,VNAME,4) ! Get GEANT volume name
        MEDIUM = IRAW(9)               ! GEANT medium number
        A1 = RAW(10)  ! cross-section angle at lower end (in degrees)
        A2 = RAW(11)  ! cross-section angle at upper end
C
        IF ( BULGE ) THEN
          CTEMP = NAME(1:LT)//INNER
          CALL EZGSET (CTEMP(1:TRULEN(CTEMP)),IRAW(1),1)
        ENDIF
        IF ( FRONT ) THEN
          CTEMP = NAME(1:LT)//OUTER
          CALL EZGSET (CTEMP(1:TRULEN(CTEMP)),IRAW(1),1)
        ENDIF
C        IF ( BULGE ) CALL EZGSET (NAME(1:LT)//INNER,IRAW(1),1)
C        IF ( FRONT ) CALL EZGSET (NAME(1:LT)//OUTER,IRAW(1),1)
        Y2    = RAW(2)   ! Inner radius of ring
C
        R = RADC1 + 0.5*THICK ! Correct radius of curvature for thickness
C
C ****  Compute first and last angle defining angular range of section.
C
        SINTH = (Y1-YREL1)/R
        THETA1 = ASIN(SINTH)
        SINTH = (Y2-YREL1)/R
        THETA2 = ASIN(SINTH)
C
C ****  Compute points along arc
C
        DTHETA = (THETA2-THETA1)/(NPT-1)
        DO 100 II =  1, NPT
          THETA = THETA1 + (II-1)*DTHETA
          Z(II) = R*COS(THETA)*JSIGN + ZREL1
          Y(II) = R*SIN(THETA) + YREL1
  100   CONTINUE
C
      ELSEIF ( BACK ) THEN
C
C ****  END CRYOSTAT BACK SURFACES
C
        CALL EZGSET (NAME(1:L),IRAW(1),1)
        NPT1  = IRAW(1) ! Number of points in section
        ZREL1 = RAW(2) - EMINUS  ! Center of curvature rel. to point E0
        YREL1 = RAW(3)  ! Center of curvature relative to point E0, Y coord.
        RADC1 = RAW(4)  ! Radius of curvature to first surface
        THICK = RAW(5)  ! Thickness of skin
        Y1    = RAW(6)  ! Y Coordinate of lowest point
C
        CALL UHTOC (IRAW(7),4,SHAPE,4) ! Get GEANT shape
        CALL UHTOC (IRAW(8),4,VNAME,4) ! Get GEANT volume name
        MEDIUM = IRAW(9)               ! GEANT medium number
        A1 = RAW(10)  ! cross-section angle at lower end (in degrees)
        A2 = RAW(11)  ! cross-section angle at upper end
C
        CTEMP = NAME(1:L)//'_BEND'
        CALL EZGSET (CTEMP(1:TRULEN(CTEMP)),IRAW(1),1)
C        CALL EZGSET (NAME(1:L)//'_BEND',IRAW(1),1)
        NPT2  = IRAW(1)
        ZREL2 = RAW(2) - EMINUS
        YREL2 = RAW(3)
        RADC2 = RAW(4)
C
        R = RADC1 + 0.5*THICK ! Correct radius of curvature for thickness
C
C ****  Compute first and last angle defining angular range of section.
C
        SINTH = (Y1-YREL1)/R
        THETA1 = ASIN(SINTH)
        DZ = ZREL2 - ZREL1
        DY = YREL2 - YREL1
        THETA2 = ATAN2(DY,DZ)
C
C ****  Compute points along arc
C
        DTHETA = (THETA2-THETA1)/(NPT1-1)
        DO 110 II =  1, NPT1
          THETA = THETA1 + (II-1)*DTHETA
          Z(II) = R*COS(THETA) + ZREL1
          Y(II) = R*SIN(THETA) + YREL1
  110   CONTINUE
C
C ****  Compute correct radius of curvature for bend section
C
        R = RADC2 + 0.5*THICK
C
C ****  Compute points for bend
C
        THETA1 = THETA2
        THETA2 = PIBY2
        DTHETA = (THETA2-THETA1)/(NPT2-1)
        II = NPT1
        DO 120 JJ =  2, NPT2
          THETA = THETA1 + (JJ-1)*DTHETA
          II = II + 1
          Z(II) = R*COS(THETA) + ZREL2
          Y(II) = R*SIN(THETA) + YREL2
  120   CONTINUE
C
        CTEMP = NAME(1:L)//'_TUBE'
        CALL EZGSET (CTEMP(1:TRULEN(CTEMP)),IRAW(1),1)
C        CALL EZGSET (NAME(1:L)//'_TUBE',IRAW(1),1)
        ZLEN  = RAW(4)
        NPT   = NPT1 + NPT2
        Z(NPT) = Z(NPT-1) - ZLEN
        Y(NPT) = Y(NPT-1)
C
      ELSEIF ( RING .OR. BTUBE .OR. FLANGE .OR. BELLOW ) THEN
C
C ****  END RINGS ETC.
C
        CALL EZGSET (NAME(1:L),IRAW(1),1)
        NPT    = IRAW(1)  ! Number of points
        RMIN   = RAW(2)   ! Inner radius of ring
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
C
      ENDIF
C
C ****  Convert to centimetres
C
      THICK = SCALE*THICK
      DO 130 I =  1,NPT
        Z(I) = SCALE*Z(I)
        Y(I) = SCALE*Y(I)
  130 CONTINUE
C
  999 RETURN
      END

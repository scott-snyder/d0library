      SUBROUTINE GETRAP(ECCH,CRACK,NSEG,TRAP,RAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns TRAP parameters given
C-                        Tube parameters in ECCH
C-
C-   Inputs  : ECCH(1-3) = Inner, Outer radius and Half width of tube
C-                           Inner radius touches the inner face
C-                           Outer radius touches outer face.
C-             ECCH(4-6) = Zposition of lower face center at inner radius
C-                         Zposition of upper face center at outer radius
C-                         Draft distance. (not necessary)
C-             CRACK  = Width of required Crack
C-             NSEG   = Number of segments in PHI
C-
C-   Outputs : TRAP  = Array containing TRAP parameters
C-             RAD   = Radius to place the TRAP
C-   Controls: None
C-
C-   Created  16-NOV-1985   Rajendran Raja
C-   Updated  14-SEP-1988   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL ECCH(6),TRAP(11),RAD,PHI,SPHI,CPHI,DEL,DINF,CRACK,ECCH1
      REAL PI
      INTEGER NSEG
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL GTSRCP('PI',PI,1)
      ENDIF
      PHI=PI/NSEG
C HALF ANGLE
      SPHI=SIN(PHI)
      CPHI=COS(PHI)
      DEL=CRACK/(CPHI)
      DINF=TAN(PHI)*CRACK
C
      TRAP(1)=(ECCH(2)-ECCH(1))*0.5   !half-length in z in trap system
      TRAP(1)=TRAP(1)-CRACK   !crack now goes all the way around
      TRAP(2)=ATAN2(0.5*ABS(ECCH(5)-ECCH(4)),.5*(ECCH(2)-ECCH(1))) !Th
      TRAP(2)=TRAP(2)*180./PI
      TRAP(3)=270.
C Trap (2) and (3) are polar and azimuth angles of the line connecting
C centers of the +/- z faces.
      TRAP(4)=ECCH(3)       !H1
      TRAP(5)=ECCH(1)*TAN(PHI)-DEL+DINF  !LB1
      TRAP(6)=TRAP(5)       !LH1 no draft
      TRAP(7)=0.            !TH1
      TRAP(8)=ECCH(3)       !H2
      TRAP(9)=ECCH(2)*TAN(PHI)-DEL-DINF  !LB2
      TRAP(10)=TRAP(9)      !LH2 no draft
      TRAP(11)=0.           !TH2
      RAD=(ECCH(2)+ECCH(1))*0.5        !radius where to place.
      RETURN
      END

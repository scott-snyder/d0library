      SUBROUTINE CRYOUT(LUNOUT,NAME,SHAPE,VNAME,MEDIUM,
     &                  ANGLE1,ANGLE2,THICK,Z,Y,NPT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert Z,Y points of cryostat into PCON and
C-                         tube parameters and write out parameters.
C-
C-   Inputs  : SHAPE       GEANT shape name (CHARACTER*4)
C-             VNAME       User-supplied volume name (CHARACTER*32)
C-             MEDIUM      GEANT medium number
C-             ANGLE1      Parameters defining shape of polytrapezoid
C-             ANGLE2
C-             THICK
C-             Z(*),Y(*)   NB: Data starts at z(2),y(2) !
C-             NPT
C-   Outputs : None
C-   Controls: None
C-
C-   Created   8-DEC-1988   Harrison B. Prosper
C-                          Modularizing code a bit more
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL         PI,PIBY2,PHI,DPHI,Z0,ANGLE1,ANGLE2,THICK,DZ,DY
      INTEGER      I,J,K,L,M,N,NMAX,NSEC,NP,II,III,MEDIUM,NZ,LNAME
      INTEGER      NPT,LUNOUT,MXSEC
C
      PARAMETER( PI     = 3.14159265 )
      PARAMETER( PIBY2  = PI/2.0 )
      PARAMETER( PHI = 0.0 )
      PARAMETER( DPHI = 360.0  )
      PARAMETER( NMAX = 500 )
C
      CHARACTER*4  VNAME,SHAPE
      CHARACTER*32 STRING
      CHARACTER*(*) NAME
      REAL  Z(*),Y(*)
      REAL  Z1(0:NMAX),Z2(0:NMAX),Y1(0:NMAX),Y2(0:NMAX)
      REAL  RMIN(0:NMAX),RMAX(0:NMAX)
C----------------------------------------------------------------------
C
C ****  Print out (Z,Y) points
C
C        CALL CPRZY (LUNZY,NAME(II),
C     &  SHAPE,VNAME,MEDIUM,ANGLE1,ANGLE2,THICK,Z(2),Y(2),NPT)
C
C ***********************************
C ****  COMPUTE SHAPE PARAMETERS ****
C ***********************************
C
C ****  POLYCONE
C
      IF ( SHAPE .EQ. 'PCON' ) THEN
C
C ****  Convert first to polytrapezoid
C
        CALL PTRAP
     &    (ANGLE1,ANGLE2,THICK,Z(2),Y(2),NPT,Z1(1),Y1(1),Z2(1),Y2(1))
C
C ****  Convert (Z,Y) points to polycone.
C
        CALL PCONE
     &      (Z1(0),Y1(0),Z2(0),Y2(0),NPT,Z(2),RMIN(1),RMAX(1),NZ)
        Z(1)    = PHI
        RMIN(0) = DPHI
        RMAX(0) = NZ
        NP = 3*(NZ+1)
        Z0 = Z(2) ! Z-displacement
        DO 220 I =  1,NZ
          Z(I+1) = Z(I+1) - Z0
  220   CONTINUE
C
C ****  TUBE
C
      ELSEIF ( SHAPE .EQ. 'TUBE' ) THEN
        RMIN(0) = 0.5*(Y(2)+Y(3) - THICK)
        RMAX(0) = 0.5*(Y(2)+Y(3) + THICK)
        Z(1) = 0.5*ABS(Z(3)-Z(2))
        NP = 3
        Z0 = Z(2) + Z(1) ! Z-displacement
      ENDIF
C
C ************************************
C ****  PRINT OUT IN SRCP FORMAT  ****
C ************************************
C
C ****  Make +Z name
C
      CALL WORD (NAME,I,J,LNAME)
      STRING = NAME(1:LNAME)//'+Z'
C
      CALL CRYPRT
     &    (LUNOUT,STRING,SHAPE,VNAME,MEDIUM,Z0,Z(1),RMIN(0),RMAX(0),NP)
C
C ****  Output mirror image of data (reflected in X-Y plane)
C
      Z0 = -Z0
      IF ( SHAPE .EQ. 'PCON' ) THEN
        DO 230 I =  1,NZ
          Z(I+1) =-Z(I+1)
  230   CONTINUE
      ENDIF
      STRING = NAME(1:LNAME)//'-Z'
      VNAME = VNAME(1:2)//'-'//VNAME(4:4)
C
      CALL CRYPRT
     &    (LUNOUT,STRING,SHAPE,VNAME,MEDIUM,Z0,Z(1),RMIN(0),RMAX(0),NP)
C
  999 RETURN
      END

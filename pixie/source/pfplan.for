      SUBROUTINE PFPLAN(HALF,A,B,C,D)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw an outline of a plane through FDC Half
C-                         at radius of 80cm from x,y=0., |z| =100. & 140.
C-
C-   Inputs  : HALF = Half of FDC where plane will be drawn
C-             A,B,C,D = Equation constants for the plane
C-                           Ax + By + Cz + D = 0
C-
C-   Created   8-JUN-1990   Jeffrey Bantly
C-   Updated   7-FEB-1992   Robert E. Avery   Hard code colour.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER HALF,I
      REAL    A,B,C,D
      REAL    DIR,X(4),Y(4),Z(4)
      REAL    A1,B1,C1,A2,B2,C2,D1,D2
      REAL    ROOT1,ROOT2,ROOT3,ROOT4,TEMP
      CHARACTER*4 P3DCLR
      CHARACTER*4 REM
      INTEGER TYP,IVAL,IER
      LOGICAL EZERROR
C----------------------------------------------------------------------
      DIR=-1.
      IF(HALF.EQ.1) DIR=1.
      Z(1)=100.*DIR
      Z(2)=100.*DIR
      Z(3)=140.*DIR
      Z(4)=140.*DIR
      D1=C*Z(1)+D
      D2=C*Z(3)+D
C
      IF(B.NE. 0.0 .OR. A.EQ.0.0) THEN  ! Solve for X's first.
        IF( B.EQ.0.0) B=0.00001
        A1=1.+((A/B)**2.)
        A2=A1
        B1=2.*A*D1/(B**2.)
        B2=2.*A*D2/(B**2.)
        C1=(D1/B)**2. - (80.**2.)
        C2=(D2/B)**2. - (80.**2.)
        TEMP=(B1**2.) - (4*A1*C1)
        IF(TEMP.LT.0.) THEN
          TEMP=0.0
        ENDIF
        ROOT1= -B1 - SQRT(TEMP)
        X(1)=ROOT1/(2.*A1)
        Y(1)=-1.*(A*X(1)/B + D1/B)
        ROOT2= -B1 + SQRT(TEMP)
        X(2)=ROOT2/(2.*A1)
        Y(2)=-1.*(A*X(2)/B + D1/B)
        TEMP=(B2**2.) - (4*A2*C2)
        IF(TEMP.LT.0.) THEN
          TEMP=0.0
        ENDIF
        ROOT3= -B2 + SQRT(TEMP)
        X(3)=ROOT3/(2.*A2)
        Y(3)=-1.*(A*X(3)/B + D2/B)
        ROOT4= -B2 - SQRT(TEMP)
        X(4)=ROOT4/(2.*A2)
        Y(4)=-1.*(A*X(4)/B + D2/B)
C
      ELSE                              ! Solve for Y's first.
        A1=1.+((B/A)**2.)
        A2=A1
        B1=2.*B*D1/(A**2.)
        B2=2.*B*D2/(A**2.)
        C1=(D1/A)**2. - (80.**2.)
        C2=(D2/A)**2. - (80.**2.)
        TEMP=(B1**2.) - (4*A1*C1)
        IF(TEMP.LT.0.) THEN
          TEMP=0.0
        ENDIF
        ROOT1= -B1 - SQRT(TEMP)
        Y(1)=ROOT1/(2.*A1)
        X(1)=-1.*(B*Y(1)/A + D1/A)
        ROOT2= -B1 + SQRT(TEMP)
        Y(2)=ROOT2/(2.*A1)
        X(2)=-1.*(B*Y(2)/A + D1/A)
        TEMP=(B2**2.) - (4*A2*C2)
        IF(TEMP.LT.0.) THEN
          TEMP=0.0
        ENDIF
        ROOT3= -B2 + SQRT(TEMP)
        Y(3)=ROOT3/(2.*A2)
        X(3)=-1.*(B*Y(3)/A + D2/A)
        ROOT4= -B2 - SQRT(TEMP)
        Y(4)=ROOT4/(2.*A2)
        X(4)=-1.*(B*Y(4)/A + D2/A)
      ENDIF
C
C  Draw outline of plane from points caculated above.
C
      CALL PXCOLR('DMG ')
      CALL J3MOVE( X(4),Y(4),Z(4) )
      DO 10 I=1,4
        CALL J3DRAW( X(I),Y(I),Z(I) )
   10 CONTINUE
C----------------------------------------------------------------------
  999 RETURN
      END

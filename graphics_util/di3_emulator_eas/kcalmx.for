      SUBROUTINE KCALMX(MAT,TRCODE,P1,P2,P3,P4,P5,P6,P7)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : calculates the transformation matrix for modeling
C-
C-   Inputs  :
C-              TRCODE : the transformation code as in JBUILD and JTRANS
C-              p1 ---> p7  : Various transformation parameters.
C-   Outputs :
C-              MAT : the output 4X4 transformation matrix.
C-
C-
C-   Created   8-AUG-1989   SHAHRIAR ABACHI
C-   Updated  24-MAR-2004   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL MAT(4,4)
      INTEGER TRCODE
      REAL P1, P2, P3, P4, P5, P6, P7
      INTEGER I, J, IJMP, TNMAT
      REAL A, B, TMAT1(4,4),TMAT2(4,4)
      REAL CNV, ANG, ALPHA, BETA, UPS
      REAL DX, DY, DZ, XSQ, YSQ, ZSQ, EPS, RHO
      DATA CNV, EPS/0.017453292, 1.0E-9/
C
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:pi.def'
C
      IF (TRCODE .LT. 1 .OR. TRCODE .GT. 11) THEN
        CALL ERROR('JTRANS: TRCODE OUT OF RANGE (1..11)')
      ENDIF
      CALL KMTID(MAT)
      CALL KMTID(TMAT1)
      CALL KMTID(TMAT2)
C
      ANG = P1
      IJMP = 0
      GOTO (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), TRCODE
    1 CONTINUE
      GOTO 999
C
C  Traslate by P1, P2, P3
C
    2 CONTINUE
      MAT(1,4) = P1
      MAT(2,4) = P2
      MAT(3,4) = P3
      GOTO 999
C
C  Scale in X,Y,Z by P4, P5, P6 about point P1, P2, P3
C
    3 CONTINUE
      TMAT1(1,4) = - P1
      TMAT1(2,4) = - P2
      TMAT1(3,4) = - P3
      TMAT2(1,1) =   P4
      TMAT2(2,2) =   P5
      TMAT2(3,3) =   P6
      CALL KMMUL(TMAT2, TMAT1, TMAT1)
      CALL KMTID(TMAT2)
      TMAT2(1,4) = P1
      TMAT2(2,4) = P2
      TMAT2(3,4) = P3
      CALL KMMUL(TMAT2, TMAT1, MAT)
      GOTO 999
    6 CONTINUE
      IJMP = IJMP + 1
    5 CONTINUE
      IJMP = IJMP + 1
    4 CONTINUE
      IJMP = IJMP + 1
      A = COS(ANG)
      B = SIN(ANG)
      GOTO (41, 42, 43), IJMP
C
C  Rotate in X
C
   41 CONTINUE
      MAT(2,2) = A
      MAT(2,3) = -B
      MAT(3,2) = B
      MAT(3,3) = A
      GOTO 999
C
C  Rotate in Y
C
   42 CONTINUE
      MAT(1,1) = A
      MAT(1,3) = -B
      MAT(3,1) = B
      MAT(3,3) = A
      GOTO 999
C
C  Rotate in Z
C
   43 CONTINUE
      MAT(1,1) = A
      MAT(1,2) = -B
      MAT(2,1) =  B
      MAT(2,2) = A
      GOTO 999
    7 CONTINUE
      ANG = ANG * CNV
      GOTO 4
    8 CONTINUE
      ANG = ANG * CNV
      GOTO 5
    9 CONTINUE
      ANG = ANG * CNV
      GOTO 6
C
C   Rotate about a line defined by two points (P1, P2, P3) & (P4, P5, P6)
C   by P7 radians.
C
   10 CONTINUE
      UPS = P7 / CNV
      IF (ABS(UPS) .LT. EPS) GOTO 999
      DX  = P4 - P1
      DY  = P5 - P2
      DZ  = P6 - P3
      XSQ = DX * DX
      YSQ = DY * DY
      ZSQ = DZ * DZ
      RHO = SQRT(XSQ + YSQ + ZSQ)
      IF (RHO .LT. EPS) THEN
        CALL ERROR('JOPEN: MODELING ERROR')
        RETURN
      ENDIF
C   Perform the general rotation about a line.
      IF (ABS(DY) .GT. EPS) THEN
        ALPHA = ATAN(DX / DY)*180/pi
        IF (DY .LT. 0.) ALPHA = ALPHA + 180.
      ELSE IF (DX .GT. EPS) THEN
        ALPHA = 90.
      ELSE IF (DX .LT. -EPS) THEN
        ALPHA = 270.
      ELSE
        ALPHA = 0.
      ENDIF
      BETA = ACOS(DZ / RHO)*180/pi
      TMAT1(1,4) = -P1
      TMAT1(2,4) = -P2
      TMAT1(3,4) = -P3
      TMAT2(1,1) = COS(ALPHA*pi/180)
      TMAT2(1,2) = -SIN(ALPHA*pi/180)
      TMAT2(2,1) = SIN(ALPHA*pi/180)
      TMAT2(2,2) = COS(ALPHA*pi/180)
      CALL KMMUL(TMAT2, TMAT1, TMAT1)
      CALL KMTID(TMAT2)
      TMAT2(2,2) = COS(BETA*pi/180)
      TMAT2(2,3) = -SIN(BETA*pi/180)
      TMAT2(3,2) = SIN(BETA*pi/180)
      TMAT2(3,3) = COS(BETA*pi/180)
      CALL KMMUL(TMAT2, TMAT1, TMAT1)
      CALL KMTID(TMAT2)
      TMAT2(1,1) = COS(UPS*pi/180)
      TMAT2(1,2) = -SIN(UPS*pi/180)
      TMAT2(2,1) = SIN(UPS*pi/180)
      TMAT2(2,2) = COS(UPS*pi/180)
      CALL KMMUL(TMAT2, TMAT1, TMAT1)
      CALL KMTID(TMAT2)
      TMAT2(2,2) = COS(-BETA*pi/180)
      TMAT2(2,3) = -SIN(-BETA*pi/180)
      TMAT2(3,2) = SIN(-BETA*pi/180)
      TMAT2(3,3) = COS(-BETA*pi/180)
      CALL KMMUL(TMAT2, TMAT1, TMAT1)
      CALL KMTID(TMAT2)
      TMAT2(1,1) = COS(-ALPHA*pi/180)
      TMAT2(1,2) = -SIN(-ALPHA*pi/180)
      TMAT2(2,1) = SIN(-ALPHA*pi/180)
      TMAT2(2,2) = COS(-ALPHA*pi/180)
      CALL KMMUL(TMAT2, TMAT1, TMAT1)
      CALL KMTID(TMAT2)
      TMAT2(1,4) = P1
      TMAT2(2,4) = P2
      TMAT2(3,4) = P3
      CALL KMMUL(TMAT2, TMAT1, MAT)
      GOTO 999
   11 CONTINUE
C
  999 CONTINUE
      RETURN
      END

      SUBROUTINE KMODL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Performs Modeling Transformation
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  DEC-1988   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      REAL DX, DY, DZ, XSQ, YSQ, ZSQ
      REAL P1, P2, P3, P4, P5, P6, P7, CNV
      REAL ALPHA, BETA, RHO, UPS, EPS
      INTEGER TRCODE, TNMAT, I
      DATA CNV /57.29577951/
      DATA EPS /1.0E-9/
      INTEGER ERRHND
      EXTERNAL ERRHND

      TNMAT = INT(MODMAT(4,1))
      DO 999 I=NTRN(TNMAT),1,-1
        TRCODE = INT(MODACT(1,I,TNMAT))
        P1 = MODACT(2,I,TNMAT)
        P2 = MODACT(3,I,TNMAT)
        P3 = MODACT(4,I,TNMAT)
        P4 = MODACT(5,I,TNMAT)
        P5 = MODACT(6,I,TNMAT)
        P6 = MODACT(7,I,TNMAT)
        P7 = MODACT(8,I,TNMAT)
        GOTO (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), TRCODE
    1   CONTINUE
        GOTO 999
C
C   Translate by p1, p2, p3.
C
    2   CONTINUE
        TRV(1)  = P1
        TRV(2)  = P2
        TRV(3)  = P3 * RIGHT
        CALL PTRANS('"', TRV, '"', ERRHND)
        GOTO 999
C
C  Scale in X,Y,Z by P4, P5, P6 about point P1, P2, P3
C
    3   CONTINUE
        TRV(1)  = P1
        TRV(2)  = P2
        TRV(3)  = P3 * RIGHT
        CALL PTRANS('"', TRV, '"', ERRHND)
        SCV(1)  = P4
        SCV(2)  = P5
        SCV(3)  = P6
        CALL PSCALE('"', SCV, '"', ERRHND)
        TRV(1)  = -P1
        TRV(2)  = -P2
        TRV(3)  = -P3 * RIGHT
        CALL PTRANS('"', TRV, '"', ERRHND)
        GOTO 999
C
C   Rotate in X by P1 radians
C
    4   CONTINUE
        CALL PROTX('"', -P1 * CNV, '"', ERRHND)
        GOTO 999
C
C   Rotate in Y by P1 radians
C
    5   CONTINUE
        CALL PROTY('"', -P1 * CNV, '"', ERRHND)
        GOTO 999
C
C   Rotate in Z by P1 radians
C
    6   CONTINUE
        CALL PROTZ('"', -P1 * CNV * RIGHT, '"', ERRHND)
        GOTO 999
C
C   Rotate in X by P1 degrees.
C
    7   CONTINUE
        CALL PROTX('"', -P1, '"', ERRHND)
        GOTO 999
C
C   Rotate in Y by P1 degrees
C
    8   CONTINUE
        CALL PROTY('"', -P1, '"', ERRHND)
        GOTO 999
C
C   Rotate in Z by P1 degrees
C
    9   CONTINUE
        CALL PROTZ('"', -P1 * RIGHT, '"', ERRHND)
        GOTO 999
C
C   Rotate about a line defined by two points (P1, P2, P3) & (P4, P5, P6)
C   by P7 radians.
C
   10   CONTINUE
        UPS = P7 * CNV
        IF (ABS(UPS) .LT. EPS) GOTO 999
        TRV(1) = P1
        TRV(2) = P2
        TRV(3) = P3 * RIGHT
        CALL PTRANS('"', TRV, '"', ERRHND)
        DX  = P4 - P1
        DY  = P5 - P2
        DZ  = (P6 - P3) * RIGHT
        XSQ = DX * DX
        YSQ = DY * DY
        ZSQ = DZ * DZ
        RHO = SQRT(XSQ + YSQ + ZSQ)
        IF (RHO .LT. EPS) THEN
          CALL ERROR('JOPEN: MODELING ERROR')
          RETURN
        ENDIF
C
C   Perform the general rotation about a line.
C
        IF (ABS(DY) .GT. EPS) THEN
          ALPHA = ATAND(DX / DY)
          IF (DY .LT. 0.) ALPHA = ALPHA + 180.
        ELSE IF (DX .GT. EPS) THEN
          ALPHA = 90.
        ELSE IF (DX .LT. -EPS) THEN
          ALPHA = 270.
        ELSE
          ALPHA = 0.
        ENDIF
C
        BETA = ACOSD(DZ / RHO)
C
        CALL PROTZ('"', -ALPHA, '"', ERRHND)
        CALL PROTX('"', -BETA, '"', ERRHND)
        CALL PROTZ('"', -UPS, '"', ERRHND)
        CALL PROTX('"', BETA, '"', ERRHND)
        CALL PROTZ('"', ALPHA, '"', ERRHND)
  101   CONTINUE
        TRV(1) = -P1
        TRV(2) = -P2
        TRV(3) = -P3 * RIGHT
        CALL PTRANS('"', TRV, '"', ERRHND)
        GOTO 999
   11   CONTINUE
        GOTO 999
C
C  Exit.
C
 999  CONTINUE
      RETURN
      END

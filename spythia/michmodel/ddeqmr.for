      SUBROUTINE DDEQMR(N,XA,XZ,Y,H0,EPS,SUB,W)

C     Based on a modification of the Runge-Kutta method suggested
C     by Merson. See G.N. Lance, Numerical Methods for High speed
C     Computers, Iliffe & Sons, London 1960, pp. 56-57

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*(*) NAME
      PARAMETER (NAME='DDEQMR')
      LOGICAL LER,LFN
C The below flag is for use with the CMSSM random_gen package. It
C causes a cascade of Returns back to the Calling Program which checks 
C this flag on return and Generates another point.
      Logical gtop_too_big
      Common/FLAGS/ gtop_too_big
      EXTERNAL SUB

      DIMENSION Y(N),W(N,0:*)

      PARAMETER (DELTA = 1D-13)
      PARAMETER (ONE = 1, TWO = 2, R2 = ONE/2, R3 = ONE/3)
      PARAMETER (R4 = 3/(4*TWO), R5 = 3/TWO, R6 = 9/TWO)
      PARAMETER (R7 = 2*TWO/3, R0 = ONE/32)

      IF(N .LT. 1 .OR. XA .EQ. XZ .OR. H0 .EQ. 0) RETURN
      DELTAX=DELTA*ABS(XZ-XA)
      EPS5=5*ABS(EPS)
      EPS0=R0*EPS5
      X=XA
      H1=SIGN(ONE,XZ-XA)*ABS(H0)
      SGH=SIGN(ONE,H1)
   12 IF(SGH*(X+H1-XZ) .LT. 0) THEN
        HH=H1
        H0=H1
        LFN=.FALSE.
      ELSE
        HH=XZ-X
        IF(ABS(HH) .LT. DELTAX) THEN
          DO 10 I = 1,N
   10       Y(I)=W(I,0)
          RETURN
        END IF
        LFN=.TRUE.
      END IF
      S2=R2*HH
      S3=R3*HH
      S7=R7*HH
      X1=X+HH
      X2=X+S2
      X3=X+S3

      CALL SUB(X,Y,W(1,1))
      If (gtop_too_big) return
      DO 1 I = 1,N
        W(I,1)=S3*W(I,1)
    1   W(I,0)=Y(I)+W(I,1)

          CALL SUB(X3,W(1,0),W(1,2))
      If (gtop_too_big) return
      DO 2 I = 1,N
        W(I,2)=S3*W(I,2)
    2   W(I,0)=Y(I)+R2*(W(I,1)+W(I,2))

      CALL SUB(X3,W(1,0),W(1,3))
      If (gtop_too_big) return
      DO 3 I = 1,N
        W(I,3)=S3*W(I,3)
        W(I,2)=3*W(I,3)
    3   W(I,0)=Y(I)+R4*(W(I,1)+W(I,2))

      CALL SUB(X2,W(1,0),W(1,4))
      If (gtop_too_big) return
      DO 4 I = 1,N
        W(I,4)=S7*W(I,4)
    4   W(I,0)=Y(I)+R5*(W(I,1)-W(I,2)+W(I,4))

      CALL SUB(X1,W(1,0),W(1,5))
      If (gtop_too_big) return
      DO 5 I = 1,N
        W(I,5)=S3*W(I,5)
    5   W(I,0)=Y(I)+R2*(W(I,1)+W(I,4)+W(I,5))

      DO 8 I = 1,N
        W(I,2)=ABS(W(I,1)-R6*W(I,3)+W(I,4)-R2*W(I,5))
        W(I,1)=ABS(W(I,0))
        IF(W(I,2) .GT. EPS5*W(I,1)) THEN
          H1=R2*HH
          IF(ABS(H1) .LT. DELTAX) THEN
            WRITE(6,100) NAME, X
            write(6,*) 'Index= ',I
            gtop_too_big = .TRUE.
            write(6,*) 'Shorting out of this point'
            RETURN
          END IF
          GO TO 12
        END IF
    8 CONTINUE
      LER=.TRUE.
      DO 7 I = 1,N
    7   LER=LER .AND. W(I,2) .LT. EPS0*W(I,1)
      DO 9 I = 1,N
    9   Y(I)=W(I,0)
      IF(LER) THEN
        H0=H1+H1
        H1=HH+HH
      END IF
      IF(LFN) RETURN
      X=X1
      GO TO 12
  100 FORMAT(1X,'***** CERN D202 ',A,' ... TOO HIGH ACCURACY ',
     1          'REQUIRED NEAR  X = ',E15.8)
      END

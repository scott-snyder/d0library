      SUBROUTINE ELLIPSE_INTERSECT(A,B,NSOL,RINT,IERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SOLVES THE INTERSECTION OF TWO ELLIPSES
C-
C-   Inputs  : A,B DESCRIBES TWO ELLIPSES
C-   Outputs : NSOL = NUMBER OF SOLUTIONS
C-   RINT(2,4) = X,Y POINTS OF THE SOLUTIONS
C-   Controls: ELLIPSE EQUATION IS A(1)*X**2 + A(2)*Y**2 + A(3)X*Y + 
C-   A(4)*X + A(5)*Y + A(6) = 0, SAME FOR B
C-   IERROR = non-zero, error has occurred
C-
C-   Created  17-MAR-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION    A(*),B(*),RINT(2,*)
      INTEGER NSOL
      DOUBLE PRECISION    A1,A2,A3,A4,A5,A6
      DOUBLE PRECISION    B1,B2,B3,B4,B5,B6
      DOUBLE PRECISION    Q0,Q1,Q2,Q3,Q4 ,C(4)
      DOUBLE PRECISION    SOLQ(4)
      INTEGER I
      DOUBLE PRECISION    X,Y
      COMPLEX*16 Z(4)
      DOUBLE PRECISION AA,BB,CC,DD,DC
      INTEGER MT
C
      INTEGER IER,IERROR
      INTEGER ILEN,INUM,IPT,ERROR(100)
      LOGICAL LERR(100)
      INTEGER ERR_NUM,ERR_FLG
      EQUIVALENCE (LERR,ERROR)
C
      DOUBLE PRECISION    CC1,CC2,DD1,DD2
C
      LOGICAL first
      SAVE first
      DATA first / .true. /
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        CALL EZGETA('ERROR_HANDLING',0,0,0,ILEN,IER)
        CALL EZGETA('ERROR_HANDLING',1,ILEN,1,ERROR,IER)
      ENDIF
C
      A1=A(1)
      B1=B(1)
      A2=A(2)
      B2=B(2)
      A3=A(3)
      B3=B(3)
      A4=A(4)
      B4=B(4)
      A5=A(5)
      B5=B(5)
      A6=A(6)
      B6=B(6)
C
      Q4 = (A1*A2*(B3**2-2*B1*B2)+A3*(-A1*B2*B3-A2*B1*B3)+A1**2*B2**2+A
     1   3**2*B1*B2+A2**2*B1**2)
C
      Q3 = +(A3*(A1*(-B3*B5-B2*B4)-A2*B1*B4)+
     &  A1*A2*(2*B3*B4-2*B1*B5)+2*A1**2*B2*B5+
     &  A3**2*B1*B5+A5*(A1*(B3**2-2*B1*B2)-A3*B1*B3+2*A2*B1**2)+
     &  A4*(-A1*B2*B3-A2*B1*B3+2*A3*B1*B2))
C
      Q2 = +(A1*A3*(-B3*B6-B4*B5)+A1**2*(2*B2*B6+B5**2)+A1*A2*(B4**
     5   2-2*B1*B6)+A3**2*B1*B6+A4*(A1*(-B3*B5-B2*B4)+2*A3*B1*B5-A2*B1*B
     6   4)+A5*(A1*(2*B3*B4-2*B1*B5)-A3*B1*B4-A4*B1*B3)+A6*(A1*(B3**2-2*
     7   B1*B2)-A3*B1*B3+2*A2*B1**2)+A4**2*B1*B2+A5**2*B1**2)
C
      Q1 = +(A4*(A1*(-B3*B6-B4*B5)+2*A3*B1*B6)+
     &  A5*(A1*(B4**2-2*B1*B6)-A4*B1*B4)+
     9   2*A1**2*B5*B6-A1*A3*B4*B6+A6*(A1*(2*B3*B4-2*B1*B5)-
     9   A3*B1*B4-A4*B1*B3+2*A5*B1**2)+A4**2*B1*B5)
C
      Q0 = +A1**2*B6**2+A6*(A1*(B4**2-2*B1*B6)-A4*B1*B4)-
     &  A1*A4*B4*B6+A4**2*B1*B6+A6**2*B1**2
C
      C(1) = Q3/Q4
      C(2) = Q2/Q4
      C(3) = Q1/Q4
      C(4) = Q0/Q4
C
      AA=C(1)
      BB=C(2)
      CC=C(3)
      DD=C(4)
C
      CALL DRTEQ4(AA,BB,CC,DD,Z,DC,MT)
C
      IERROR = 0
      NSOL = 0
      INUM = ILEN/6
      IPT = 1
      DO I = 1 , INUM
        ERR_NUM = ERROR(IPT)
        IPT = IPT + 6
        CALL ERRTST(ERR_NUM,ERR_FLG)
        IF ( ERR_FLG.EQ.1 ) THEN
          IERROR = ERR_NUM   !ERROR HAS OCCURRED
        ENDIF
C TESTING AND RESETTING ALL ERRORS IN LIST
      ENDDO
      IF ( IERROR.NE.0 ) THEN
        RETURN
      ENDIF
C
C
      IF ( MT.EQ.1 ) THEN
        NSOL =4
      ELSEIF ( MT.EQ.2 ) THEN
        NSOL=0
      ELSEIF ( MT.EQ.3 ) THEN
        NSOL = 2
      ENDIF
      DO I = 1, NSOL
        SOLQ(I) = Z(I)
      ENDDO
C
      DO I = 1 , NSOL
C
C ****  SOLQ CONTAINS THE Y CORDINATES.
C
        Y = SOLQ(I)
        CC1 = A(3)*Y+A(4)
        CC2 = B(3)*Y+B(4)
        DD1 = A(2)*Y*Y+A(5)*Y+A(6)
        DD2 = B(2)*Y*Y+B(5)*Y+B(6)
        X = (-A(1)*DD2+B(1)*DD1)/(A(1)*CC2-B(1)*CC1)
        RINT(1,I) = X
        RINT(2,I) = Y
      ENDDO
C
  999 RETURN
      END

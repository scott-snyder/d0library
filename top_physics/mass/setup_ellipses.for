      SUBROUTINE SETUP_ELLIPSES(IERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SETUP THE QUANTITIES FOR THE TWO ELLIPSES FOR THE
C-   NEUTRINO ET SUCH THAT THE TOTAL MISSING ET IS THE ONE OBSERVED.
C-
C-   Inputs  :
C-   Outputs :IERROR NON ZERO. OVERFLOW WHEN SETTING UP ELLIPSES
C-   Controls:
C-
C-   Created  17-MAR-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TOP_SOLNSE.INC'
      INCLUDE 'D0$INC:KINEQ.INC'
      DOUBLE PRECISION    BB(4),L(4),AA(4),A1,B1,C1,A(6),B(3),
     &  C(6),DD(6,2),D(6),E(6),F(6),G(2),KK(2),BBB(4,2),LL(4,2)
      DOUBLE PRECISION    K, BSQ
      INTEGER ITOP,I
      INTEGER NSOL,ISOL
      DOUBLE PRECISION    NEUT(2,4),DEL1,DEL2,X,Y,Z
      DOUBLE PRECISION    DOT2
      REAL TOLW,TOLT
      INTEGER IERR
      INTEGER IER,IERROR
      LOGICAL first
      SAVE first
      DATA first / .true. /
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        CALL EZPICK('TOP_MASS_RCP')
        CALL EZGET('TOLERANCE_W_MASS',TOLW,IER)
        CALL EZGET('TOLERANCE_TOP_MASS',TOLT,IER)
        CALL EZRSET
      ENDIF
      IERROR = 0
      DO ITOP = 1 , 2
        DO I = 1 , 4
          L(I) = LEPTON1(I)
          BB(I) = JET1(I) + L(I)
          IF ( ITOP.EQ.2 ) THEN
            L(I) = LEPTON2(I)
            BB(I) = JET2(I) + L(I)
          ENDIF
          L(I) = L(I)/WMASS
          BB(I) = BB(I)/WMASS
          BBB(I,ITOP) = BB(I)
          LL(I,ITOP) = L(I)
        ENDDO
C
        BSQ = BB(4)**2-BB(3)**2-BB(2)**2-BB(1)**2
        K = 0.5*(BB(4) - ((TMASSE*TMASSE)/(WMASS*WMASS)-BSQ)*L(4))
        KK(ITOP) = K
C
        DO I = 1 , 3
          AA(I) = L(4)*BB(I) - BB(4)*L(I)
        ENDDO
C
        A1 = -AA(1)/AA(3)
        B1 = -AA(2)/AA(3)
        C1 = K/AA(3)
        A(1) = A1*A1 + 1.0
        A(2) = B1*B1 + 1.0
        A(3) = 2.0*A1*B1
        A(4) = 2.0*A1*C1
        A(5) = 2.0*B1*C1
        A(6) = C1*C1
        DO I = 1 , 6
          A(I) = A(I)*L(4)*L(4)
        ENDDO
        B(1) = A1*L(3) + L(1)
        B(2) = B1*L(3) + L(2)
        B(3) = C1*L(3) + 1.0/2.0
        C(1) = B(1)*B(1)
        C(2) = B(2)*B(2)
        C(3) = 2.0*B(1)*B(2)
        C(4) = 2.0*B(3)*B(1)
        C(5) = 2.0*B(3)*B(2)
        C(6) = B(3)*B(3)
C
        DO I = 1 , 6
          DD(I,ITOP) = A(I) - C(I)
        ENDDO
      ENDDO
C
      DO I = 1 , 6
        D(I) = DD(I,1) !FOR 1ST TOP
        E(I) = DD(I,2) !FOR 2ND TOP
      ENDDO
C
      G(1) = PNUT(1)/WMASS
      G(2) = PNUT(2)/WMASS
C
      F(1) = E(1)
      F(2) = E(2)
      F(3) = E(3)
      F(4) = -(2.0*G(1)*E(1)+G(2)*E(3)+E(4))
      F(5) = -(2.0*G(2)*E(2)+G(1)*E(3)+E(5))
      F(6) = E(1)*G(1)*G(1) + E(2)*G(2)*G(2) + E(3)*G(1)*G(2) +
     &  E(4)*G(1) + E(5)*G(2) + E(6)
C
C
C ****  NOW TO FIND THE INTERSECTIONBETWEENTWO ELIPSES DEFINED BY D AND F.
C ****  THE INTERSECTION GIVES THE NEUTRINO X AND Y COMPONENTS OF 1ST
C ****  NEUTRINO. ELLIPSE EQUATION IS
C ****  D(1)*X*X + D(2)*Y*Y D(3)*X*Y + D(4)*X + D(5)*Y + D(6) = 0
C
C
      CALL ELLIPSE_INTERSECT(D,F,NSOL,NEUT,IERROR)
      IF(IERROR.NE.0)RETURN
C
      NSOLS(NTOPS)=NSOL
C
      IERR = 0
      DO ISOL =  1, NSOL
        X=NEUT(1,ISOL)
        Y=NEUT(2,ISOL)
        DEL1 = D(1)*X*X + D(2)*Y*Y + D(3)*X*Y + D(4)*X + D(5)*Y +
     &      D(6)
C
        DEL2 = F(1)*X*X + F(2)*Y*Y + F(3)*X*Y + F(4)*X + F(5)*Y +
     &      F(6)
        ITOP = 1
        DOT2=(LL(4,ITOP)*BBB(1,ITOP)-BBB(4,ITOP)*LL(1,ITOP))*X+
     &    (LL(4,ITOP)*BBB(2,ITOP)-BBB(4,ITOP)*LL(2,ITOP))*Y
C
        Z = (KK(ITOP)-DOT2)/
     &    (LL(4,ITOP)*BBB(3,ITOP)-BBB(4,ITOP)*LL(3,ITOP))
C
        NEUT1(1,ISOL,NTOPS) = X*WMASS
        NEUT1(2,ISOL,NTOPS) = Y*WMASS
        NEUT1(3,ISOL,NTOPS) = Z*WMASS
        NEUT1(4,ISOL,NTOPS) = SQRT(X*X+Y*Y+Z*Z)*WMASS
C
        X = G(1)-X
        Y = G(2)-Y   !2ND NEUTRINO
        ITOP = 2
C
        DOT2=(LL(4,ITOP)*BBB(1,ITOP)-BBB(4,ITOP)*LL(1,ITOP))*X+
     &    (LL(4,ITOP)*BBB(2,ITOP)-BBB(4,ITOP)*LL(2,ITOP))*Y
C
        Z = (KK(ITOP)-DOT2)/
     &    (LL(4,ITOP)*BBB(3,ITOP)-BBB(4,ITOP)*LL(3,ITOP))
C
        NEUT2(1,ISOL,NTOPS) = X*WMASS
        NEUT2(2,ISOL,NTOPS) = Y*WMASS
        NEUT2(3,ISOL,NTOPS) = Z*WMASS
        NEUT2(4,ISOL,NTOPS) = SQRT(X*X+Y*Y+Z*Z)*WMASS
C
        DO I = 1 , 4
          WB1(I,ISOL,NTOPS) = LEPTON1(I) + NEUT1(I,ISOL,NTOPS)
          T1(I,ISOL,NTOPS) = WB1(I,ISOL,NTOPS)+JET1(I)
          WB2(I,ISOL,NTOPS) = LEPTON2(I) + NEUT2(I,ISOL,NTOPS)
          T2(I,ISOL,NTOPS) = WB2(I,ISOL,NTOPS)+JET2(I)
        ENDDO
C
        IERR = 0
        CALL TEST_MASS('WMASS 1',WB1(1,ISOL,NTOPS),WMASS,TOLW,IER)
        IERR = IERR + IER
        CALL TEST_MASS('TOP_MASS1',T1(1,ISOL,NTOPS),
     &    TOP_MASS(NTOPS),TOLT,IER)
        IERR = IERR + IER
        CALL TEST_MASS('WMASS2',WB2(1,ISOL,NTOPS),WMASS,TOLW,IER)
        IERR = IERR + IER
        CALL TEST_MASS('TOP_MASS2',T2(1,ISOL,NTOPS),
     &    TOP_MASS(NTOPS),TOLT,IER)
        IERR = IERR + IER
C
      ENDDO
C
      IF ( IERR.NE.0 ) THEN
        NSOLS(NTOPS) = 0  !DO NOT USE THIS MASS SOLUTION
      ENDIF
C
  999 RETURN
      END

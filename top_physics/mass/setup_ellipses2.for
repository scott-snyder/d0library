      SUBROUTINE SETUP_ELLIPSES2
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SETUP THE QUANTITIES FOR THE TWO ELLIPSES FOR THE
C-   NEUTRINO ET SUCH THAT THE TOTAL MISSING ET IS THE ONE OBSERVED.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  17-MAR-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:KINEQ.INC'
      INCLUDE 'D0$INC:TOP_SOLNSE.INC'
      REAL    BB(4),L(4),D(6),E(6),G1,G2
      REAL    K, L1,L2,L3,L4,B1,B2,B3,B4,BSQ
      INTEGER ITOP,I,ISOL
      INTEGER NSOL
      REAL    NEUT(2,4),DEL1,DEL2,X,Y
C----------------------------------------------------------------------
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
        ENDDO
        BSQ = BB(4)**2-BB(1)**2-BB(2)**2-BB(3)**2
        K = (TMASSE/WMASS)**2-BSQ
        G1 = PNUT(1)/WMASS
        G2 = PNUT(2)/WMASS
        L1 = L(1)
        L2 = L(2)
        L3 = L(3)
        L4 = L(4)
C
        B1 = BB(1)
        B2 = BB(2)
        B3 = BB(3)
        B4 = BB(4)
C
        IF ( ITOP.EQ.1 ) THEN
        D(1) = -16*L4**2*(B3**2*L4**2+B1**2*L4**2-
     &    2*B3*B4*L3*L4-2*B1*B4*L1*L4+B4**2*L3**2-B1**2*L3**2+
     &    2*B1*B3*L1*L3+B4**2*L1**2-B3**2*L1**2)

        D(2) = -16*L4**2*(B3**2*L4**2+B2**2*L4**2-
     &    2*B3*B4*L3*L4-2*B2*B4*L2*L4+B4**2*L3**2-
     &    B2**2*L3**2+2*B2*B3*L2*L3+B4**2*L2**2-B3**2*L2**2)

        D(3) = -32*L4**2*(B1*B2*L4**2-B1*B4*L2*L4-B2*B4*L1*L4-B1*B2*L
     3    3**2+B1*B3*L2*L3+B2*B3*L1*L3+B4**2*L1*L2-B3**2*L1*L2)

        D(4) = -16*L4**2*(B1*K*L4**2-B4*K*L1*L4-B1*B4*L4-
     &    B1*K*L3**2+B3*K*L1*L3+B1*B3*L3+B4**2*L1-B3**2*L1)

        D(5) = -16*L4**2*(B2*K*L4**2-B4*K*L2*L4-B2*B4*L4-B2*K*L3**2+
     4    B3*K*L2*L3+B2*B3*L3+B4**2*L2-B3**2*L2)

        D(6) = -4*L4**2*(K*L4-K*L3-B4+B3)*(K*L4+K*L3-B4-B3)
C
        ELSE
        E(1) = -16*L4**2*(B3**2*L4**2+B1**2*L4**
     <    2-2*B3*B4*L3*L4-2*B1*B4*L1*L4+B4**2*L3**2-B1**2*L3**2+
     <    2*B1*B3*L1*L3+B4**2*L1**2-B3**2*L1**2)
C
        E(2) = -16*L4**2*(B3**2*L4**2+B2**2*L4**2-2*B
     :    3*B4*L3*L4-2*B2*B4*L2*L4+B4**2*L3**2-B2**2*L3**2+
     :    2*B2*B3*L2*L3+B4**2*L2**2-B3**2*L2**2)
C
        E(3) = -32*L4**2*(B1*B2*L4**2-B1*B4
     >    *L2*L4-B2*B4*L1*L4-B1*B2*L3**2+B1*B3*L2*L3+
     >    B2*B3*L1*L3+B4**2*L1*L2-B3**2*L1*L2)
C
        E(4) = -16*L4**2*(B1*K*L4**2-2*B1*B2*G2*L4**2-
     &    2*B3**2*G1*L4**2-2*B1**2*G1*L4**2+4*B3*B4*G1*L3*L4+
     &    2*B1*B4*G2*L2*L4-B4*K*L1*L4+2*B2*B4*G2*L1*L4+
     &    4*B1*B4*G1*L1*L4-B1*B4*L4-B1*K*L3**2+2*B1*B2*G2*L3**2-
     &    2*B4**2*G1*L3**2+2*B1**2*G1*L3**2-2*B1*B3*G2*L2*L3+
     &    B3*K*L1*L3-2*B2*B3*G2*L1*L3-4*B1*B3*G1*L1*L3+B1*B3*L3-
     &    2*B4**2*G2*L1*L2+2*B3**2*G2*L1*L2-2*B4**2*G1*L1**2+
     &    2*B3**2*G1*L1**2+B4**2*L1-B3**2*L1)
C
        E(5) = -16*L4**2*(B2*K*L4**2-2*B3**2*G2*L4**2-
     &    2*B2**2*G2*L4**2-2*B1*B2*G1*L4**2+4*B3*B4*G2*L3*L4-
     &    B4*K*L2*L4+4*B2*B4*G2*L2*L4+2*B1*B4*G1*L2*L4+
     &    2*B2*B4*G1*L1*L4-B2*B4*L4-B2*K*L3**2-2*B4**2*G2*L3**2+
     &    2*B2**2*G2*L3**2+2*B1*B2*G1*L3**2+B3*K*L2*L3-
     &    4*B2*B3*G2*L2*L3-2*B1*B3*G1*L2*L3-2*B2*B3*G1*L1*L3+
     &    B2*B3*L3-2*B4**2*G2*L2**2+2*B3**2*G2*L2**2-
     &    2*B4**2*G1*L1*L2+2*B3**2*G1*L1*L2+B4**2*L2-B3**2*L2)
C
        E(6) = -4*L4**2*(K**2*L4**2-4*B2*G2*K*L4**2-4*B1*G1*K*L4**2+
     &    4*B3**2*G2**2*L4**2+4*B2**2*G2**2*L4**2+8*B1*B2*G1*G2*L4**2+
     &    4*B3**2*G1**2*L4**2+4*B1**2*G1**2*L4**2-8*B3*B4*G2**2*L3*L4-
     &    8*B3*B4*G1**2*L3*L4+4*B4*G2*K*L2*L4-8*B2*B4*G2**2*L2*L4-
     &    8*B1*B4*G1*G2*L2*L4+4*B4*G1*K*L1*L4-8*B2*B4*G1*G2*L1*L4-
     &    8*B1*B4*G1**2*L1*L4-2*B4*K*L4+4*B2*B4*G2*L4+4*B1*B4*G1*L4-
     &    K**2*L3**2+4*B2*G2*K*L3**2+4*B1*G1*K*L3**2+
     &    4*B4**2*G2**2*L3**2-
     &    4*B2**2*G2**2*L3**2-8*B1*B2*G1*G2*L3**2+4*B4**2*G1**2*L3**2-
     &    4*B1**2*G1**2*L3**2-4*B3*G2*K*L2*L3+8*B2*B3*G2**2*L2*L3+
     &    8*B1*B3*G1*G2*L2*L3-4*B3*G1*K*L1*L3+8*B2*B3*G1*G2*L1*L3+
     &    8*B1*B3*G1**2*L1*L3+2*B3*K*L3-4*B2*B3*G2*L3-4*B1*B3*G1*L3+
     &    4*B4**2*G2**2*L2**2-4*B3**2*G2**2*L2**2+8*B4**2*G1*G2*L1*L2-
     &    8*B3**2*G1*G2*L1*L2-4*B4**2*G2*L2+4*B3**2*G2*L2+
     &    4*B4**2*G1**2*L1**2-4*B3**2*G1**2*L1**2-4*B4**2*G1*L1+
     &    4*B3**2*G1*L1+B4**2-B3**2)
        ENDIF
C
C ****  NOW TO FIND THE INTERSECTIONBETWEENTWO ELIPSES DEFINED BY D AND F.
C ****  THE INTERSECTION GIVES THE NEUTRINO X AND Y COMPONENTS OF 1ST
C ****  NEUTRINO. ELLIPSE EQUATION IS
C ****  D(1)*X*X + D(2)*Y*Y D(3)*X*Y + D(4)*X + D(5)*Y + D(6) = 0
C
C
        ENDDO
C
        CALL ELLIPSE_INTERSECT(D,E,NSOL,NEUT)
C
        DO ISOL =  1, NSOL
          X=NEUT(1,ISOL)
          Y=NEUT(2,ISOL)
          DEL1 = D(1)*X*X + D(2)*Y*Y + D(3)*X*Y + D(4)*X + D(5)*Y +
     &      D(6) 
C
          DEL2 = E(1)*X*X + E(2)*Y*Y + E(3)*X*Y + E(4)*X + E(5)*Y +
     &      E(6) 
        ENDDO
  999   RETURN
        END

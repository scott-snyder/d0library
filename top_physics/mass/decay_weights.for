      DOUBLE PRECISION FUNCTION DECAY_WEIGHTS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate the weights due to decay of
C-   Top quark and antiquark
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  14-FEB-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PARTON_KINE.INC'
      INCLUDE 'D0$INC:KINEQ.INC'
      DOUBLE PRECISION  B,FACT,PL,K1,K2,B1,B2,K,D1,D2
      INTEGER I
C----------------------------------------------------------------------
C
      FACT(B) = (TMASS**2-B**2)**2 + WMASS*WMASS*(TMASS**2 + B**2)
     &  -2.0*WMASS**4
      PL(K,B) = 4.0*K*(TMASS**2-B**2-2.0*K)/FACT(B)
C
      K1 = LEPTON1(4)*JET1(4)
      K2 = LEPTON2(4)*JET2(4)
      DO I = 1 , 3
        K1 = K1 - LEPTON1(I)*JET1(I)
        K2 = K2 - LEPTON2(I)*JET2(I)
      ENDDO
C
      K1 = K1 + WMASS*WMASS/2.
      K2 = K2 + WMASS*WMASS/2.
C
C
C ****  B MASS = JET EFFECTIVE MASS
C
      B1 = SQRT(JET1(4)**2 - JET1(3)**2 - JET1(2)**2 -JET1(1)**2)
      B2 = SQRT(JET2(4)**2 - JET2(3)**2 - JET2(2)**2 -JET2(1)**2)
C
      D1 = PL(K1,B1)
      D2 = PL(K2,B2)
C
      DECAY_WEIGHTS = D1*D2
C
      IF ( TESTQ ) THEN
        L1 = K1/TMASS
        L2 = K2/TMASS
        CALL DO_HF1D(206,L1,D1)
        CALL DO_HF1D(207,L2,D2)
C
        CALL DO_HF1D(306,L1,1.0D0)
        CALL DO_HF1D(307,L2,1.0D0)

      ENDIF
C
  999 RETURN
      END

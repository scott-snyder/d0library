      SUBROUTINE SWAP_LEPTON12
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SWAP LEPTONS 1 AND 2
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  28-JAN-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:KINEQ.INC'
      DOUBLE PRECISION    TEMP
      INTEGER I
      INTEGER ITEMP
C----------------------------------------------------------------------
      DO I = 1 , 4
        TEMP = LEPTON1(I)
        LEPTON1(I) = LEPTON2(I)
        LEPTON2(I) = TEMP
      ENDDO
      ITEMP=LEPTON_TYPE(1)
      LEPTON_TYPE(1)=LEPTON_TYPE(2)
      LEPTON_TYPE(2)=ITEMP
  999 RETURN
      END

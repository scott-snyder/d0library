      SUBROUTINE NRAN(VECTOR,N)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Replace CERNLIB V105 to make use of RNDM only
C-
C-   Inputs  : VECTOR = vector to be filled with N random numbers
C-
C-   Created  28-FEB-1990   John Womersley - Extracted from CERNLIB GEN.CAR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,N
      REAL VECTOR(N),RNDM,V
C----------------------------------------------------------------------
      DO 100 I=1,N
        VECTOR(I) = RNDM(I)
  100 CONTINUE
      RETURN
C
      ENTRY NRANIN (V)
      CALL RDMIN(V)
      RETURN
C
      ENTRY NRANUT (V)
      CALL RDMOUT(V)
      RETURN
      END

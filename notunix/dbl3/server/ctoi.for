      INTEGER FUNCTION CTOI (C)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns numeric value corresponding
C-                         to numeric character
C-
C-   Inputs  : C    Input character
C-   Outputs :
C-   Controls:
C-
C-   Modified  07-OCT-1991   S. Abachi
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) C
      INTEGER L,KK,I,IASCII,K
C
      L = LEN(C)
      KK = 0
      DO 10,I=1,L
      IASCII = ICHAR(C(I:I))
*                       Watch out ! 240 for IBM, 48 for Vax
      IF(IASCII.LT.48.OR.IASCII.GT.57) IASCII = 48
      K=IASCII-48
      KK=KK+(K*(10**(L-I)))
   10 CONTINUE
      CTOI=KK
  999 RETURN
      END

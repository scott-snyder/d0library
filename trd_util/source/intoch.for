      FUNCTION INTOCH (N)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert integer n in character
C-
C-   Returned value  :
C-   Inputs  :N integer
C-   Outputs :Intoch: Character conversion of N
C-   Controls:
C-
C-   Created  31-JUL-1991   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER II,N,NRES
      CHARACTER*(*) INTOCH
      CHARACTER*1 C(10)
      DATA C/'0','1','2','3','4','5','6','7','8','9'/
C----------------------------------------------------------------------
      NRES=N
      INTOCH=' '
      DO WHILE(NRES.NE.0)
        II=MOD(NRES,10)
        INTOCH=C(II+1)//INTOCH
        NRES=NRES/10
      END DO
  999 RETURN
      END

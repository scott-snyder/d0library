      SUBROUTINE MUUPLN(IN,N1,N2,N3,N4,N5,N6,N7,N8,N9)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : uppacks word compressed modulo 10
C-
C-   Inputs  : IN input word
C-   Outputs : N1-N9 integer in each place
C-   Controls: 
C-
C-   Created   8-OCT-1992   David Hedin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IN,I,N1,N2,N3,N4,N5,N6,N7,N8,N9
      I=IN
      N1=MOD(I,10)
      I=I/10
      N2=MOD(I,10)
      I=I/10
      N3=MOD(I,10)
      I=I/10
      N4=MOD(I,10)
      I=I/10
      N5=MOD(I,10)
      I=I/10
      N6=MOD(I,10)
      I=I/10
      N7=MOD(I,10)
      I=I/10
      N8=MOD(I,10)
      I=I/10
      N9=MOD(I,10)
C----------------------------------------------------------------------
  999 RETURN
      END

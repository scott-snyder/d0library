      FUNCTION EZZAND(II,JJ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Do (II .AND. JJ). On the VAX use the function
C-   IAND. Note, however, that this is NOT standard F77.
C-
C-   Returned value  : The AND of II and JJ
C-   Inputs  : II,JJ    [I]     Integers to be ANDED
C-   Outputs : 
C-   Controls: 
C-
C-   Created  11-MAY-1990   Harrison B. Prosper
C-   Updated  12-Feb-1992   Herbert Greenlee
C-      UNIX version.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER II,JJ
      INTEGER EZZAND,EZZSHFT
C----------------------------------------------------------------------
      EZZAND = IAND(II,JJ)
      RETURN
C
      ENTRY EZZSHFT(II,JJ)
      EZZSHFT= ISHFT(II,JJ)
  999 RETURN
      END

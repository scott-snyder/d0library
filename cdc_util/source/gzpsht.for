      INTEGER FUNCTION GZPSHT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get the pointer to PSHT
C-
C-   Returned value  : Bank addess
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created  23-OCT-1995   Hailin Li
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPSHT.LINK'
      INTEGER LUPGD,GZUPGD
C----------------------------------------------------------------------
      GZPSHT = 0
C
      LUPGD = GZUPGD()
      IF ( LUPGD .LE. 0 ) RETURN
      GZPSHT = LQ(LUPGD-IZPSHT)
  999 RETURN
      END

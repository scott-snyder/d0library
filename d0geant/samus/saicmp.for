      FUNCTION SAICMP(I, J)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compare two integer values for QSORT routine
C-
C-   Returned value  : -1 if (i < j), 0 if (i==j), 1 if (i > j)
C-   Inputs  : Two integer values
C-   Outputs : None
C-   Controls: None
C-
C-   Created  16-OCT-1990   V. Glebov & V. Podstavkov
C-   Updated  21-MAR-1991   V. GLEBOV
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C&IF VAXVMS,IBMAIX,ULTRIX,ALFOSF
      INTEGER SAICMP
C&ENDIF
C&IF SIUNIX,LINUX
C&      INTEGER*2 SAICMP
C&ENDIF
      INTEGER I(3), J(3), II, JJ
      II = IAND (I(1),'377'O)
      JJ = IAND (J(1),'377'O)
      IF     (II .LT. JJ) THEN
        SAICMP = -1
      ELSEIF (II .EQ. JJ) THEN
        SAICMP = 0
      ELSE
        SAICMP = 1
      ENDIF
  999 RETURN
      END

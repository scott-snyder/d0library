      INTEGER FUNCTION GZUCWX()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return link to UCWX bank
C-
C-   Returned value  :  link to UCWX
C-   Inputs  :          NONE
C-   Outputs :          NONE
C-   Controls:          NONE
C-
C-   Created   7-DEC-1993   Ian Adam
C-   Updated  14-OCT-1995   Ian Adam  modify UCSH->UCWX 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZUCWX.LINK'
      INTEGER LANLS,GZANLS
C----------------------------------------------------------------------
C- Check that ANLS exists; if so, check that there are enough links so
C- that UCWX could exist.

      LANLS=GZANLS()
      IF (LANLS.LE.0) THEN
        GZUCWX=0
      ELSE
        IF (IQ(LANLS-3).GE.IZUCWX) THEN
          GZUCWX=LQ(LANLS-IZUCWX)
        ELSE
          GZUCWX=0
        ENDIF
      ENDIF

  999 RETURN
      END

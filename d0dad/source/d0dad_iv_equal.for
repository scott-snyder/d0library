      LOGICAL FUNCTION D0DAD_IV_EQUAL(IV1,IV2,ILEN)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Compare two integer vectors.  If they are
C-    equal, return true.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  26-Jul-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IV1(*),IV2(*),ILEN,I
C-----------------------------------------------------------------------
C
      D0DAD_IV_EQUAL=.TRUE.
      DO I=1,ILEN
        IF( IV1(I).NE.IV2(I) ) GOTO 901
      ENDDO
      RETURN
C
 901  CONTINUE
      D0DAD_IV_EQUAL=.FALSE.
      RETURN
C
      END

      SUBROUTINE ECDUPS(IARR,NMAX,NELE,IKEY,IKEY2,NDUP)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Remove duplicate entries from a sorted 2D
C-      array.
C-
C-   Inputs  : IARR(NELE,NMAX) - Array to be checked
C-             IKEY  - Field to check
C-             IKEY2 - If duplicates, resolve with this field
C-   Outputs : NDUP  - Number of duplicates
C-   Controls:
C-
C-   Created  13-Jan-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NELE,NMAX,IARR(NELE,NMAX),IKEY,NDUP,IKEY2,I,J
C-----------------------------------------------------------------------
C
      NDUP=0
      DO 30 I=1,NMAX-1
         IF( I.GE.(NMAX-NDUP) ) GOTO 30
         IF( IARR(IKEY,I).NE.IARR(IKEY,I+1) ) GOTO 30
         NDUP=NDUP+1
         IF( IARR(IKEY2,I+1).LT.IARR(IKEY2,I) ) 
     +      CALL UCOPY(IARR(1,I+1),IARR(1,I),NELE)
         DO J=I+1,NMAX-NDUP
            CALL UCOPY(IARR(1,J+1),IARR(1,J),NELE)
         ENDDO
 30   CONTINUE
C
  999 RETURN
      END

      SUBROUTINE JVSAVE(ARRAY)
C
C    Purpose:
CD   The purpose of this module is to save the viewing transformation
CD   information in the area ARRAY.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 09-Nov-1988
CH   History:
CH      09-NOV-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      GRFPAR-R
C
C    Calls:
CC      NONE.
C
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      REAL ARRAY(85)
C
C    Then local declarations of variables (non-common variables).
C
      INTEGER I, NSEL
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
C
C    Then executable code follows
C
      NSEL = 1
      DO 10 I=1,6
         ARRAY(NSEL)   = UWIND(I)
         ARRAY(NSEL+6) = UVIEW(I)
         NSEL = NSEL + 1
   10 CONTINUE
      NSEL = NSEL + 6
      DO 20 I=1,3
         ARRAY(NSEL)   = VUPNT(I)
         ARRAY(NSEL+3) = NORML(I)
         ARRAY(NSEL+6) = UPVEC(I)
         NSEL = NSEL + 1
   20 CONTINUE
      NSEL = NSEL + 6
      IF (WCLIP) THEN
         ARRAY(NSEL) = 1.0
      ELSE
         ARRAY(NSEL) = 0.0
      ENDIF
      NSEL = NSEL + 1
      IF (HCLIP) THEN
         ARRAY(NSEL) = 1.0
      ELSE
         ARRAY(NSEL) = 0.0
      ENDIF
      NSEL = NSEL + 1
      IF (YCLIP) THEN
         ARRAY(NSEL) = 1.0
      ELSE
         ARRAY(NSEL) = 0.0
      ENDIF
      NSEL = NSEL + 1
      ARRAY(NSEL) = RIGHT
      RETURN
      END

      SUBROUTINE JTTYPE(TTYPE)
C
C    Purpose:
CD   The purpose of this module is to designate the type of 2D image 
CD   transformation to apply to subsequently created retained segments.
CD   The parameter passed is the particular type of transformation to
CD   apply to the segment. Valid values are:
CD      TTYPE = 0 -->  No image transformation applied.
CD      TTYPE = 1 -->  Just apply a 2D translation.
CD      TTYPE = 2 -->  Apply the full 2D transformation matrix.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 23-Sep-1988
CH   History:
CH      23-SEP-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      SEGINF-W
C
C    Calls:
CC      ERROR
C
C    Next is the declaration of parameters passed to the subroutine/function.
      INTEGER TTYPE
C
C    Then local declarations of variables (non-common variables).
C

C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
C
C    Then executable code follows
C
      IF (SEGOPN) THEN
         CALL ERROR('JTTYPE: A SEGMENT IS OPEN')
      ENDIF
      IF (TTYPE .LT. 0 .OR. TTYPE .GT. 2) THEN
         CALL ERROR('JTTYPE: TRTYPE IS NOT IN RANGE')
      ENDIF
      CTTYPE = TTYPE
      RETURN
      END

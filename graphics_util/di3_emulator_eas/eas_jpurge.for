      SUBROUTINE JPURGE(NAMSEG)
C
C    Purpose:
CD   This module is used to delete a retained segment. The parameter
CD   passed is the segment name to delete.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 16-Sep-1988
CH   History:
CH      16-SEP-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      SEGINF-W
C
C    Calls:
CC      ERROR, KDELS
C
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      INTEGER NAMSEG
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
         CALL ERROR('JPURGE: A SEGMENT IS OPEN')
      ENDIF
      CALL KDELS(NAMSEG)
      RETURN
      END

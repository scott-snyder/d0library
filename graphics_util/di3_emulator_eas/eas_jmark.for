      SUBROUTINE JMARK(XPOS, YPOS)
C
C    Purpose:
CD   This module displays the current marker symbol at location
CD   (XPOS, YPOS, CPZ).
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 30-Aug-1988
CH   History:
CH      30-AUG-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      SEGINF-R, PRIMVR-W
C
C    Calls:
CC      ERROR, KMARK
C
C    Next is the declaration of parameters passed to the subroutine/function.
      REAL XPOS, YPOS
C
C    Then local declarations of variables (non-common variables).
C
 
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:PRIMVR.INC/LIST'
C
C    Then executable code follows
C
      IF (SEGOPN) THEN
         CALL KMARK(XPOS, YPOS, CPZ)
         CPX = XPOS
         CPY = YPOS
      ELSE
         CALL ERROR('JMARK: NO SEGMENT IS OPEN')
      ENDIF
      RETURN
      END

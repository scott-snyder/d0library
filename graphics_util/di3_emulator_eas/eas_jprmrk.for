      SUBROUTINE JPRMRK(XOFF, YOFF, NOFF)
C
C    Purpose:
CD   This module displays the current marker symbol at locations 
CD   specified by arrays XOFF, YOFF and CPZ.
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
      INTEGER NOFF
      REAL XOFF(NOFF), YOFF(NOFF)
C
C    Then local declarations of variables (non-common variables).
C
      INTEGER I
      REAL XPOS, YPOS
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:PRIMVR.INC/LIST'
C
C    Then executable code follows
C
      IF (SEGOPN) THEN
         IF (NOFF .GT. 0) THEN
            XPOS = CPX
            YPOS = CPY
            DO 10 I=1,NOFF
               XPOS = XPOS + XOFF(I)
               YPOS = YPOS + YOFF(I)
               CALL KMARK(XPOS, YPOS, CPZ)
   10       CONTINUE
            CPX = XPOS
            CPY = YPOS
         ELSE
            CALL ERROR('JPRMRK: N <= 0')
         ENDIF
      ELSE
         CALL ERROR('JPRMRK: NO SEGMENT IS OPEN')
      ENDIF
      RETURN
      END

      SUBROUTINE JPR3MR(XOFF, YOFF, ZOFF, NOFF)
C
C    Purpose:
CD   This module displays the current marker symbol at locations 
CD   specified by offset arrays XOFF, YOFF and ZOFF from the CP.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 08-Nov-1988
CH   History:
CH      08-NOV-88  ATV  Add right handed stuff.
CH      06-SEP-99  ATV  Correct spelling of subroutine name.
CH      30-AUG-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      GRFPAR-R, SEGINF-R, PRIMVR-W
C
C    Calls:
CC      ERROR, KMARK
C
C    Next is the declaration of parameters passed to the subroutine/function.
      INTEGER NOFF
      REAL XOFF(NOFF), YOFF(NOFF), ZOFF(NOFF)
C
C    Then local declarations of variables (non-common variables).
C
      INTEGER I
      REAL XPOS, YPOS, ZPOS
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:PRIMVR.INC/LIST'
C
C    Then executable code follows
C
      IF (SEGOPN) THEN
         IF (NOFF .GT. 0) THEN
            XPOS = CPX
            YPOS = CPY
            ZPOS = CPZ
            DO 10 I=1,NOFF
               XPOS = XPOS + XOFF(I)
               YPOS = YPOS + YOFF(I)
               ZPOS = ZPOS + (ZOFF(I) * RIGHT)
               CALL KMARK(XPOS, YPOS, ZPOS)
   10       CONTINUE
            CPX = XPOS
            CPY = YPOS
            CPZ = ZPOS
         ELSE
            CALL ERROR('JPR3MR: N <= 0')
         ENDIF
      ELSE
         CALL ERROR('JPR3MR: NO SEGMENT IS OPEN')
      ENDIF
      RETURN
      END

      SUBROUTINE JP3MRK(XPOS, YPOS, ZPOS, NPOS)
C
C    Purpose:
CD   This module displays the current marker symbol at locations 
CD   specified by arrays XPOS, YPOS and ZPOS.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 08-Nov-1988
CH   History:
CH      08-NOV-88  ATV  Add right handed stuff.
CH      13-SEP-88  ATV  Change the name to protect the innocent.
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
      INTEGER NPOS
      REAL XPOS(NPOS), YPOS(NPOS), ZPOS(NPOS)
C
C    Then local declarations of variables (non-common variables).
C
      INTEGER I
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
         IF (NPOS .GT. 0) THEN 
            DO 10 I=1,NPOS
               CALL KMARK(XPOS(I), YPOS(I), ZPOS(I) * RIGHT)
   10       CONTINUE
            CPX = XPOS(NPOS)
            CPY = YPOS(NPOS)
            CPZ = ZPOS(NPOS) * RIGHT
         ELSE
            CALL ERROR('JP3MRK: N <= 0')
         ENDIF
      ELSE
         CALL ERROR('JP3MRK: NO SEGMENT IS OPEN')
      ENDIF
      RETURN
      END

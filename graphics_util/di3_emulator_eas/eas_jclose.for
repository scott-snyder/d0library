C DEC/CMS REPLACEMENT HISTORY, Element JCLOSE.FOR
C *3    10-JUL-1989 04:05:43 ABACHI "UPDATE"
C *2    17-MAY-1989 17:21:47 ABACHI " "
C *1     6-DEC-1988 13:27:56 ABACHI "EAS EMULATOR ROUTINES"
C DEC/CMS REPLACEMENT HISTORY, Element JCLOSE.FOR
      SUBROUTINE JCLOSE
C
C    Purpose:
CD   This module closes the currently open temporary segment.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 26-Oct-1988
CH   History:
CH      26-OCT-88  ATV  Make changes to reflect new JOPEN
CH      30-SEP-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      SEGINF-W, PRIMVR-W
C
C    Calls:
CC      ERROR, PPURGE
C
      EXTERNAL ERRHND
C
C    Next is the declaration of parameters passed to the subroutine/function.
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
      SEGNUM = -1
      IF (SEGOPN) THEN
         CALL KUPDV
CCCC         CALL PPURGE(ERRHND)
         CPX = 0.0
         CPY = 0.0
         CPZ = 0.0
      ELSE
         CALL ERROR('JCLOSE: A SEGMENT IS NOT OPEN')
      ENDIF
      SEGOPN = .FALSE.
      RETURN
      END

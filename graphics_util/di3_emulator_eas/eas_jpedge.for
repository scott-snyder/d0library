      SUBROUTINE JPEDGE(CVALUE)
C
C    Purpose:
CD   This module sets the current polygon edge style. The parameter
CD   passed is an integer value (0..32767) indicates the current edge
CD   for all subsequent created polygons in the currently open segment.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 20-Oct-1988
CH   History:
CH      20-OCT-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      SEGINF-R, PLGATT-W
C
C    Calls:
CC      ERROR.
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      INTEGER CVALUE
C
C    Then local declarations of variables (non-common variables).
C
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:PLGATT.INC/LIST'
C
C    Then executable code follows
C
      IF (SEGOPN) THEN
         IF (CVALUE .LT. 0 .OR. CVALUE .GT. 32767) THEN
            CALL ERROR('JPEDGE: CVALUE OUT OF RANGE (0..32767)')
         ENDIF
         CPEDGE = CVALUE
      ELSE
         CALL ERROR('JPEDGE: NO SEGMENT IS OPEN')
      ENDIF
      RETURN
      END

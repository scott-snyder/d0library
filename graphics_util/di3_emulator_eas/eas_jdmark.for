      SUBROUTINE JDMARK(MARKER)
C
C    Purpose:
CD   This module selects the default marker symbol MARKER.
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
CB      SEGINF-R, LINATT-W
C
C    Calls:
CC      ERROR
C
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      INTEGER MARKER
C
C    Then local declarations of variables (non-common variables).
C
 
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:LINATT.INC/LIST'
C
C    Then executable code follows
C
      IF (SEGSOP) THEN
         CALL ERROR('JDMARK: CANNOT CHANGE MARKER AFTER SEG. OPEN')
      ELSE
         IF (MARKER .GT. 0 .AND. MARKER .LT. 32768) THEN
            DMARKR = MARKER
         ELSE
            CALL ERROR('JDMARK: DVALUE NOT IN RANGE (1..32767)')
         ENDIF
      ENDIF
      RETURN
      END

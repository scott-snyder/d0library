      SUBROUTINE JDVISB( VISFLG )
C
C    Purpose:
CD   This module changes the status of the default visibility flag.
CD   The parameter passed is the new visibility for retained segments.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 11-Oct-1988
CH   History:
CH      11-OCT-88  ATV  Initial entry.
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
C
      INTEGER VISFLG
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
      IF (VISFLG .EQ. 1 .OR. VISFLG .EQ. 0) THEN
         DVISIB = VISFLG
      ELSE
         CALL ERROR('JDVISB: VISFLG OUT OF RANGE (0..1)')
      ENDIF
      RETURN
      END

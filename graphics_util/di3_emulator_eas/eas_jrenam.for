      SUBROUTINE JRENAM(OLDNAM, NEWNAM)
C
C    Purpose:
CD   This module renames a segment. The parameters are a segment old 
CD   name (OLDNAM) and a segment new name (NEWNAM).
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 07-Nov-1988
CH   History:
CH      07-NOV-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      SEGINF-W
C
C    Calls:
CC      ERROR.
C
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      INTEGER OLDNAM, NEWNAM
C
C    Then local declarations of variables (non-common variables).
C
      INTEGER I, NLOC, OLOC
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
C
C    Then executable code follows
C
      IF (NEWNAM .LT. 1 .OR. NEWNAM .GT. 32767) THEN
         CALL ERROR('JRENAM: NEWNAM IS NOT IN RANGE (1..32767)')
      ENDIF
      NLOC = 0
      OLOC = 0
      DO 10 I=1,NSEGS
         IF (SEGINF(1,I) .EQ. NEWNAM) NLOC = I
         IF (SEGINF(1,I) .EQ. OLDNAM) OLOC = I
   10 CONTINUE
      IF (NLOC .GT. 0) THEN
         CALL ERROR('JRENAM: NEWNAM ALEADY EXISTS')
      ENDIF
      IF (OLOC .EQ. 0) THEN
         CALL ERROR('JRENAM: OLDNAM DOES NOT EXIST')
      ENDIF
      SEGINF(1,OLOC) = NEWNAM
      RETURN
      END

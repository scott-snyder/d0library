      SUBROUTINE JNORML(DXVEC, DYVEC, DZVEC)
C
C    Purpose:
CD   This module defines a normal viewing vector.  The parameters are 
CD   all real values in world coordinates.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 24-Oct-1988
CH   History:
CH      24-OCT-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      GRFPAR-W, SEGINF-R
C
C    Calls:
CC      ERROR.
C
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      REAL DXVEC, DYVEC, DZVEC
C
C    Then local declarations of variables (non-common variables).
C
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
C
C    Then executable code follows
C
      IF (SEGOPN) THEN
         CALL ERROR('JNORML: A SEGMENT IS OPEN')
      ENDIF
      IF (DXVEC .EQ. 0.0 .AND. DYVEC .EQ. 0.0 .AND. DZVEC .EQ. 0.0) THEN
         CALL ERROR('JNORML: VALUES PASSED DO NOT DEFINE A VECTOR')
      ENDIF
      NORML(1) = DXVEC
      NORML(2) = DYVEC
      NORML(3) = DZVEC
      RETURN
      END

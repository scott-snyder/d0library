      SUBROUTINE JDD3D(LVALUE)
C
C    Purpose:
CD   The purpose of this module is to declare the dimensionality.
CD   The parameter LVALUE is a logical value.  If the value passed
CD   evaluates to .TRUE., then coordinates are set to 3-D. If the 
CD   value passed evaluates to .FALSE. then 2-D is set.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 15-AUG-1988
CH   History:
CH      15-AUG-88  ATV  Initial entry.
C
      IMPLICIT NONE

C    Common blocks:
CB      SEGINF-R, PRIMVR-W
C
C    Calls:
CC      ERROR
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      LOGICAL LVALUE
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
      IF (.NOT. SEGOPN) THEN
         THREED = LVALUE
      ELSE
         CALL ERROR('JDD3D, A SEGMENT IS OPEN')
      ENDIF
      RETURN
      END

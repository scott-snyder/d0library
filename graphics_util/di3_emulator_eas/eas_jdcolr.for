        SUBROUTINE JDCOLR(ICOLOR)
C
C    Purpose:
CD   This module selects a color to be used as the default color with
CD   all drawing primitives.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 12-AUG-1988
CH   History:
CH      12-AUG-88  ATV  Corrected erroneous error messages.
CH      09-JUL-88  ATV  Changed location of SEGSOP to SEGINF.
CH      29-JUN-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB        SEGINF-R, LINATT-W
C
C    Calls:
CC        ERROR
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      INTEGER ICOLOR 
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
      IF (SEGSOP)
     + CALL ERROR('THE DEFAULT COLOR NDX CANNOT BE CHANGED') 
      IF (ICOLOR .LT. 0 .OR. ICOLOR .GT. 32767) THEN
         CALL ERROR('COLOR DVALUE IS NOT IN RANGE')
      ENDIF
      DEFCOL = MOD(ICOLOR,40)
      RETURN
      END

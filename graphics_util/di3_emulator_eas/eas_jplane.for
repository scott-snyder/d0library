      SUBROUTINE JPLANE(XPLAN, YPLAN, ZPLAN)
C
C    Purpose:
CD   This routine set the current plane vector for text output.
CD   The parameters are the coefficients to the unit vector and define a 
CD   plane that each character will lie in.
C
C    A. Virgo, R. Heckelsberg
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 09-Jul-1988
CH   History:
CH      09-JUL-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      SEGINF-R, TEXATT-W
C
C    Calls:
CC      ERROR
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      REAL XPLAN, YPLAN, ZPLAN
C
C    Then local declarations of variables (non-common variables).
C
 
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:TEXATT.INC/LIST'
C
C    Then executable code follows
C
      IF (SEGOPN) THEN
         IF (XPLAN .EQ. 0.0 .AND.
     +       YPLAN .EQ. 0.0 .AND.
     +       ZPLAN .EQ. 0.0) THEN
            CALL ERROR('NO VALID PLANE VECTOR DEFINED')
         ELSE
            CXPLAN = XPLAN
            CYPLAN = YPLAN
            CZPLAN = ZPLAN
         ENDIF
      ELSE
         CALL ERROR('NO SEGMENT IS OPEN')
      ENDIF
      RETURN
      END

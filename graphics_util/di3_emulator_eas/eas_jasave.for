      SUBROUTINE JASAVE(IARRAY)
C
C    Purpose:
CD   This module saves the primitive attributes in an array. The 
CD   parameter passed is the designated save area.  It is assumed that
CD   this array is dimensioned at least to 11.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 05-Oct-1988
CH   History:
CH      05-OCT-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      SEGINF-R, LINATT-R
C
C    Calls:
CC      ERROR
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      INTEGER IARRAY(*)
C
C    Then local declarations of variables (non-common variables).
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:LINATT.INC/LIST'
C
C    Then executable code follows
C
      IF (SEGOPN) THEN
C
C   Save drawing primitive attributes.
C
         IARRAY(1)  = CURCOL
         IARRAY(2)  = CINTEN
         IARRAY(3)  = CLSTYL
         IARRAY(4)  = 0
         IARRAY(5)  = 0
         IARRAY(6)  = 0
         IARRAY(7)  = 0
         IARRAY(8)  = 0
         IARRAY(9)  = 0
         IARRAY(10) = CMARKR
C
C   Save current pick id.
C
         IARRAY(11) = CPIKID
      ELSE
         CALL ERROR('JASAVE: NO SEGMENT IS OPEN')
      ENDIF
      RETURN
      END

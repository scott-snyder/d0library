      SUBROUTINE JALOAD(IARRAY)
C
C    Purpose:
CD   This module loads the primitive attributes from an array. The 
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
CH      10-MAR-89  SA   UVIEW(5) and UVIEW(6) was set here.
CH      05-OCT-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      SEGINF-W, LINATT-W
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
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:LINATT.INC/LIST'
C
C    Then executable code follows
C
      IF (SEGOPN) THEN
C
C   Load drawing primitive attributes.
C
         CURCOL = IARRAY(1)
         CINTEN = IARRAY(2)
         CLSTYL = IARRAY(3)
C         IARRAY(4)
C         IARRAY(5)
C         IARRAY(6)
C         IARRAY(7)
C         IARRAY(8)
C         IARRAY(9)
         CMARKR = IARRAY(10)
         UVIEW(5) = MINTEN / 32767.0
         UVIEW(6) = CINTEN / 32767.0
C
C   Load the saved pick id.
C
         CPIKID = IARRAY(11)
      ELSE
         CALL ERROR('JALOAD: NO SEGMENT IS OPEN')
      ENDIF
      RETURN
      END

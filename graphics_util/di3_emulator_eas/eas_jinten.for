        SUBROUTINE JINTEN(INTEN)
C
C    Purpose:
CD   This module sets the current intensity for all drawing primitives.
CD   The parameter passed INTEN is an integer between 0 and 32767.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 15-Aug-1988
CH   History:
CH      10-MAR-89  ATV  UVIEW(5) and UVIEW(6) was set here.
CH      15-AUG-88  ATV  Initial entry.
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
      INTEGER INTEN
C
C    Then local declarations of variables (non-common variables).
C
 
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:LINATT.INC/LIST'
C
C    Then executable code follows
C
      IF (INTEN .LT. 0 .OR. INTEN .GT. 32767) THEN
         CALL ERROR('INTENSITY CVALUE IS NOT IN RANGE')
      ENDIF
      CINTEN = INTEN
      UVIEW(5) = MINTEN / 32767.0
      UVIEW(6) = CINTEN / 32767.0
      RETURN
      END

      SUBROUTINE JWCLIP(FLAG)
C
C    Purpose:
CD   The purpose of this module is to change the window clipping status.
CD   The parameter passed is a logical value (True or False) that either
CD   enables Window clipping (True) or disables window clipping (False).
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 04-Oct-1988
CH   History:
CH      04-OCT-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      'GRFPAR-W', 'SEGINF-R'
C
C    Calls:
CC      ERROR.
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      LOGICAL FLAG
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
         CALL ERROR('JWCLIP: A SEGMENT IS OPEN')
      ENDIF
      WCLIP = FLAG
      RETURN
      END

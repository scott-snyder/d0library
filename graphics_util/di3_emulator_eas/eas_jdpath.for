      SUBROUTINE JDPATH(PATH)
C
C    Purpose:
CD   This module sets the default character path dependent upon the PATH
CD   argument passed. The following values are valid:
CD     PATH = 1 --> Character path is to the right.
CD     PATH = 2 --> Character path is up.
CD     PATH = 3 --> Character path is to the left.
CD     PATH = 4 --> Character path is down.
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
CC        ERROR
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      INTEGER PATH 
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
      IF (.NOT. SEGSOP) THEN
         IF (PATH .LT. 1 .OR. PATH .GT. 4) THEN
            CALL ERROR('DEFAULT PATH VALUE NOT IN VALID RANGE')
         ELSE
            DPATH = PATH
         ENDIF
      ELSE
         CALL ERROR('DEFAULT CHAR PATH MAY NOT BE CHANGED')
      ENDIF
      RETURN
      END

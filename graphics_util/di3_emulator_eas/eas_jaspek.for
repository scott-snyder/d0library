      SUBROUTINE JASPEK(NDEV,RAT)
C
C    Purpose:
CD   This module returns the aspect ratio (ymax - ymin) / (xmax - xmin)
CD   of a device. The current device driver returns an aspect ratio of
CD   1.0 since the default display is square.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 26-Oct-1988
CH   History:
CH      26-OCT-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      GENRL-R, GRFPAR-R
C
C    Calls:
CC      ERROR
C

C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      INTEGER NDEV
      REAL RAT 
C
C    Then local declarations of variables (non-common variables).
C
 
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:GENRL.INC/LIST'
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
C
C    Then executable code follows
C
      IF (NDEV .EQ. DSPDEV .AND. DINIT) THEN
         RAT = 1.0
      ELSE
         CALL ERROR('JASPEK: DEVICE IS NOT INITIALIZED')
      ENDIF
      RETURN
      END

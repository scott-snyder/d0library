        SUBROUTINE JDEVOF(DEV)
C
C    Purpose:
CD   The purpose of this routine is a dummy that just sets a flag to
CD   indicate that the device is turned off. The parameter DEV is
CD   a dummy variable used for conformity. 
CD   
C
C    A. Virgo, R. Heckelsberg
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 18-May-1988
CH   History:
CH      18-May-88  ATV   Initial entry
C
      IMPLICIT NONE
C
C    Common blocks:
CB      GRFPAR-R, DEVSTS-W, SEGINF-R
C
C    Calls:
CC        NONE.
C
C    parameters passed to the subroutine.
      INTEGER DEV
C
C    Then local declarations of variables (non-common variables).
C
 
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:GRFPAR.INC/LIST' 
      INCLUDE 'D0$INC:DEVSTS.INC/LIST' 
      INCLUDE 'D0$INC:SEGINF.INC/LIST' 
C
C    Then executable code follows
C
      IF (DINIT) THEN
         IF (DEVON) THEN
            IF (SEGOPN) THEN
               CALL ERROR('A SEGMENT IS OPEN')
            ELSE
               DEVON = .FALSE.
            ENDIF
         ELSE
            CALL ERROR('DSPDEV IS NOT SELECTED')
         ENDIF
      ELSE
         CALL ERROR('DSPDEV IS NOT INITIALIZED')
      ENDIF
      RETURN
      END

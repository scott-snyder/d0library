      SUBROUTINE JDLSTY(LSTYLE)
C
C    Purpose:
CD   This module sets the default line style that is used to pattern
CD   the subsequent vector-list with.  The parameter is a number between
CD   0 and 32767, but the emulator only supports 10 different line-styles.
CD   This routine may only be called if no segment has been opened since
CD   the call to JBEGIN.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 22-Aug-1988
CH   History:
CH      22-AUG-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      SEGINF-R, LINATT-W
C
C    Calls:
CC      ERROR
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      INTEGER LSTYLE 
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
      IF (SEGSOP) THEN
         CALL ERROR('JDLSTY: DEFAULT LINE STYLE MAY NOT BE CHANGED')
      ELSE
         IF (LSTYLE .LT. 0 .OR. LSTYLE .GT. 32767) THEN
            CALL ERROR('JDLSTY: DVALUE OUT OF RANGE')
         ELSE
            DLSTYL = LSTYLE
         ENDIF
      ENDIF
      RETURN
      END

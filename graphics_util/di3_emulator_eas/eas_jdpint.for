      SUBROUTINE JDPINT(DVALUE)
C
C    Purpose:
CD   This module sets the default polygon interior style. The parameter
CD   passed (DVALUE) is an integer with a valid value 0..32767.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 21-Oct-1988
CH   History:
CH      21-OCT-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      SEGINF-R, PLGATT-W
C
C    Calls:
CC      ERROR.
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
C
C    Then local declarations of variables (non-common variables).
C
      INTEGER DVALUE
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:PLGATT.INC/LIST'
C
C    Then executable code follows
C
      IF (SEGSOP) THEN
         CALL ERROR('JDPINT: DEF. POLY. INT. STY MAY NOT BE CHANGED')
      ENDIF
      IF (DVALUE .LT. 0 .OR . DVALUE .GT. 32767) THEN
         CALL ERROR('JDPINT: DVALUE OUT OF RANGE (0..32767)')
      ENDIF
      DPINTR = DVALUE
      RETURN
      END
